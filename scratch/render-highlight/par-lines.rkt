#lang racket

;; The parallelism we converged on: N INDEPENDENT, crumb-free, allocation-free per-line
;; descents, fanned across futures.  Each descent walks the FULL rope to one line's start,
;; folding everything before it into a REUSED mutable box (no buf-combine, no rope rebuild,
;; decisions inlined from the box fields).  Zero allocation in the hot loop -> futures don't
;; park on GC.  Sequential vs parallel, with an exact per-line check.
;;
;; Result (100k lines x 40 cols, 20 cores): sequential 7549 ms -> parallel 1961 ms = 3.8x,
;; PARKS 0.  The allocation-free descent dodges the GC wall entirely -- the allocating
;; versions parked and capped at ~1-2x; this one makes no garbage, so futures actually run.
;; 3.8x (not the 7.5x in-place microbench) because the descent is MEMORY-LATENCY bound:
;; it pointer-chases the rope tree + does bundle reads, jumping around memory, so 20 cores
;; contend on memory, not compute.  char+linecol only (strsexp would need the array-stack box).
;; Batch-only: an interactive ~50-line viewport uses A's O(1)/line scan -- no cores needed.
;;
;; Run:  racket scratch/render-highlight/par-lines.rkt

(require racket/future future-visualizer/trace
         "../../rope-core.rkt"               ; make-rope
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         (submod "../../rope-core.rkt" internal))   ; leaf? branch-left branch-right leaf-text

(define buf (bundle char-smr linecol-smr))

;; the before-accumulator: char count + linecol (head/lines/cols), all mutable fixnums
(struct bx (ch hd ln cl) #:mutable)
(define (bx-reset! b) (set-bx-ch! b 0) (set-bx-hd! b 0) (set-bx-ln! b 0) (set-bx-cl! b 0))

;; fold a cached child summary v (a bundle-val) into b: b := b (+) v, in place, NO alloc
(define (fold! b v)
  (match-define (linecol vh vl vc) (linecol-smr v))
  (when (zero? (bx-ln b)) (set-bx-hd! b (+ (bx-hd b) vh)))
  (set-bx-cl! b (if (zero? vl) (+ (bx-cl b) vc) vc))
  (set-bx-ln! b (+ (bx-ln b) vl))
  (set-bx-ch! b (+ (bx-ch b) (char-smr v))))

;; fold one char into b (used in the boundary leaf), NO alloc
(define (fold-char! b c)
  (set-bx-ch! b (add1 (bx-ch b)))
  (cond [(char=? c #\newline) (set-bx-ln! b (add1 (bx-ln b))) (set-bx-cl! b 0)]
        [else (when (zero? (bx-ln b)) (set-bx-hd! b (add1 (bx-hd b)))) (set-bx-cl! b (add1 (bx-cl b)))]))

;; descend to the start of line `target`, folding everything before it into b (b pre-reset)
(define (descend-before! b target rope)
  (let loop ([t rope])
    (cond
      [(leaf? t)
       (define s (leaf-text t)) (define len (string-length s))
       (let scan ([i 0]) (when (and (< i len) (not (= (bx-ln b) target)))
                           (fold-char! b (string-ref s i)) (scan (add1 i))))]
      [else
       (define l (branch-left t)) (define r (branch-right t))
       (define vs (buf l))                                  ; l's cached summary (O(1))
       (match-define (linecol _ lln lcl) (linecol-smr vs))
       (define Lln (+ (bx-ln b) lln))
       (define Lcl (if (zero? lln) (+ (bx-cl b) lcl) lcl))
       (cond
         [(< Lln target)                   (fold! b vs) (loop r)]   ; line past l -> fold l, go right
         [(and (= Lln target) (zero? Lcl)) (fold! b vs)]            ; line exactly at l's end -> done
         [else                             (loop l)])])))           ; line inside l -> go left

;; fill out[i] = before-char-count at line i, for i in [lo,hi), reusing one box (no alloc)
(define (run-range rope out lo hi)
  (define b (bx 0 0 0 0))
  (for ([i (in-range lo hi)]) (bx-reset! b) (descend-before! b i rope) (vector-set! out i (bx-ch b))))

(define (par-fill rope out N cores)
  (define sz (quotient (+ N cores -1) cores))
  (for-each touch
    (for/list ([c (in-range cores)])
      (define lo (* c sz)) (define hi (min N (* (add1 c) sz)))
      (future (lambda () (run-range rope out lo hi))))))

(define (blocks thunk)
  (start-future-tracing!) (thunk) (stop-future-tracing!)
  (count (lambda (e) (and (future-event? e) (eq? (future-event-what e) 'block)))
         (map indexed-future-event-fevent (timeline-events))))

(module+ main
  (define cores (processor-count))
  (define COLS 40) (define N 100000)
  (define text (apply string-append (make-list N (string-append (make-string COLS #\x) "\n"))))
  (define rope ((make-rope buf) text))
  (printf "cores ~a   doc ~a lines x ~a cols   N=~a per-line descents\n\n" cores N COLS N)

  ;; ---------- correctness: before-char at line i is exactly i*(COLS+1) ----------
  (define out (make-vector N 0))
  (run-range rope out 0 N)
  (for ([i (in-range N)]) (unless (= (vector-ref out i) (* i (add1 COLS))) (error 'check "line ~a: ~a" i (vector-ref out i))))
  (define outp (make-vector N -1))
  (par-fill rope outp N cores)
  (for ([i (in-range N)]) (unless (= (vector-ref out i) (vector-ref outp i)) (error 'par "mismatch line ~a" i)))
  (printf "correctness: every line's before-count exact, and seq == parallel  ok\n\n")

  (define (timeit thunk) (collect-garbage) (define t0 (current-inexact-milliseconds)) (thunk) (- (current-inexact-milliseconds) t0))
  (define (seq) (run-range rope out 0 N))
  (define (par) (par-fill rope outp N cores))
  (seq) (par)                                       ; warm
  (define s (timeit seq)) (define p (timeit par))
  (printf "sequential (~a descents):   ~a ms\n" N (~r s #:precision 0))
  (printf "parallel   (~a cores):      ~a ms   speedup ~ax   parks ~a\n"
          cores (~r p #:precision 0) (~r (/ s (max 1.0 p)) #:precision 1) (blocks par)))
