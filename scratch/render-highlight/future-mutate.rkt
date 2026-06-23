#lang racket

;; Hypothesis: futures don't park on the *work*, they park on *allocation* (the persistent
;; combine mints a struct/cons cell every call).  Test it directly, away from the zipper:
;; a MUTABLE (boxed) summary whose combine MUTATES IN PLACE -- zero allocation in the hot
;; loop -- vs the same monoid done allocationally (a fresh struct per combine).  If the
;; in-place one scales on futures and the allocating one doesn't, allocation was the blocker.
;;
;; Run:  racket scratch/render-highlight/future-mutate.rkt

(require racket/future
         future-visualizer/trace)

;; a boxed summary: 3 fixnum fields (stand-in for linecol / a few bundle slots)
(struct msum (x y z) #:mutable)

;; in-place combine: target := a (+) b.  Only field writes + fixnum arithmetic -> NO allocation.
(define (combine! t a b)
  (set-msum-x! t (+ (msum-x a) (msum-x b)))
  (set-msum-y! t (+ (msum-y a) (msum-y b)))
  (set-msum-z! t (if (> (msum-z a) (msum-z b)) (msum-z a) (msum-z b))))
(define (work-inplace t a b reps)
  (for ([i (in-range reps)]) (combine! t a b)) t)

;; allocating combine: a fresh msum every call (like the persistent rope's combine)
(define (combine-alloc a b)
  (msum (+ (msum-x a) (msum-x b)) (+ (msum-y a) (msum-y b))
        (if (> (msum-z a) (msum-z b)) (msum-z a) (msum-z b))))
(define (work-alloc a b reps)
  (let loop ([acc a] [i reps]) (if (zero? i) acc (loop (combine-alloc acc b) (sub1 i)))))

(define (ms) (current-inexact-milliseconds))
(define (timeit thunk) (collect-garbage) (define t0 (ms)) (thunk) (- (ms) t0))

(define (blocks-of thunk)        ; count future 'block (park) events during thunk
  (start-future-tracing!) (thunk) (stop-future-tracing!)
  (count (lambda (e) (and (future-event? e) (eq? (future-event-what e) 'block)))
         (map indexed-future-event-fevent (timeline-events))))

(module+ main
  (define cores (processor-count))
  (define a (msum 1 2 3)) (define b (msum 1 1 1))
  (define reps 4000000)
  (printf "cores: ~a   futures-enabled?: ~a   ~a reps/task, ~a tasks\n\n"
          cores (futures-enabled?) reps cores)

  ;; ---------- in-place (no allocation in the combine) ----------
  (define (seq-ip) (for ([i (in-range cores)]) (work-inplace (msum 0 0 0) a b reps)))
  (define (par-ip)
    (for-each touch
      (for/list ([i (in-range cores)])
        (define t (msum 0 0 0))                 ; allocated up front, on the main thread
        (future (lambda () (work-inplace t a b reps))))))
  (seq-ip) (par-ip)                             ; warm
  (define s1 (timeit seq-ip)) (define p1 (timeit par-ip))
  (printf "IN-PLACE  (mutate, 0 alloc):   seq ~a ms   par ~a ms   speedup ~ax   parks ~a\n"
          (~r s1 #:precision 0) (~r p1 #:precision 0)
          (~r (/ s1 (max 1.0 p1)) #:precision 1) (blocks-of par-ip))

  ;; ---------- allocating (fresh struct per combine) ----------
  (define (seq-al) (for ([i (in-range cores)]) (work-alloc a b reps)))
  (define (par-al)
    (for-each touch (for/list ([i (in-range cores)]) (future (lambda () (work-alloc a b reps))))))
  (seq-al) (par-al)
  (define s2 (timeit seq-al)) (define p2 (timeit par-al))
  (printf "ALLOCATING (fresh per combine): seq ~a ms   par ~a ms   speedup ~ax   parks ~a\n"
          (~r s2 #:precision 0) (~r p2 #:precision 0)
          (~r (/ s2 (max 1.0 p2)) #:precision 1) (blocks-of par-al)))
