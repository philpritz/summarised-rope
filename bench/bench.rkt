#lang racket

;; A small timing harness: for a given input size, warm up, run K trials, and
;; report a representative per-call time. Reporting only -- no assertions, no
;; complexity fitting; just the numbers. Lives off the CLAUDE.md orientation
;; import list (like scribl/) -- a tool, not core machinery.
;;
;;   measure : thunk -> stats        time an already-prepared thunk
;;   bench   : op gen sizes -> rows  build each input (untimed), time op over a size range
;;
;; min/median/mean are all reported, but min and median are the ones to trust:
;; noise only ever slows a run, so the minimum is the cleanest estimate of
;; intrinsic cost and the median the robust middle; the mean is dragged up by
;; outliers (GC pauses, OS preemption).

(provide measure bench (struct-out stats))

(struct stats (min median mean count) #:transparent)

(define (median xs)
  (define s (sort xs <))
  (define n (length s))
  (define mid (quotient n 2))
  (if (odd? n)
      (list-ref s mid)
      (/ (+ (list-ref s (sub1 mid)) (list-ref s mid)) 2.0)))

;; measure : (-> any) -> stats   -- time an already-prepared thunk.
;;   warm up `warmup` calls (discarded), then run `trials` timed trials; each
;;   trial times `reps` calls and divides, so sub-ms ops don't read as 0.
;;   reports PER-CALL milliseconds.
(define (measure thunk
                 #:warmup [warmup 3]
                 #:trials [trials 11]
                 #:reps   [reps 1])
  (for ([_ (in-range warmup)]) (thunk))                 ; warm: discard
  (define per-call
    (for/list ([_ (in-range trials)])
      (define t0 (current-inexact-milliseconds))
      (for ([_ (in-range reps)]) (thunk))
      (/ (- (current-inexact-milliseconds) t0) reps)))
  (stats (apply min per-call)
         (median per-call)
         (/ (apply + per-call) trials)
         trials))

;; bench : (input -> any) (size -> input) (listof size) -> (listof (cons size stats))
;;   `gen` builds each input OUTSIDE the timed region; only `op` is timed.
;;   Inputs are pure ropes, so the same input is reused across reps/trials.
(define (bench op gen sizes
               #:warmup [warmup 3] #:trials [trials 11] #:reps [reps 1])
  (for/list ([n (in-list sizes)])
    (define input (gen n))                              ; not timed
    (cons n (measure (lambda () (op input))
                     #:warmup warmup #:trials trials #:reps reps))))

;; ============================================================================
;; A demo run. Four suites, all on the same size axis:
;;   build   -- construct the whole tree            (expect LINEAR)
;;   split   -- one midpoint cut on a prebuilt rope  (expect ~flat / log)
;;   nav,char -- install a midpoint cursor (char summary)   (expect ~flat / log)
;;   nav,sexp -- install a midpoint cursor (sexp summary)   (expect ~flat / log)
;; Inputs are built in `gen` (untimed); only the op is timed.
(module+ main
  (require "../sexp-edit.rkt"            ; re-exports rope-core, zipper-core, summaries
           (submod "../sexp-edit.rkt" gen) ; gen:shape gen:populate tree->text
           rackcheck)                     ; gen:resize gen:bind sample
  (define sum (make-summary string-length +))
  (define (fmt x) (~r x #:precision '(= 4) #:min-width 10))
  (define (table label rows)
    (printf "\n~a\n" label)
    (printf "~a  ~a  ~a  ~a\n"
            (~a "n" #:min-width 8) (~a "min(ms)" #:min-width 10)
            (~a "median" #:min-width 10) (~a "mean" #:min-width 10))
    (for ([row rows])
      (match-define (cons n st) row)
      (printf "~a  ~a  ~a  ~a\n"
              (~a n #:min-width 8) (fmt (stats-min st))
              (fmt (stats-median st)) (fmt (stats-mean st)))))

  (define sizes '(1000 2000 4000 8000 16000 32000))

  ;; build: times constructing the WHOLE tree -- O(N), linear.
  (table "build: ((make-rope sum) (make-string n #\\x))      -- expect linear"
         (bench (lambda (s) ((make-rope sum) s))
                (lambda (n) (make-string n #\x))
                sizes #:reps 5))

  ;; split: rope prebuilt in gen (UNTIMED); time only the midpoint cut -- O(log N).
  (define (halve L R) (cond [(< L R) 1] [(> L R) -1] [else 0]))
  (table "split: ((multisect (vector halve)) rope)           -- expect ~flat (log)"
         (bench (lambda (r) ((multisect (vector halve)) r))
                (lambda (n) ((make-rope sum) (make-string n #\x)))
                sizes #:reps 50))

  ;; nav (char): install a gap at the char midpoint on the root zipper. The
  ;; install triggers `navigate` (ascend . descend . carve) -- the O(log N) op.
  ;; (The char guide lives in zipper-core's test module, so reproduce it here.)
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define (gap n) (vector (at n) (at n)))
  (define (install p) ((zipper-guide (cdr p)) (car p)))   ; (zipper . guide-vec) -> navigate
  (table "nav,char: install midpoint gap, n-char rope        -- expect ~flat (log)"
         (bench install
                (lambda (n) (let ([g (gap (quotient n 2))])
                              (cons (start sum ((make-rope sum) (make-string n #\x)) g) g)))
                sizes #:reps 50))

  ;; nav (sexp): the genuine sexp cursor over REAL generated trees. We DON'T bound
  ;; on document size here -- we bound on (max-kids . depth) and let the generator
  ;; decide the size. Each config draws K trees at a fixed generator size
  ;; (gen:resize, so the shape is governed by depth/max-kids rather than
  ;; rackcheck's creeping `size`), renders each to source, builds a rope, and times
  ;; navigating to a gap at the text midpoint -- averaged over the K trees. The
  ;; target spine is read off the cut by the real `sand-spines`.
  (random-seed 42)                                    ; reproducible trees
  (define K 50)
  (define gen-size 40)
  (define (front-at text i)                           ; the front spine at char cut i
    (let-values ([(f _b) (sand-spines (sexp-smr (substring text 0 i))
                                      (sexp-smr (substring text i)))])
      f))
  (define (prep-tree t)                               ; (chars . (zipper . guide-vec))
    (define text (tree->text t))
    (define tgt  (front-at text (quotient (string-length text) 2)))
    (define gs   (sexp-guides tgt))
    (define z    (start sexp-smr ((make-rope sexp-smr) text) gs))
    (cons (string-length text) (cons z gs)))
  (printf "\nnav,sexp: generated trees, navigate to midpoint gap (avg over ~a trees/config)\n" K)
  (printf "          axis is max-kids x depth, NOT size  -- expect ~~flat/log in doc size\n")
  (printf "~a  ~a  ~a  ~a\n"
          (~a "mk x d" #:min-width 8) (~a "avg chars" #:min-width 10)
          (~a "min(ms)" #:min-width 10) (~a "median" #:min-width 10))
  (for ([cfg (list (cons 2 4) (cons 3 4) (cons 3 5) (cons 4 5) (cons 4 6) (cons 5 6))])
    (define g (gen:resize (gen:bind (gen:shape (car cfg) (cdr cfg)) gen:populate) gen-size))
    (define batch (map prep-tree (sample g K)))
    (define avg-chars (quotient (apply + (map car batch)) K))
    (define st (measure (lambda () (for ([p batch]) (install (cdr p)))) #:reps 1))
    (printf "~a  ~a  ~a  ~a\n"
            (~a (format "~a x ~a" (car cfg) (cdr cfg)) #:min-width 8)
            (~a avg-chars #:min-width 10)
            (fmt (/ (stats-min st) K))
            (fmt (/ (stats-median st) K))))

  ;; nav,sexp on LARGE docs (tens of thousands of chars). Same generator, bigger
  ;; (max-kids . depth). Collapsed draws are common at the tail, so oversample and
  ;; keep the biggest k by char count -- the point is to time navigation on
  ;; genuinely large documents (only the kept trees are built into ropes).
  (define (big-batch g pool k)
    (define sized (sort (for/list ([t (sample g pool)])
                          (cons (string-length (tree->text t)) t))
                        > #:key car))
    (map (lambda (ct) (prep-tree (cdr ct))) (take sized k)))
  (printf "\nLARGE docs (biggest 12 of 20 draws/config): nav to midpoint gap, then to-root\n")
  (printf "~a  ~a  ~a  ~a\n"
          (~a "mk x d" #:min-width 8) (~a "avg chars" #:min-width 12)
          (~a "nav med(ms)" #:min-width 12) (~a "to-root med" #:min-width 12))
  (for ([cfg (list (cons 6 7) (cons 6 8) (cons 8 7))])
    (define g (gen:resize (gen:bind (gen:shape (car cfg) (cdr cfg)) gen:populate) gen-size))
    (define batch (big-batch g 20 12))
    (define avg-chars (quotient (apply + (map car batch)) 12))
    (define nav-st  (measure (lambda () (for ([p batch]) (install (cdr p)))) #:reps 1))
    (define homed   (for/list ([p batch]) (install (cdr p))))           ; navigated, untimed
    (define home-st (measure (lambda () (for ([z homed]) (to-root z))) #:reps 1))
    (printf "~a  ~a  ~a  ~a\n"
            (~a (format "~a x ~a" (car cfg) (cdr cfg)) #:min-width 8)
            (~a avg-chars #:min-width 12)
            (fmt (/ (stats-median nav-st) 12))
            (fmt (/ (stats-median home-st) 12))))

  ;; ---- deep DAG: a rope where both children of every branch are the SAME object.
  ;; r_0 = base, r_{k+1} = (branch r_k r_k) -- so r_d is 2^d copies of base in O(d)
  ;; nodes (each shared child's summary is computed once). Lets us probe navigation
  ;; against documents far too large to allocate: d=5000 is ~10^1506 forms. We COUNT
  ;; guide calls instead of timing -- at this depth the slot index is a d-bit bignum,
  ;; so timing would measure bignum arithmetic (O(d) per compare), not the rope; the
  ;; call count stays a fixnum and shows descent is O(d) = O(log N).
  (define dag-base ((make-rope sexp-smr) (string-append* (make-list 12 "(a b c)"))))  ; 84 chars, 12 forms
  (define (doubled d) (for/fold ([h dag-base]) ([_ (in-range d)]) ((make-rope sexp-smr) h h)))
  (define (counting gs counter)                       ; wrap each guide to tally its calls
    (vector-map (lambda (g) (lambda (L R) (set-box! counter (add1 (unbox counter))) (g L R))) gs))
  (printf "\ndeep DAG (branch(h,h) sharing): navigate to the middle form, COUNT guide calls\n")
  (printf "          count is fixnum O(d)=O(log N); the doc has 12*2^d forms, never allocated\n")
  (printf "~a  ~a  ~a  ~a\n"
          (~a "d" #:min-width 6) (~a "forms digits" #:min-width 14)
          (~a "guide calls" #:min-width 12) (~a "build ms" #:min-width 10))
  (for ([d '(50 100 500 1000 5000)])
    (define t0 (current-inexact-milliseconds))
    (define r  (doubled d))
    (define t1 (current-inexact-milliseconds))
    (define forms  (sexp-forms (sexp-smr r)))         ; 12*2^d, read O(1) off the cache
    (define target (list (quotient forms 2)))         ; the middle top-level form
    (define counter (box 0))
    (define gs (counting (sexp-guides target) counter))
    ((zipper-guide gs) (start sexp-smr r gs))         ; navigate, tallying guide calls
    (printf "~a  ~a  ~a  ~a\n"
            (~a d #:min-width 6)
            (~a (string-length (number->string forms)) #:min-width 14)
            (~a (unbox counter) #:min-width 12)
            (~a (round (- t1 t0)) #:min-width 10))))
