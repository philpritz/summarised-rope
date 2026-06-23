#lang racket
;; THROWAWAY: isolate dispatch cost. 5 summaries with DISTINCT combine code (distinct code
;; pointers) but identical work (all compute x+y on ints). Vary only the call target.
;; Delete after.
(require "../rope-core.rkt")    ; make-summary

;; 5 distinct lambdas -> 5 distinct code pointers, all = x+y, all accept integers
(define cv (vector
  (make-summary string-length (lambda (x y) (+ x y)))
  (make-summary string-length (lambda (x y) (+ 0 x y)))
  (make-summary string-length (lambda (x y) (+ x y 0)))
  (make-summary string-length (lambda (x y) (+ (+ x 0) y)))
  (make-summary string-length (lambda (x y) (+ y x)))))
(define c0 (vector-ref cv 0))
(define a 17) (define b 25)

(define (run label iters body)         ; body : i -> _
  (body 0) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (body i))) '()))
  (printf "  ~a ~a ns/op\n" (~a label #:min-width 46) (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 8 #:align 'right)))
(define N 5000000)

(printf "same work (x+y on ints), varying ONLY the call site / target:\n")
(run "direct named  (c0 a b)"                       N (lambda (i) (c0 a b)))
(run "indirect, 1 target  ((vref cv 0) a b)"        N (lambda (i) ((vector-ref cv (bitwise-and i 0)) a b)))
(run "indirect, 2 targets ((vref cv {0,1}) a b)"    N (lambda (i) ((vector-ref cv (bitwise-and i 1)) a b)))
(run "indirect, 4 targets ((vref cv {0..3}) a b)"   N (lambda (i) ((vector-ref cv (bitwise-and i 3)) a b)))
(run "indirect, 5 targets ((vref cv {0..4}) a b)"   N (lambda (i) ((vector-ref cv (remainder i 5)) a b)))

;; control: the bitwise-and / remainder loop overhead alone (no call)
(printf "\nloop-overhead controls (no summary call):\n")
(run "bitwise-and i 0 + vector-ref"                 N (lambda (i) (vector-ref cv (bitwise-and i 0))))
(run "remainder i 5 + vector-ref"                   N (lambda (i) (vector-ref cv (remainder i 5))))

;; the actual bundle-combine shape: build a 5-vector either way
(define c1 (vector-ref cv 1)) (define c2 (vector-ref cv 2))
(define c3 (vector-ref cv 3)) (define c4 (vector-ref cv 4))
(printf "\nbundle-combine shape (5 distinct components, same work, builds a 5-vector):\n")
(run "megamorphic: build-vector, 1 site x 5 targets"  N
     (lambda (i) (build-vector 5 (lambda (j) ((vector-ref cv j) a b)))))
(run "monomorphic: unrolled, 5 sites x 1 target each" N
     (lambda (i) (vector (c0 a b) (c1 a b) (c2 a b) (c3 a b) (c4 a b))))
