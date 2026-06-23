#lang racket/base
;; Does the 2-value fast path matter?  Same 2-value fixpoint workload through:
;;   unified  -- the rest-arg lambda (all apply/list)
;;   caselam  -- a case-lambda with a list-free arity-2 clause
(require racket/list)

(define (fixed-uni improve [same? equal?] [key list])
  (lambda args
    (let loop ([args args] [kp (apply key args)])
      (call-with-values (lambda () (apply improve args))
        (lambda args*
          (define kp* (apply key args*))
          (if (same? kp kp*) (apply values args*) (loop args* kp*)))))))

(define (fixed-cl improve [same? equal?] [key list])
  (case-lambda
    [(a b) (let loop ([a a] [b b] [kp (key a b)])
             (define-values (a* b*) (improve a b)) (define kp* (key a* b*))
             (if (same? kp kp*) (values a* b*) (loop a* b* kp*)))]
    [xs (let loop ([xs xs] [kp (apply key xs)])
          (define ys (call-with-values (lambda () (apply improve xs)) list))
          (define kp* (apply key ys))
          (if (same? kp kp*) (apply values ys) (loop ys kp*)))]))

;; 2-value step: a counts down to 0 (~5 iters), b accumulates; key = first, same? = =
(define improve (lambda (a b) (values (if (> a 0) (sub1 a) 0) (add1 b))))
(define key     (lambda (a b) a))
(define uni (fixed-uni improve = key))
(define cl  (fixed-cl  improve = key))

(define N 3000000)
(define (bench label seeker)
  (collect-garbage) (collect-garbage) (collect-garbage)
  (printf "~a " label)
  (define acc (time (for/fold ([s 0]) ([i (in-range N)])
                      (call-with-values (lambda () (seeker 5 0)) (lambda (a b) (+ s a b))))))
  (void acc))

(bench "unified   " uni)
(bench "caselambda" cl)
