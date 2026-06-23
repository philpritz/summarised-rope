#lang racket
;; "One body" taken to its conclusion: if the loop body is identical for every arity, it's all
;; apply/call-with-values -- so there's no case-lambda and no macro, just a rest-arg lambda.
;; (This is essentially the ORIGINAL variadic `fixed`, with call-with-values instead of compose-list.)
(define (fixed improve [same? equal?] [key list])
  (lambda args
    (let loop ([args args] [kp (apply key args)])
      (call-with-values
       (lambda () (apply improve args))
       (lambda args*
         (define kp* (apply key args*))
         (if (same? kp kp*) (apply values args*) (loop args* kp*)))))))

(require rackunit)
(check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)
(check-equal? (call-with-values (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list) '(3 3))
(check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24)
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c) (values b c (min a b c)))) 9 5 7)) list) '(5 5 5))
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c d e) (values b c d e (min a b c d e)))) 5 4 3 2 1)) list)
              '(1 1 1 1 1))
(displayln "fixed-unified (one body, rest-arg lambda): all checks passed")
