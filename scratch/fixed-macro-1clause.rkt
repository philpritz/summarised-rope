#lang racket
;; One fixed-case clause: an optional #:rest decides lean (define-values, exact) vs
;; general (call-with-values + apply, tail).  syntax-parse gives the optional keyword.
(require (for-syntax syntax/parse))

(define (fixed improve [same? equal?] [key list])
  (define-syntax (fixed-case stx)
    (syntax-parse stx
      [(_ v:id ... (~optional (~seq #:rest xs:id)))
       (if (attribute xs)
           ;; general: leading fixed vars + a rest list -- must apply/call-with-values for the tail
           (with-syntax ([(v* ...) (generate-temporaries #'(v ...))]
                         [(xs*)    (generate-temporaries #'(xs))])
             #'(let loop ([v v] ... [xs xs] [kp (apply key v ... xs)])
                 (call-with-values
                  (lambda () (apply improve v ... xs))
                  (lambda (v* ... . xs*)
                    (define kp* (apply key v* ... xs*))
                    (if (same? kp kp*) (apply values v* ... xs*) (loop v* ... xs* kp*))))))
           ;; lean: exact arity -- direct calls, no apply
           (with-syntax ([(v* ...) (generate-temporaries #'(v ...))])
             #'(let loop ([v v] ... [kp (key v ...)])
                 (define-values (v* ...) (improve v ...))
                 (define kp* (key v* ...))
                 (if (same? kp kp*) (values v* ...) (loop v* ... kp*)))))]))
  (case-lambda
    [(a)          (fixed-case a)]
    [(a b)        (fixed-case a b)]
    [(a b c . xs) (fixed-case a b c #:rest xs)]))

;; --- arities 1, 2 (lean branch), 3 (general, empty rest), 5 (general, non-empty rest) ---
(require rackunit)
(check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list) '(3 3))
(check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24)
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c) (values b c (min a b c)))) 9 5 7)) list) '(5 5 5))
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c d e) (values b c d e (min a b c d e)))) 5 4 3 2 1)) list)
              '(1 1 1 1 1))
(displayln "fixed-macro (1 clause, optional #:rest): all checks passed")
