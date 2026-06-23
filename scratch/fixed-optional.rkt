#lang racket
(require (for-syntax syntax/parse))
;; One macro CASE with an optional #:rest.  The pattern is single; the body picks the
;; whole template -- direct define-values when there's no rest, call-with-values when there is.

(define (fixed improve [same? equal?] [key list])
  (define-syntax (fixed-case stx)
    (syntax-parse stx
      [(_ v:id ... (~optional (~seq #:rest xs:id)))
       #:with (v* ...) (generate-temporaries #'(v ...))
       (if (attribute xs)
           (with-syntax ([(xs*) (generate-temporaries #'(xs))])
             #'(let loop ([v v] ... [xs xs] [kp (apply key v ... xs)])
                 (call-with-values
                  (lambda () (apply improve v ... xs))
                  (lambda (v* ... . xs*)
                    (define kp* (apply key v* ... xs*))
                    (if (same? kp kp*) (apply values v* ... xs*) (loop v* ... xs* kp*))))))
           #'(let loop ([v v] ... [kp (key v ...)])
               (define-values (v* ...) (improve v ...))
               (define kp* (key v* ...))
               (if (same? kp kp*) (values v* ...) (loop v* ... kp*))))]))
  (case-lambda
    [(a)          (fixed-case a)]              ; no #:rest -> define-values path
    [(a b)        (fixed-case a b)]
    [(a b c d)    (fixed-case a b c d)]
    [(a b c d . xs) (fixed-case a b c d #:rest xs)]))   ; #:rest -> call-with-values path

(require rackunit)
(check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)
(check-equal? (call-with-values (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list) '(3 3))
(check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24)
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c d) (values b c d (min a b c d)))) 9 5 7 3)) list)
              '(3 3 3 3))
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c d e) (values b c d e (min a b c d e)))) 5 4 3 2 1)) list)
              '(1 1 1 1 1))
(displayln "fixed-optional (single case, optional #:rest): all checks passed")
