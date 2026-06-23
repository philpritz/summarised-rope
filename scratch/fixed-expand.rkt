#lang racket
;; Show what fixed-case expands to (top-level copy of the macro so we can expand-once it).
(define improve #f) (define same? #f) (define key #f)   ; dummies so refs resolve

(define-syntax (fixed-case stx)
  (syntax-case stx ()
    [(_ v ... #:rest xs)
     (with-syntax ([(v* ...) (generate-temporaries #'(v ...))]
                   [(xs*)    (generate-temporaries #'(xs))])
       #'(let loop ([v v] ... [xs xs] [kp (apply key v ... xs)])
           (call-with-values
            (lambda () (apply improve v ... xs))
            (lambda (v* ... . xs*)
              (define kp* (apply key v* ... xs*))
              (if (same? kp kp*) (apply values v* ... xs*) (loop v* ... xs* kp*))))))]
    [(_ v ...)
     (with-syntax ([(v* ...) (generate-temporaries #'(v ...))])
       #'(let loop ([v v] ... [kp (key v ...)])
           (define-values (v* ...) (improve v ...))
           (define kp* (key v* ...))
           (if (same? kp kp*) (values v* ...) (loop v* ... kp*))))]))

(displayln "=== (fixed-case a b c #:rest xs) ===")
(pretty-print (syntax->datum (expand-once #'(fixed-case a b c #:rest xs))))
(displayln "=== (fixed-case #:rest xs)  [empty leading -> pure rest] ===")
(pretty-print (syntax->datum (expand-once #'(fixed-case #:rest xs))))
(displayln "=== (fixed-case a b)  [pure fixed] ===")
(pretty-print (syntax->datum (expand-once #'(fixed-case a b))))
