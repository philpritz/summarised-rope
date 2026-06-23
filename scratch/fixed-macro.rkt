#lang racket
;; Prototype: per-clause macro for the inlined fixed arities, and the xs/list path pulled
;; out into a plain internal function (rest-loop) placed after the define-syntax.

(define (fixed improve [same? equal?] [key list])
  ;; fixed-case: hand it the clause's vars, it builds that arity's no-list loop.
  (define-syntax (fixed-case stx)
    (syntax-case stx ()
      [(_ v ...)
       (with-syntax ([(v* ...) (generate-temporaries #'(v ...))])
         #'(let loop ([v v] ... [kp (key v ...)])
             (define-values (v* ...) (improve v ...))
             (define kp* (key v* ...))
             (if (same? kp kp*) (values v* ...) (loop v* ... kp*))))]))
  ;; rest-loop: the generic tail -- the tuple held as a list, for any other arity.
  (define (rest-loop xs)
    (let ([step (compose list improve)])
      (let loop ([xs xs] [kp (apply key xs)])
        (define ys (apply step xs))
        (define kp* (apply key ys))
        (if (same? kp kp*) (apply values ys) (loop ys kp*)))))
  (case-lambda
    [(a)   (fixed-case a)]
    [(a b) (fixed-case a b)]
    [xs    (rest-loop xs)]))

;; --- arities 1, 2 (the macro clauses) ---
(require rackunit)
(check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list)
              '(3 3))
(check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24)

;; --- arities 3 and 5 (the rest-loop function) ---
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c) (values b c (min a b c)))) 9 5 7)) list)
              '(5 5 5))
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c d e) (values b c d e (min a b c d e)))) 5 4 3 2 1)) list)
              '(1 1 1 1 1))

(displayln "fixed-macro (macro + rest-loop fn): all checks passed")
