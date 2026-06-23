#lang racket
(require (for-syntax syntax/parse))
;; Unified MACRO BODY: the loop skeleton (let loop / call-with-values / if same?) is written
;; ONCE.  Only the pieces differ between no-rest and rest -- (improve v ...) vs (apply improve
;; v ... xs), (key ...) vs (apply key ...), the receiver formals, the recur args -- computed by
;; the `if` and spliced in.  call-with-values is uniform (Racket fuses literal-lambda c-w-v).

(define (fixed improve [same? equal?] [key list])
  (define-syntax (fixed-case stx)
    (syntax-parse stx
      [(_ v:id ... (~optional (~seq #:rest xs:id)))
       #:with (v* ...) (generate-temporaries #'(v ...))
       (define-values (bindings improve-e recvr next-key ret recur)
         (if (attribute xs)
             (with-syntax ([(xs*) (generate-temporaries #'(xs))])
               (values #'([v v] ... [xs xs] [kp (apply key v ... xs)])  ; bindings
                       #'(apply improve v ... xs)                        ; improve-e
                       #'(v* ... . xs*)                                  ; recvr
                       #'(apply key v* ... xs*)                          ; next-key
                       #'(apply values v* ... xs*)                       ; ret
                       #'(v* ... xs*)))                                  ; recur
             (values #'([v v] ... [kp (key v ...)])
                     #'(improve v ...)
                     #'(v* ...)
                     #'(key v* ...)
                     #'(values v* ...)
                     #'(v* ...))))
       (with-syntax ([bindings bindings] [improve-e improve-e] [recvr recvr]
                     [next-key next-key] [ret ret] [(rarg ...) recur])
         ;; --- the body, written once ---
         #'(let loop bindings
             (call-with-values (lambda () improve-e)
               (lambda recvr
                 (define kp* next-key)
                 (if (same? kp kp*) ret (loop rarg ... kp*))))))]))
  (case-lambda
    [(a)            (fixed-case a)]
    [(a b)          (fixed-case a b)]
    [(a b c d)      (fixed-case a b c d)]
    [(a b c d . xs) (fixed-case a b c d #:rest xs)]))

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
(displayln "fixed-unified-macro (shared body, computed pieces): all checks passed")
