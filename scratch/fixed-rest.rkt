#lang racket
;; Q: can fixed-case take (a b c #:rest xs) -- leading inlined + a rest list -- and is it
;; still an efficiency gain?  (1) macro form + correctness, (2) a bench vs exact-arity & bare-list.

;; ---------- (1) the macro, now accepting a leading-vars + #:rest form ----------
(define (fixed improve [same? equal?] [key list])
  (define-syntax (fixed-case stx)
    (syntax-case stx ()
      [(_ v ... #:rest xs)                       ; leading inlined, tail as a list (v ... may be empty)
       (with-syntax ([(v* ...) (generate-temporaries #'(v ...))]
                     [(xs*)    (generate-temporaries #'(xs))])
         #'(let loop ([v v] ... [xs xs] [kp (apply key v ... xs)])
             (call-with-values (lambda () (apply improve v ... xs))
               (lambda (v* ... . xs*)
                 (define kp* (apply key v* ... xs*))
                 (if (same? kp kp*) (apply values v* ... xs*) (loop v* ... xs* kp*))))))]
      [(_ v ...)                                 ; pure fixed arity, fully inlined (no apply/list)
       (with-syntax ([(v* ...) (generate-temporaries #'(v ...))])
         #'(let loop ([v v] ... [kp (key v ...)])
             (define-values (v* ...) (improve v ...))
             (define kp* (key v* ...))
             (if (same? kp kp*) (values v* ...) (loop v* ... kp*))))]))
  (case-lambda
    [(a)          (fixed-case a)]
    [(a b)        (fixed-case a b)]
    [(a b c . xs) (fixed-case a b c #:rest xs)]))  ; arity 3+: first 3 inlined, tail listed

(require rackunit)
(check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)
(check-equal? (call-with-values (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list) '(3 3))
(check-equal? (call-with-values (lambda () ((fixed (lambda (a b c) (values b c (min a b c)))) 9 5 7)) list) '(5 5 5))
(check-equal? (call-with-values
               (lambda () ((fixed (lambda (a b c d e) (values b c d e (min a b c d e)))) 5 4 3 2 1)) list)
              '(1 1 1 1 1))
(displayln "mixed macro: correctness ok")

;; ---------- (2) three shapes, hand-written, on a 5-value loop that runs ~A iterations ----------
;; improve decrements the first value to 0; key = the first value; ~A steps to converge.
(define (improve5 a b c d e) (values (if (> a 0) (sub1 a) a) b c d e))
(define (key5 a b c d e) a)

(define exact                                ; exact 5-arity, fully inlined: (improve a b c d e), no apply
  (let ([improve improve5] [same? =] [key key5])
    (lambda (a b c d e)
      (let loop ([a a] [b b] [c c] [d d] [e e] [kp (key a b c d e)])
        (define-values (a* b* c* d* e*) (improve a b c d e))
        (define kp* (key a* b* c* d* e*))
        (if (same? kp kp*) (values a* b* c* d* e*) (loop a* b* c* d* e* kp*))))))

(define mixed                                ; 3 inlined + #:rest: (apply improve a b c xs) + rest capture
  (let ([improve improve5] [same? =] [key key5])
    (lambda (a b c . xs)
      (let loop ([a a] [b b] [c c] [xs xs] [kp (apply key a b c xs)])
        (call-with-values (lambda () (apply improve a b c xs))
          (lambda (a* b* c* . xs*)
            (define kp* (apply key a* b* c* xs*))
            (if (same? kp kp*) (apply values a* b* c* xs*) (loop a* b* c* xs* kp*))))))))

(define bare                                 ; whole tuple as one list: (apply step xs), ys a list
  (let ([improve improve5] [same? =] [key key5])
    (lambda xs
      (let ([step (compose list improve)])
        (let loop ([xs xs] [kp (apply key xs)])
          (define ys (apply step xs)) (define kp* (apply key ys))
          (if (same? kp kp*) (apply values ys) (loop ys kp*)))))))

(define A 1000)   ; iterations per fixpoint search
(define M 4000)   ; searches per bench
(define (bench label seeker)
  (collect-garbage) (collect-garbage) (collect-garbage)
  (printf "~a\t" label)
  (define acc (time (for/fold ([s 0]) ([i (in-range M)])
                      (+ s (call-with-values (lambda () (seeker A 4 3 2 1)) (lambda (a . _) a))))))
  (void acc))

(printf "\n~a searches x ~a iters:\n" M A)
(bench "exact  (5 inlined, no apply) " exact)
(bench "mixed  (3 inlined + #:rest)  " mixed)
(bench "bare   (whole tuple a list)  " bare)
