#lang racket

;; Head-to-head: optional-coerce PARAM vs minimal-variadic + premap WRAP -- the two
;; actual designs. The premap path makes 2 extra wrapper-closure calls per binary op
;; (the param calls coerce directly inside variadic's clause); this asks whether that
;; shows above noise. combine=+, light coerce, arity-2 fast path, 31 trials/round,
;; order alternated across rounds to cancel ordering/JIT bias.

(require "../helper-algebras.rkt" "../bench/bench.rkt")

(define id 0)
(define (coerce x) (cond [(string? x) (string-length x)] [(pair? x) (car x)] [else x]))
(define ((premap op f) acc x) (op acc (f x)))
(define (variadic-min op id)                          ; the PROPOSED minimal 2-arg variadic
  (case-lambda
    [(a b) (op (op id a) b)] [(a) (op id a)] [() id]
    [xs (foldl (lambda (x acc) (op acc x)) id xs)]))
(define (variadic-c op id [coerce values])            ; LOCAL copy of the shipped param variadic
  (case-lambda
    [(a b) (op (op id (coerce a)) (coerce b))]
    [(a)   (op id (coerce a))]
    [()    id]
    [xs    (foldl (lambda (x acc) (op acc (coerce x))) id xs)]))

(define imp-param  (variadic   + id coerce))             ; IMPORTED variadic, coerce PARAM
(define loc-param  (variadic-c + id coerce))             ; LOCAL    variadic, coerce PARAM
(define loc-premap (variadic-min (premap + coerce) id))  ; LOCAL minimal variadic + premap WRAP

(define N 5000000)
(define (ns st) (* 1e6 (/ (stats-min st) N)))
(define (a2 G) (ns (measure (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i 1)))) #:trials 31)))

(printf "param vs premap, controlling for cross-module  (combine=+, ~a calls x 31 trials)\n" N)
(printf "~a  ~a  ~a  ~a\n" (~a "round" #:min-width 6) (~a "imp-param" #:min-width 10)
        (~a "loc-param" #:min-width 10) (~a "loc-premap" #:min-width 10))
(define-values (is ls ps)
  (for/fold ([is '()] [ls '()] [ps '()]) ([r (in-range 8)])
    (define i (a2 imp-param)) (define l (a2 loc-param)) (define p (a2 loc-premap))
    (printf "~a  ~a  ~a  ~a\n" (~a r #:min-width 6)
            (~a (~r i #:precision '(= 2)) #:min-width 10)
            (~a (~r l #:precision '(= 2)) #:min-width 10)
            (~a (~r p #:precision '(= 2)) #:min-width 10))
    (values (cons i is) (cons l ls) (cons p ps))))
(printf "\nbest-of-~a:  imp-param ~a   loc-param ~a   loc-premap ~a  ns  (min = cleanest)\n"
        (length is) (~r (apply min is) #:precision '(= 2))
        (~r (apply min ls) #:precision '(= 2)) (~r (apply min ps) #:precision '(= 2)))
(printf "if loc-param ~~ loc-premap, the gap was cross-module; if loc-param ~~ imp-param,\n")
(printf "premap genuinely wins.\n")
