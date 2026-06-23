#lang racket

;; Why is "current" faster than variadic/c in the coerce bench? Decompose the gap into
;; (1) inlining and (2) the id-fold, by holding the BODY fixed and varying only how
;; combine/coerce are bound:
;;   ms-direct  : case-lambda over MODULE-LEVEL combine/coerce -> compiler inlines them.
;;                This is what the earlier bench's "current" was -- and it is UNFAIR:
;;   ms-closure : make-summary's REAL shape -- combine is a param, coerce a local, so
;;                both are CLOSURE vars the compiler can't inline. 1 combine, no id-fold.
;;   inline-c   : variadic/c -- closure vars too, PLUS one extra combine (folds id).
;; ms-direct -> ms-closure  = the inlining my bench wrongly gave "current".
;; ms-closure -> inline-c   = the genuine cost (one extra combine).

(require "../helper-algebras.rkt" "../bench/bench.rkt")

(define id 0)
(define (coerce x) (cond [(string? x) (string-length x)] [(pair? x) (car x)] [else x]))
(define (cheap a b) (+ a b))                 ; module-level combine: the compiler knows it

(define (variadic/c op id [coerce values])
  (case-lambda
    [(a b) (op (op id (coerce a)) (coerce b))]
    [(a)   (op id (coerce a))]
    [()    id]
    [xs    (foldl (lambda (x acc) (op acc (coerce x))) id xs)]))

(define (make-msum combine coerce id)        ; real make-summary: combine/coerce CLOSURE vars
  (case-lambda
    [(a b) (combine (coerce a) (coerce b))]
    [(a)   (coerce a)]
    [()    id]
    [parts (foldl (lambda (x acc) (combine acc x)) id (map coerce parts))]))

;; module-level combine/coerce -> inlinable (the earlier bench's "current")
(define ms-direct
  (case-lambda [(a b) (cheap (coerce a) (coerce b))] [(a) (coerce a)] [() id]
               [parts (foldl (lambda (x acc) (cheap acc x)) id (map coerce parts))]))
(define ms-closure (make-msum cheap coerce id))     ; closure vars, 1 combine
(define inline-c   (variadic/c cheap id coerce))    ; closure vars, 2 combines (id-fold)

(define N 3000000)
(define (ns-of t) (* 1e6 (/ (stats-min (measure t #:trials 15)) N)))
(define (a1 G) (ns-of (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i))))))
(define (a2 G) (ns-of (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i 1))))))

(printf "decompose current-vs-variadic gap  (combine=+, ~a calls/measure, per-call ns)\n" N)
(printf "~a  ~a  ~a  ~a\n" (~a "arity" #:min-width 6)
        (~a "ms-direct" #:min-width 11) (~a "ms-closure" #:min-width 12) (~a "inline-c" #:min-width 11))
(printf "~a  ~a  ~a  ~a\n" (~a "1" #:min-width 6)
        (~a (~r (a1 ms-direct)  #:precision '(= 1)) #:min-width 11)
        (~a (~r (a1 ms-closure) #:precision '(= 1)) #:min-width 12)
        (~a (~r (a1 inline-c)   #:precision '(= 1)) #:min-width 11))
(printf "~a  ~a  ~a  ~a\n" (~a "2" #:min-width 6)
        (~a (~r (a2 ms-direct)  #:precision '(= 1)) #:min-width 11)
        (~a (~r (a2 ms-closure) #:precision '(= 1)) #:min-width 12)
        (~a (~r (a2 inline-c)   #:precision '(= 1)) #:min-width 11))
(printf "\nms-direct->ms-closure = inlining the earlier bench wrongly gave 'current';\n")
(printf "ms-closure is make-summary's REAL cost.  ms-closure->inline-c = the id-fold.\n")
