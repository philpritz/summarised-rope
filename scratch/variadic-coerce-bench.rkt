#lang racket

;; Coerce scenario: make-summary must coerce each arg before combining. How much does
;; "map coerce first" cost vs coercing INLINE in the fast paths? Four contenders, same
;; cheap combine (+), id 0, light coerce; op cheap so plumbing shows. Per-call ns,
;; LITERAL-arity calls (the fast path needs positional args, not apply).
;;
;;   inline-c : (variadic/c combine id coerce) -- variadic EXTENDED with an optional
;;              coerce fn, applied inside each clause (no list at arity<=2). 2 combines
;;              at binary (folds id), like the plain variadic.
;;   current  : make-summary's hand-rolled case-lambda today (1 combine at binary,
;;              drops id; map coerce + foldl only on the >=3 fallback).
;;   var.map  : (apply (variadic combine id) (map coerce parts)) -- plain variadic on a
;;              pre-coerced list: builds the rest-arg list, the mapped list, AND apply
;;              re-spreads into variadic's own rest-arg. The form you flagged.
;;   foldl.map: (foldl combine id (map coerce parts)) -- the pure must-map baseline.

(require "../helper-algebras.rkt" "../bench/bench.rkt")

(define combine +)
(define id 0)
(define (coerce x) (cond [(string? x) (string-length x)] [(pair? x) (car x)] [else x]))

;; the proposed extension: variadic with an optional inline coerce (default = values)
(define (variadic/c op id [coerce values])
  (case-lambda
    [(a b) (op (op id (coerce a)) (coerce b))]
    [(a)   (op id (coerce a))]
    [()    id]
    [xs    (foldl (lambda (x acc) (op acc (coerce x))) id xs)]))

(define V-inline (variadic/c combine id coerce))
(define MS-current
  (case-lambda
    [(a b) (combine (coerce a) (coerce b))]
    [(a)   (coerce a)]
    [()    id]
    [parts (foldl (lambda (x acc) (combine acc x)) id (map coerce parts))]))
(define V-plain (variadic combine id))
(define (V-map . parts) (apply V-plain (map coerce parts)))
(define (F-map . parts) (foldl combine id (map coerce parts)))

(define N 2000000)
(define (ns-of thunk) (* 1e6 (/ (stats-min (measure thunk #:trials 15)) N)))

;; one row = a literal arity (i varies, the rest are constants); a cell per contender.
(define-syntax-rule (row label arg ...)
  (let ([c (lambda (G) (ns-of (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i arg ...))))))])
    (printf "~a  ~a  ~a  ~a  ~a\n"
            (~a label #:min-width 7)
            (~a (~r (c V-inline)   #:precision '(= 1)) #:min-width 12)
            (~a (~r (c MS-current) #:precision '(= 1)) #:min-width 12)
            (~a (~r (c V-map)      #:precision '(= 1)) #:min-width 12)
            (~a (~r (c F-map)      #:precision '(= 1)) #:min-width 12))))

(printf "coerce scenario: per-call ns, combine=+, light coerce  (~a calls/measure)\n" N)
(printf "~a  ~a  ~a  ~a  ~a\n"
        (~a "arity" #:min-width 7) (~a "inline-c" #:min-width 12) (~a "current" #:min-width 12)
        (~a "var.map" #:min-width 12) (~a "foldl.map" #:min-width 12))
(row "1")
(row "2" 1)
(row "3" 1 2)
(row "5" 1 2 3 4)

(printf "\ninline-c = variadic/c (coerce in the clause); current = make-summary today.\n")
(printf "var.map / foldl.map MUST allocate to map coerce -- the cost of 'map first'.\n")
(printf "inline-c vs current isolates variadic's one extra id-fold at the hot arities.\n")
