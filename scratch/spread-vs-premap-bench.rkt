#lang racket

;; The new spread-combine (macro cases up to 4) vs premap, both inside variadic:
;;   premap: (variadic (premap + coerce) id)            -- accumulator untouched
;;   spread: (variadic (spread + values coerce) id)     -- spread-combine, arity-2 macro case
;; spread's arity-2 clause is now an inlined positional lambda (no rest-arg / map),
;; so it should be premap-class; the only residual is the (values acc) identity call
;; that premap skips by leaving the first arg alone.  combine=+, light coerce.

(require "../helper-algebras.rkt" "../bench/bench.rkt")

(define id 0)
(define (coerce x) (cond [(string? x) (string-length x)] [(pair? x) (car x)] [else x]))

(define prem (variadic (premap + coerce) id))
(define sprd (variadic (spread + values coerce) id))

(define N 3000000)
(define (ns st) (* 1e6 (/ (stats-min st) N)))
(define (a1 G) (ns (measure (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i)))) #:trials 17)))
(define (a2 G) (ns (measure (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i 1)))) #:trials 17)))

(printf "premap vs spread-combine (inlined macro cases), inside variadic  (combine=+, ~a calls)\n" N)
(printf "~a  ~a  ~a\n" (~a "approach" #:min-width 10) (~a "arity1 ns" #:min-width 11) (~a "arity2 ns" #:min-width 11))
(for ([p (list (cons "premap" prem) (cons "spread" sprd))])
  (printf "~a  ~a  ~a\n" (~a (car p) #:min-width 10)
          (~a (~r (a1 (cdr p)) #:precision '(= 1)) #:min-width 11)
          (~a (~r (a2 (cdr p)) #:precision '(= 1)) #:min-width 11)))

(printf "\nspread's small-arity case is now a positional lambda (no list/map) -- premap-class;\n")
(printf "residual gap = the (values acc) identity call premap avoids. (old variadic spread: 68 ns.)\n")
