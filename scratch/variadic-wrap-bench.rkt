#lang racket

;; Optional-coerce PARAM vs WRAPPING combine. variadic's op is (op acc x); only the
;; ELEMENT x needs coercing (acc is already coerced), so a wrap must touch arg 1 and
;; pass arg 0. All compute the SAME value -- they differ only in per-op plumbing:
;;   opt-coerce  : (variadic + 0 coerce)                                 -- the shipped param
;;   wrap-lambda : (variadic (lambda (acc x) (+ acc (coerce x))) 0)      -- hand wrap
;;   wrap-compose: (variadic (compose + (lambda (acc x) (values acc (coerce x)))) 0)
;;   wrap-on-arg : (variadic (compose + (on-arg 1 coerce)) 0)            -- combinator wrap
;;   current     : make-summary's hand-rolled clause (1 combine, no wrap) -- floor
;; combine=+, light coerce, literal-arity calls (fast path), per-call ns.

(require "../helper-algebras.rkt" "../bench/bench.rkt")

(define id 0)
(define (coerce x) (cond [(string? x) (string-length x)] [(pair? x) (car x)] [else x]))

;; the hypothesised `on-arg`: map argument i through f, pass the rest, all as values.
;; VARIADIC: collects args into a list (rest-arg), rebuilds a list (for/list), then
;; `apply values` spreads it back out -- two list allocations + a loop per call.
(define ((on-arg i f) . xs)
  (apply values (for/list ([x (in-list xs)] [j (in-naturals)]) (if (= j i) (f x) x))))

;; the SAME idea, but POSITIONAL (arity-2): no rest-arg list, no for/list, no apply.
(define ((on-arg2 f) acc x) (values acc (f x)))

;; `premap`: the general wrap-lambda -- pre-map the element through f, then combine.
;; Positional, single value, no compose: (premap op f) acc x = (op acc (f x)).
(define ((premap op f) acc x) (op acc (f x)))

(define opt-coerce   (variadic + id coerce))
(define wrap-lambda  (variadic (lambda (acc x) (+ acc (coerce x))) id))
(define wrap-premap  (variadic (premap + coerce) id))
(define wrap-compose (variadic (compose + (lambda (acc x) (values acc (coerce x)))) id))
(define wrap-on-arg  (variadic (compose + (on-arg 1 coerce)) id))
(define wrap-on-arg2 (variadic (compose + (on-arg2 coerce)) id))
(define current      ; faithful make-summary shape: combine a closure var, 1 combine, drops id
  ((lambda (combine)
     (case-lambda [(a b) (combine (coerce a) (coerce b))] [(a) (coerce a)] [() id]
                  [parts (foldl (lambda (x acc) (combine acc x)) id (map coerce parts))]))
   +))

(define N 3000000)
(define (ns-of t) (* 1e6 (/ (stats-min (measure t #:trials 17)) N)))
(define (a1 G) (ns-of (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i))))))
(define (a2 G) (ns-of (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (G i 1))))))

(printf "incorporate coerce into variadic: param vs wrapping combine  (combine=+, ~a calls)\n" N)
(printf "~a  ~a  ~a\n" (~a "approach" #:min-width 14) (~a "arity1 ns" #:min-width 11) (~a "arity2 ns" #:min-width 11))
(for ([p (list (cons "opt-coerce"   opt-coerce)
               (cons "wrap-lambda"  wrap-lambda)
               (cons "wrap-premap"  wrap-premap)
               (cons "wrap-compose" wrap-compose)
               (cons "wrap-on-arg"  wrap-on-arg)
               (cons "wrap-on-arg2" wrap-on-arg2)
               (cons "current"      current))])
  (printf "~a  ~a  ~a\n" (~a (car p) #:min-width 14)
          (~a (~r (a1 (cdr p)) #:precision '(= 1)) #:min-width 11)
          (~a (~r (a2 (cdr p)) #:precision '(= 1)) #:min-width 11)))

(printf "\nopt-coerce calls coerce directly in variadic's clause; the wraps interpose a\n")
(printf "closure per op call (compose/on-arg add multi-value + a list on top).\n")
