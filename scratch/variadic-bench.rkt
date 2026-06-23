#lang racket

;; Throwaway: `variadic` (helper-algebras) vs the naive variadic-via-foldl, across
;; arities. variadic's only edge is its 0/1/2-ary inline paths, which skip the
;; rest-arg list and the foldl driver -- so it must be called with LITERAL positional
;; args (the way make-summary's smr is), not `apply` (which rebuilds the list the fast
;; path avoids). The gap is therefore pure PLUMBING (list alloc + fold loop): both do
;; the same number of `op` calls, so with a heavy op the gap vanishes. We bench a CHEAP
;; op (+), the regime where plumbing dominates -- e.g. char-smr's combine.

(require "../helper-algebras.rkt"
         "../bench/bench.rkt")

(define op +)                                    ; called accumulator-first: (op acc x)
(define id 0)
(define V (variadic op id))                      ; the fast-path folder
(define (F . xs) (foldl (lambda (x acc) (op acc x)) id xs))   ; naive variadic foldl

;; a variadic that takes an optional coerce fn and coerces each element INLINE in the
;; clauses (id stays raw) -- vs the current make-summary style of mapping coerce over a
;; freshly-built list and THEN folding. coerce defaults to values (no coercion).
(define (variadic/coerce op id [coerce values])
  (case-lambda
    [(a b) (op (op id (coerce a)) (coerce b))]
    [(a)   (op id (coerce a))]
    [()    id]
    [xs    (foldl (lambda (x acc) (op acc (coerce x))) id xs)]))

;; a representative coerce: a small type-dispatch that passes numbers through (the
;; bench's args are numbers), mirroring make-summary's match on string/rope/part/else.
(define (coerce x)
  (cond [(string? x) (string-length x)]
        [(pair? x)   (car x)]
        [else        x]))
(define Vc (variadic/coerce op id coerce))                                    ; inline coerce
(define (Fmap . xs) (foldl (lambda (x acc) (op acc x)) id (map coerce xs)))    ; map coerce, then fold

(define N 3000000)                               ; calls per timed thunk

;; per-call nanoseconds from a stats (min of N-call trials), loop overhead included
;; equally in both arms (so the V-vs-F DELTA is the clean plumbing cost).
(define (ns st) (* 1e6 (/ (stats-min st) N)))

(define (row label v-thunk f-thunk)
  (define v (ns (measure v-thunk #:trials 21)))
  (define f (ns (measure f-thunk #:trials 21)))
  (printf "~a  ~a  ~a  ~a  ~a\n"
          (~a label #:min-width 8)
          (~a (~r v #:precision '(= 1)) #:min-width 12)
          (~a (~r f #:precision '(= 1)) #:min-width 12)
          (~a (~r (- f v) #:precision '(= 1)) #:min-width 12)
          (~a (~r (/ f v) #:precision '(= 2)) #:min-width 8)))

;; literal-arity thunks; `i` keeps an arg non-constant, the running sum defeats DCE.
(printf "variadic vs naive foldl, op = +  (~a calls/measure, per-call ns)\n" N)
(printf "~a  ~a  ~a  ~a  ~a\n"
        (~a "arity" #:min-width 8) (~a "variadic ns" #:min-width 12)
        (~a "foldl ns" #:min-width 12) (~a "delta ns" #:min-width 12) (~a "x" #:min-width 8))

(row "0" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V))))
         (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (F)))))
(row "1" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i))))
         (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (F i)))))
(row "2" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i 1))))
         (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (F i 1)))))
(row "3" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i 1 2))))
         (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (F i 1 2)))))
(row "5" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i 1 2 3 4))))
         (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (F i 1 2 3 4)))))
(row "8" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i 1 2 3 4 5 6 7))))
         (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (F i 1 2 3 4 5 6 7)))))

(printf "\ndelta = per-call plumbing variadic saves; x = foldl/variadic.\n")
(printf "arity<=2 hits an inline clause (no list, no foldl); arity>=3 falls to the\n")
(printf "same foldl as F, so they converge (variadic pays one extra case dispatch).\n")

;; --- heavy op: both arms do the SAME 2 op-calls at arity 2, so once op dominates
;;     the plumbing the gap should close (~1.0x). Confirms the caveat above. ---
(define (hop acc x) (for/fold ([a acc]) ([_ (in-range 200)]) (+ a x)))   ; ~200 adds/call
(define HV (variadic hop id))
(define (HF . xs) (foldl (lambda (x acc) (hop acc x)) id xs))
(printf "\nheavy op (~~200 adds), arity 2:\n")
(row "2-hvy" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (HV i 1))))
             (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (HF i 1)))))

;; --- coerce: the make-summary reality. inline-coerce variadic (Vc) vs map-coerce-
;;     then-fold (Fmap); V (no coerce) is the reference, so Vc/V is what coercion costs
;;     and Fmap/Vc is how much worse mapping coerce first is. ---
(define (crow label v vc fmap)
  (define a (ns (measure v #:trials 21)))
  (define b (ns (measure vc #:trials 21)))
  (define c (ns (measure fmap #:trials 21)))
  (printf "~a  ~a  ~a  ~a  ~a  ~a\n"
          (~a label #:min-width 8)
          (~a (~r a #:precision '(= 1)) #:min-width 11)
          (~a (~r b #:precision '(= 1)) #:min-width 12)
          (~a (~r c #:precision '(= 1)) #:min-width 13)
          (~a (~r (/ b a) #:precision '(= 2)) #:min-width 7)
          (~a (~r (/ c b) #:precision '(= 2)) #:min-width 9)))

(printf "\ncoerce (dispatch + passthrough): inline-coerce variadic vs map-coerce-then-fold\n")
(printf "~a  ~a  ~a  ~a  ~a  ~a\n"
        (~a "arity" #:min-width 8) (~a "V plain" #:min-width 11) (~a "Vc inline" #:min-width 12)
        (~a "Fmap m+f" #:min-width 13) (~a "Vc/V" #:min-width 7) (~a "Fmap/Vc" #:min-width 9))
(crow "1" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i))))
          (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (Vc i))))
          (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (Fmap i)))))
(crow "2" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i 1))))
          (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (Vc i 1))))
          (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (Fmap i 1)))))
(crow "3" (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (V i 1 2))))
          (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (Vc i 1 2))))
          (lambda () (for/fold ([s 0]) ([i (in-range N)]) (+ s (Fmap i 1 2)))))

(printf "\nVc/V = cost of inline coercion;  Fmap/Vc = how much worse map-coerce-first is.\n")
