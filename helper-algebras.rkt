#lang racket

;; Small algebraic helpers, each self-contained and documented at its definition:
;;   iso           a focused (to, from) pair -- a reversible function (detailed below)
;;   on            (on op f) a b ... = (op (f a) (f b) ...)
;;   arg           ((arg i ...) . xs): project args by 0-based position (the K combinator)
;;   pass          ((pass . args) . fs): apply each f to the fixed args, as values (the thrush / fork)
;;   fixed         iterate an `improve` step to a fixed point
;;   lexicographic lift an element comparison to a 3-way order on sequences
;;
;; The iso is the one with structure worth spelling out here.  An ISO is a focused
;; pair (to, from): applying it runs the focused side, `inverse` toggles the focus,
;; and `expt-iso` raises it to an integer power WITHOUT leaving the type -- the
;; result is still an iso, so it can be inverted or re-exponentiated in turn.
;;
;; The point of closure: isos compose as a group (the identity iso the unit,
;; `inverse` the inverse), so `expt-iso` gets the whole of Z for free -- negatives are the
;; positive powers of the inverse, and (expt-iso i -1) = (inverse i).  This is the
;; "expt -1 = inverse" of generic arithmetic (scmutils' (expt M -1), Lean's
;; a ^ (-1 : Z) = a-inverse), landing INSIDE the iso type rather than handing back
;; a bare function.

(provide (struct-out iso)        ; (iso to from); callable = applies `to`
         compose-iso             ; compose any number of isos; inverses reversed: (g.f)-1 = f-1.g-1
         expt-iso                ; iso x Z -> iso, closed on isos
         iso-law?                ; (iso-law? i x): does x round-trip through i?
         check-iso-laws          ; (check-iso-laws i xs): the inputs that don't
         on                      ; (on op f): op on its args, each projected through f
         arg                     ; ((arg i ...) . xs): selected args as values (0-based projection / K)
         pass                    ; ((pass . args) . fs): each f applied to the fixed args, as values (thrush / fork)
         fixed                   ; (fixed improve [same? equal?] [key list]): iterate to a fixed point
         lexicographic)          ; ((lexicographic cmp) l1 l2): first-difference 3-way order

;; an iso is a focused pair; `prop:procedure` runs the focused (forward) side, so
;; an iso IS a function when called -- only its own combinators see the extra half.
(struct iso (to from)
  #:property prop:procedure (struct-field-index to))

(define (inverse i) (iso (iso-from i) (iso-to i)))

;; compose any number of isos; the inverse of a composite runs the halves in
;; reverse order.  (compose-iso) with no isos is the identity iso.
(define (compose-iso . is)
  (iso (apply compose (map iso-to is))
       (apply compose (map iso-from (reverse is)))))

;; raise to an integer power, staying an iso; negatives go through the inverse.
(define (expt-iso i n)
  (cond [(negative? n) (expt-iso (inverse i) (- n))]
        [else (for/fold ([acc (iso values values)]) ([_ (in-range n)]) (compose-iso i acc))]))

;; the iso law (over equal?): i composed with its inverse is the identity --
;; running a value through `to` then back through `from` returns it unchanged.
(define (iso-law? i x) (equal? ((compose-iso (inverse i) i) x) x))

;; sweep a corpus through the law; the result is the inputs that DON'T round-trip
;; ('() means i is a genuine iso over every one of them).
(define (check-iso-laws i xs) (filter (lambda (x) (not (iso-law? i x))) xs))

;; `on`: apply op to all its arguments, each projected through f --
;; (on op f) a b ... = (op (f a) (f b) ...).  The n-ary generalization of Haskell's
;; (binary) Data.Function.on.  With a summary as f it reads each side through that
;; summary -- e.g. wrapping a guide for a bundle: (on guide smr).
(define ((on op f) . args) (apply op (map f args)))

;; `arg`: project arguments by 0-based position -- ((arg i j ...) . xs) returns the
;; i-th, j-th, ... arguments as multiple values.  The generalized projection (the K
;; combinator): (arg 0) selects the first argument -- Haskell's `const` for two args.
;; One pass: vectorize xs only up to the furthest index, then emit in `is` order.
(define ((arg . is) . xs)
  (let ([v (list->vector (take xs (add1 (apply max is))))])
    (apply values (map (lambda (i) (vector-ref v i)) is))))

;; `pass`: hold a tuple of arguments, then apply each function to them, returning
;; the results as multiple values -- ((pass . args) f g ...) = (values (apply f
;; args) (apply g args) ...).  The thrush ((pass x) f) = (f x), flipped to fix the
;; argument and await the function, generalized to a FORK over several functions
;; (Clojure's juxt, as values not a list).  One function gives one value, so it
;; threads straight through `map`.
(define ((pass . args) . fs)
  (apply values (map (lambda (f) (apply f args)) fs)))

;; `fixed`: iterate `improve` from a seed to a fixed point, returning the seeker.
;; The seed and `improve` may carry MULTIPLE values: `(compose list improve)`
;; threads improve's returned values straight back as the next call's arguments,
;; so a values-in / values-out `improve` loops with no extra plumbing.  Each step's
;; tuple is held as a list, the converged tuple returned as multiple values.
;; The halt test is an equality over a projection, mirroring `remove-duplicates`'s
;; `[same? equal?] #:key` (here both positional): stop when the projected state
;; stops changing.  `key` is applied to the value-tuple AS ARGUMENTS (not a list) --
;; default `list` rebuilds the tuple, giving whole-tuple `equal?`, a true fixed
;; point; pick a selector (`(lambda (h k) h)`) or a derived quantity to settle on
;; that instead, with an equality (`eq?`, `=`) suited to the projected value.  A step
;; that no-ops when it can make no progress is the natural halt, so such an `improve`
;; needs no separate stop test.
(define ((fixed improve [same? equal?] [key list]) . xs)
  (let loop ([xs xs])
    (define ys (apply (compose list improve) xs))   ; improve's values, listed
    (if (same? (apply key xs) (apply key ys)) (apply values ys) (loop ys))))

;; `lexicographic`: lift an element comparison to a 3-way order on sequences.
;; Walk two lists in parallel; the first non-zero elementwise verdict (`cmp` ->
;; {-1,0,1}) decides.  If they agree up to the shorter, the shorter is the lesser
;; -- a prefix precedes its extension.  ((lexicographic cmp) l1 l2) -> {-1,0,1}.
(define ((lexicographic cmp) xs ys)
  (let loop ([xs xs] [ys ys])
    (cond [(null? xs) (if (null? ys) 0 -1)]
          [(null? ys) 1]
          [else (let ([v (cmp (car xs) (car ys))])
                  (if (zero? v) (loop (cdr xs) (cdr ys)) v))])))

;; ============================================================================
(module+ test
  (require rackunit)

  (define inc (iso add1 sub1))

  ;; --- applying an iso runs its forward side; `inverse` runs the other ---
  (check-equal? (inc 10) 11)
  (check-equal? ((inverse inc) 11) 10)

  ;; --- integer powers, closed on isos ---
  (check-equal? ((expt-iso inc 3) 10) 13)         ; forward thrice
  (check-equal? ((expt-iso inc -3) 13) 10)        ; negative = inverse's power
  (check-equal? ((expt-iso inc 0) 99) 99)         ; n = 0 is the identity iso

  ;; --- the result is still an iso: invert it, re-exponentiate it ---
  (check-equal? ((inverse (expt-iso inc 3)) 13) 10)

  ;; --- compose-iso is variadic: any number of isos, inverses reversed ---
  (check-equal? ((compose-iso inc inc inc) 10) 13)          ; three composed, forward
  (check-equal? ((inverse (compose-iso inc inc inc)) 13) 10)
  (check-equal? ((compose-iso) 42) 42)                      ; no isos = the identity iso

  ;; --- the identities that closure buys ---
  (define i (iso (lambda (x) (* 2 x)) (lambda (x) (/ x 2))))
  ;; inverse and power commute
  (check-equal? ((inverse (expt-iso i 4)) 48)
                ((expt-iso i -4) 48))
  ;; expt -1 = inverse  (the generic-arithmetic identity, inside the type)
  (check-equal? ((expt-iso i -1) 6) ((inverse i) 6))
  ;; (i^m)^n = i^(m*n)
  (check-equal? ((expt-iso (expt-iso i 2) 3) 5)
                ((expt-iso i 6) 5))

  ;; --- the iso law: a genuine iso round-trips, a non-iso is caught ---
  (check-true  (iso-law? inc 10))
  (check-true  (iso-law? (expt-iso inc 3) 10))
  (check-equal? (check-iso-laws inc '(0 5 -3 99)) '())
  (define bad (iso add1 add1))             ; from doesn't undo to
  (check-false (iso-law? bad 10))
  (check-equal? (check-iso-laws bad '(1 2 3)) '(1 2 3))

  ;; --- on: every argument projected through f, then op (any arity) ---
  (check-equal? ((on + abs) -3 4) 7)               ; abs each, then +
  (check-equal? ((on + abs) -1 2 -3) 6)            ; n-ary, not just binary
  (check-equal? ((on cons add1) 1 2) '(2 . 3))

  ;; --- pass: the thrush holds the args; several functions fork, as values ---
  (check-equal? ((pass 5) add1) 6)                 ; one function = the thrush, one value
  (check-equal? (call-with-values
                 (lambda () ((pass 3 4) + * -)) list)
                '(7 12 -1))                         ; each f applied to (3 4), as values

  ;; --- fixed: single value, multiple values, and a key projection ---
  (check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)        ; halve to the fixpoint 0
  (check-equal? (call-with-values                                   ; multi-value: (a b) -> (b min)
                 (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list)
                '(3 3))
  ;; stop when a derived quantity settles -- here the tens digit -- via key:
  (check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24))
