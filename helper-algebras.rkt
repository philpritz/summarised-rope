#lang racket

;; Small algebraic helpers.  An ISO is a focused pair (to, from): applying it runs
;; the focused side, `inverse` toggles the focus, and `expt-iso` raises it to an
;; integer power WITHOUT leaving the type -- the result is still an iso, so it can
;; be inverted or re-exponentiated in turn.
;;
;; The point of closure: isos compose as a group (the identity iso the unit,
;; `inverse` the inverse), so `expt-iso` gets the whole of Z for free -- negatives are the
;; positive powers of the inverse, and (expt-iso i -1) = (inverse i).  This is the
;; "expt -1 = inverse" of generic arithmetic (scmutils' (expt M -1), Lean's
;; a ^ (-1 : Z) = a-inverse), landing INSIDE the iso type rather than handing back
;; a bare function.

(provide (struct-out iso)        ; (iso to from); callable = applies `to`
         inverse                 ; the focus toggle; an involution
         iso∘                    ; compose, inverses reversed: (g.f)-1 = f-1.g-1
         expt-iso                ; iso x Z -> iso, closed on isos
         iso-law?                ; (iso-law? i x): does x round-trip through i?
         check-iso-laws          ; (check-iso-laws i xs): the inputs that don't
         on                      ; (on op f): op on its args, each projected through f
         fixed)                   ; (fixed improve [good-enough?]): iterate to a fixed point

;; an iso is a focused pair; `prop:procedure` runs the focused (forward) side, so
;; an iso IS a function when called -- only its own combinators see the extra half.
(struct iso (to from)
  #:property prop:procedure (struct-field-index to))

(define (inverse i) (iso (iso-from i) (iso-to i)))

;; compose; the inverse of a composite runs the halves in reverse order
(define (iso∘ g f)
  (iso (compose (iso-to g)   (iso-to f))
       (compose (iso-from f) (iso-from g))))

;; raise to an integer power, staying an iso; negatives go through the inverse.
(define (expt-iso i n)
  (cond [(negative? n) (expt-iso (inverse i) (- n))]
        [else (for/fold ([acc (iso values values)]) ([_ (in-range n)]) (iso∘ i acc))]))

;; the iso law (over equal?): i composed with its inverse is the identity --
;; running a value through `to` then back through `from` returns it unchanged.
(define (iso-law? i x) (equal? ((iso∘ (inverse i) i) x) x))

;; sweep a corpus through the law; the result is the inputs that DON'T round-trip
;; ('() means i is a genuine iso over every one of them).
(define (check-iso-laws i xs) (filter (lambda (x) (not (iso-law? i x))) xs))

;; `on`: apply op to all its arguments, each projected through f --
;; (on op f) a b ... = (op (f a) (f b) ...).  The n-ary generalization of Haskell's
;; (binary) Data.Function.on.  With a summary as f it reads each side through that
;; summary -- e.g. wrapping a guide for a bundle: (on guide smr).
(define ((on op f) . args) (apply op (map f args)))

;; `fixed`: iterate `improve` from a seed to a fixed point, returning the seeker.
;; The seed and `improve` may carry MULTIPLE values: `(compose list improve)`
;; threads improve's returned values straight back as the next call's arguments,
;; so a values-in / values-out `improve` loops with no extra plumbing.  Each step's
;; tuple is held as a list, the converged tuple returned as multiple values.
;; `good-enough?` compares the previous tuple against the next -- default `equal?`,
;; a true fixed point (improve changed nothing); compose it from a value predicate
;; with `on` (e.g. `(on = (lambda (t) (apply + t)))` to stop when a derived total
;; settles).  A step that no-ops when it can make no progress is the natural halt,
;; so such an `improve` needs no separate stop test.
(define ((fixed improve [good-enough? equal?]) . xs)
  (let loop ([xs xs])
    (define ys (apply (compose list improve) xs))   ; improve's values, listed
    (if (good-enough? xs ys) (apply values ys) (loop ys))))

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

  ;; --- fixed: single value, multiple values, and an `on`-composed good-enough? ---
  (check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)        ; halve to the fixpoint 0
  (check-equal? (call-with-values                                   ; multi-value: (a b) -> (b min)
                 (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list)
                '(3 3))
  ;; stop when a derived quantity settles -- here the tens digit -- via on:
  (check-equal? ((fixed sub1 (on = (lambda (t) (quotient (car t) 10)))) 25) 24))
