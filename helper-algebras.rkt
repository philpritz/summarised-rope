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
         expt-iso)               ; iso x Z -> iso, closed on isos

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
                ((expt-iso i 6) 5)))
