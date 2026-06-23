#lang racket

;; The simple algebra -- a reference, not production.
;;
;; `helper-algebras.rkt` at the repo root is the live module.  Several of its combinators
;; are tuned for Racket's compiler -- arity-inlined `case-lambda`s (and, in `fixed`, a
;; macro) that skip rest-arg lists / allocation on the hot path.  That speed is Chez-
;; specific, and it buries the algebra under dispatch machinery.  This file keeps the
;; implementation-INDEPENDENT forms: the one-line essence of each combinator -- what it
;; MEANS, not how Racket runs it fast.
;;
;; Reference only: nothing in the project imports this.  The root file is the source of
;; truth, and it computes the SAME values -- its production tests pin the fast forms to
;; exactly these semantics (a flipped fold, a dropped seed, a mishandled tuple would all
;; show up there).  Kept here so the elegant version is preserved alongside the tuned one.
;;
;; Only the combinators whose production form actually differs are mirrored.  The rest of
;; helper-algebras (on, pass, the iso / van-Laarhoven-lens ops) is already in its
;; simple form there -- no twin needed.

(provide variadic spread arg fixed)

;; `variadic`: lift a binary `op` (called accumulator-first, (op acc x)) and a seed `id`
;; to a variadic LEFT FOLD from the unit -- (variadic op id) a b ... = (op ... (op (op id a) b) ...).
(define ((variadic op id) . xs)
  (foldl (lambda (x acc) (op acc x)) id xs))

;; `spread`: spread-combine -- apply each function to its corresponding argument, then
;; combine the results with `h` -- (spread h f g ...) a b ... = (h (f a) (g b) ...).
(define ((spread h . fs) . xs)
  (apply h (map (lambda (f x) (f x)) fs xs)))

;; `arg`: project arguments by 0-based position, as multiple values --
;; ((arg i j ...) . xs) = (values (list-ref xs i) (list-ref xs j) ...).
(define ((arg . is) . xs)
  (apply values (map (lambda (i) (list-ref xs i)) is)))

;; `fixed`: iterate `improve` from a seed to a fixed point on the `key` projection.
;; The seed / `improve` may carry MULTIPLE values; `(compose list improve)` lists them so
;; a values-in / values-out `improve` threads straight back as the next call's arguments.
(define ((fixed improve [same? equal?] [key list]) . xs)
  (let loop ([xs xs])
    (define ys (apply (compose list improve) xs))
    (if (same? (apply key xs) (apply key ys)) (apply values ys) (loop ys))))

;; ============================================================================
;; A few checks that the simple forms compute what their names say (the same values the
;; tuned forms in helper-algebras.rkt are tested against).
(module+ test
  (require rackunit)
  (check-equal? ((variadic + 0) 1 2 3 4) 10)
  (check-equal? ((variadic - 0) 5 3) -8)                            ; (- (- 0 5) 3), order matters
  (check-equal? ((variadic cons '()) 1 2 3) '(((() . 1) . 2) . 3))
  (check-equal? ((spread + add1 sub1) 10 20) 30)                    ; (+ (add1 10) (sub1 20))
  (check-equal? ((spread list values string-length) 7 "abc") '(7 3))
  (check-equal? (call-with-values (lambda () ((arg 2 0) 'a 'b 'c)) list) '(c a))
  (check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)
  (check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24))
