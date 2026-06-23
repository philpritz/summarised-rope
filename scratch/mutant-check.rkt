#lang racket
;; Does the `-` mutant still fail identity-law? under the CURRENT make-summary
;; (which drops the id-fold in the unary/binary fast paths)?
(require "../summaries/summary-laws.rkt" "../rope-core.rkt")

(define minus (make-summary string-length -))
(define mx    (make-summary string-length max))
(define x (minus "a"))                                  ; = 1

(printf "x = (minus \"a\") = ~a\n" x)
(printf "(minus x)        unary  = ~a   (drops id? then = x)\n" (minus x))
(printf "(minus x (minus)) right = ~a\n" (minus x (minus)))
(printf "(minus (minus) x) left  = ~a   <- (combine id x), the catching case\n" (minus (minus) x))
(newline)
(printf "identity-law?    minus  = ~a   (want #f to catch the mutant)\n" (identity-law? minus x))
(printf "associativity?   minus  = ~a   (want #f)\n"
        (associativity-law? minus (minus "") (minus "") (minus "a")))
(printf "homomorphism?    max    = ~a   (want #f)\n" (homomorphism-law? mx "ab" '(1)))
(newline)
;; the original test ALSO asserted minus PASSES homomorphism (check-true). Does it still?
(printf "homomorphism?    minus \"ab\"  '(1)   = ~a   (original asserted #t)\n"
        (homomorphism-law? minus "ab" '(1)))
(printf "  (minus \"ab\") whole = ~a   (apply minus '(\"a\" \"b\")) split = ~a\n"
        (minus "ab") (minus "a" "b"))
;; would a LEFT-identity check (combine id x = x) re-catch minus?
(define (identity-law2? smr x)
  (and (equal? (smr x) x) (equal? (smr (smr) x) x) (equal? (smr x (smr)) x)))
(printf "identity-law2?   minus  = ~a   (with left-id added; want #f)\n" (identity-law2? minus x))
(printf "identity-law2?   max    = ~a   (lawful; want #t)\n" (identity-law2? mx (mx "a")))
