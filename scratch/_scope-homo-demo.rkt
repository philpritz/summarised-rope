#lang racket
;; THROWAWAY: show what the homomorphism law is, concretely -- same text, different split point.
(require "scope-summary.rkt")
(define (whole s) (sv-in-scope (scope-leaf s)))
(define (split a b) (sv-in-scope (scope+ (scope-leaf a) (scope-leaf b))))

(printf "text = \"(lambda (x) \"   -- is x in scope after it?\n\n")
(printf "  measured WHOLE                         : ~s\n"   (whole "(lambda (x) "))
(printf "  split at a TOKEN boundary  \"(lambda \"|\"(x) \" : ~s\n" (split "(lambda " "(x) "))
(printf "  split in the MIDDLE of lambda \"(lambd\"|\"a (x) \": ~s\n" (split "(lambd" "a (x) "))
