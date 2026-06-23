#lang racket
;; THROWAWAY: run the CURRENT (all-binders) monoid through the summary-law battery.
(require rackcheck rackunit racket/match
         "scope-summary.rkt" "../rope-core.rkt" "../summaries/summary-laws.rkt")

(define smr (make-summary scope-leaf scope+))

;; generator over binder tokens (incl define/for/let-family); whole tokens
(define gen:scope
  (gen:map (gen:list (gen:one-of '("lambda" "let" "let*" "letrec" "define" "for"
                                   "(" ")" "[" "]" "x" "y" "f" "1" " "))
                     #:max-length 12)
           (lambda (xs) (apply string-append xs))))
;; single-char-atom domain: no token can be split by a cut
(define gen:flat (gen:string (gen:one-of (string->list "xyf()[] ")) #:max-length 14))

;; clean predicate-based counts (no reliance on reading rackcheck output)
(define (count-fails label n thunk) (printf "  ~a : ~a / ~a violations\n" (~a label #:min-width 16) (thunk) n))

(printf "=== current monoid vs summary-law battery (predicate counts over 2000 samples) ===\n")
(define xs (sample (gen:map gen:scope smr) 2000))
(define ys (sample (gen:map gen:scope smr) 2000))
(define zs (sample (gen:map gen:scope smr) 2000))
(printf "  identity      : ~a / 2000 violations\n" (for/sum ([x xs]) (if (identity-law? smr x) 0 1)))
(printf "  associativity : ~a / 2000 violations\n" (for/sum ([x xs] [y ys] [z zs]) (if (associativity-law? smr x y z) 0 1)))
(define (gen:str+cuts gs)
  (gen:let ([s gs] [is (gen:list (gen:integer-in 0 (max 0 (string-length s))) #:max-length 6)])
    (list s (sort is <))))
(define cs (sample (gen:str+cuts gen:flat) 2000))
(printf "  homomorphism  : ~a / 2000 violations (single-char atoms)\n"
        (for/sum ([c cs]) (match-define (list s is) c) (if (homomorphism-law? smr s is) 0 1)))

(printf "\n=== rackcheck shrinking (minimal counterexamples, if any) ===\n")
(check-property (make-config) (law:identity smr gen:scope))
(check-property (make-config) (law:associativity smr gen:scope))
(check-property (make-config) (law:homomorphism smr gen:flat))
(printf "(done)\n")
