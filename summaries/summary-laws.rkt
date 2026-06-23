#lang racket

;; Summary laws: an optional conformance kit for summary writers. Given an `smr`
;; (a variadic summary fn, as built by `make-summary`) and a generator of domain
;; strings, the battery property-tests the laws the rope relies on:
;;
;;   SUMMARY laws -- the monoid (S, combine, unit):
;;     identity        (smr x) = x   and   (smr x (smr)) = x
;;     associativity   (smr (smr x y) z) = (smr x (smr y z))
;;   STRING law -- the measure is a monoid homomorphism from (text, ++):
;;     homomorphism    (apply smr (split-at-cuts s is)) = (smr s)  for any cuts is
;;
;; The two groups are a diagnosis, not just an order: a summary-group failure
;; means the combine/unit algebra is broken; a string-group failure means the
;; measure does not respect concatenation. They are independent -- combine = `-`
;; fails only the summary group (the left fold from the unit still matches the
;; whole), combine = `max` fails only the string group (a fine monoid, but max
;; of the parts is not the measure of the whole).
;;
;; Laws compare values with equal?, so a summary's values must have a sensible
;; equal?. The kit depends only on the smr value itself (plus rackcheck and
;; rackunit) -- never on rope-core. The rope-integration check (a built rope's
;; cached summary = the flat measure) is deliberately NOT in this battery: given
;; these laws it is a theorem, so it can only fail when rope-core is at fault,
;; and this battery's verdicts are about the summary.
;;
;; Each law is a plain predicate (usable in any test) wrapped in a rackcheck
;; property (random search + shrinking; a failure reports the shrunk minimal
;; counterexample and the seed to replay it). `check-summary-laws` runs the
;; battery: an optional #:corpus of curated strings is swept deterministically
;; first -- every entry, every single cut, every run (pinned cases, in the
;; Hypothesis style) -- then mixed 1:3 into the random stream. The corpus
;; doubles as regression memory: append a shrunk counterexample and it is
;; re-checked deterministically forever after.
;;
;; Design notes: discussions/2026-06-11/3-claude.md.

(provide
 check-summary-laws      ; (check-summary-laws smr gs [#:corpus strs] [#:config c])
 law:identity            ; smr x string-gen -> property; each law is also usable
 law:associativity       ;   alone via (check-property [config] (law:... smr gs))
 law:homomorphism
 identity-law?           ; the laws as plain predicates over concrete values:
 associativity-law?      ;   (identity-law? smr x), (associativity-law? smr x y z),
 homomorphism-law?)      ;   (homomorphism-law? smr s cuts)

(require rackcheck rackunit racket/match)

;; ---------- the laws, as predicates ----------
;; identity: folding a lone summary value from the unit returns it unchanged,
;; and folding the unit (smr) after it changes nothing.
(define (identity-law? smr x)
  (and (equal? (smr x) x)
       (equal? (smr x (smr)) x)))

;; associativity: grouping of combines doesn't matter. x y z are summary values.
(define (associativity-law? smr x y z)
  (equal? (smr (smr x y) z) (smr x (smr y z))))

;; split-at-cuts: the substrings between consecutive cut offsets (sorted,
;; repeats allowed -- a repeat yields an empty chunk, testing the unit in
;; context). 0 and (string-length s) bracket implicitly.
(define (split-at-cuts s is)
  (for/list ([a (in-list (cons 0 is))]
             [b (in-list (append is (list (string-length s))))])
    (substring s a b)))

;; homomorphism: measuring the whole = folding the measures of any split.
(define (homomorphism-law? smr s is)
  (equal? (apply smr (split-at-cuts s is)) (smr s)))

;; ---------- input generators ----------
(define (gen:summary smr gs) (gen:map gs smr))    ; summary values, via measuring

(define (gen:string+cuts gs)                      ; a string and 0..6 sorted cuts
  (gen:let ([s gs]
            [is (gen:list (gen:integer-in 0 (string-length s)) #:max-length 6)])
    (list s (sort is <))))

;; ---------- the laws, as properties ----------
(define (law:identity smr gs)
  (property #:name 'identity
    ([x (gen:summary smr gs)])
    (check-true (identity-law? smr x))))

(define (law:associativity smr gs)
  (property #:name 'associativity
    ([x (gen:summary smr gs)] [y (gen:summary smr gs)] [z (gen:summary smr gs)])
    (check-true (associativity-law? smr x y z))))

(define (law:homomorphism smr gs)
  (property #:name 'homomorphism
    ([c (gen:string+cuts gs)])
    (match-define (list s is) c)
    (label! (format "~a cuts" (length is)))
    (check-true (homomorphism-law? smr s is))))

;; ---------- the battery ----------
;; The corpus sweep is deterministic and exhaustive by construction: every entry
;; is identity-checked and homomorphism-checked at EVERY single cut, every run.
;; Multi-cut splits of corpus entries are left to the random layer, which mixes
;; the corpus into the generated stream.
(define (sweep-corpus smr corpus)
  (for ([s (in-list corpus)])
    (check-true (identity-law? smr (smr s))
                (format "identity on corpus entry ~s" s))
    (for ([i (in-range (add1 (string-length s)))])
      (check-true (homomorphism-law? smr s (list i))
                  (format "homomorphism: ~s cut at ~a" s i)))))

(define (check-summary-laws smr gs
                            #:corpus [corpus '()]
                            #:config [c (make-config)])
  (sweep-corpus smr corpus)
  (define gs* (if (null? corpus)
                  gs
                  (gen:frequency `((3 . ,gs) (1 . ,(gen:one-of corpus))))))
  (check-property c (law:identity smr gs*))
  (check-property c (law:associativity smr gs*))
  (check-property c (law:homomorphism smr gs*)))

;; ============================================================================
(module+ test
  (require "../rope-core.rkt")          ; make-summary -- a test-only dependency

  (define cc (make-summary string-length +))
  (define gs:ab (gen:string (gen:one-of (string->list "ab( )")) #:max-length 12))

  ;; --- the battery passes on a lawful summary, corpus included ---
  (check-summary-laws cc gs:ab #:corpus (list "" "a" "((b a)" "a b"))

  ;; --- the battery's teeth: each law group catches its own mutant ---
  ;; combine = `-` breaks the monoid, but folded left from the unit it still
  ;; matches measuring the whole: ONLY the summary group fails.
  (define minus (make-summary string-length -))
  (check-false (identity-law? minus (minus "a")))
  (check-false (associativity-law? minus (minus "") (minus "") (minus "a")))
  (check-true  (homomorphism-law? minus "ab" '(1)))
  (check-true  (homomorphism-law? minus "abc" '(1 2)))

  ;; combine = `max` is a fine monoid, but max of the parts is not the measure
  ;; of the whole: ONLY the string group fails.
  (define mx (make-summary string-length max))
  (check-true  (identity-law? mx (mx "a")))
  (check-true  (associativity-law? mx (mx "a") (mx "bb") (mx "c")))
  (check-false (homomorphism-law? mx "ab" '(1))))
