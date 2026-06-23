#lang racket
;; Decompose smr / bundle combine into dispatch vs combine vs extraction.
(require "../../rope-core.rkt"
         "../../summaries/summaries.rkt"
         "../../summaries/sexp-summary.rkt")
(define (ns label iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a  ~a ns/op\n" (~a label #:min-width 42)
          (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 7 #:align 'right)))
(define buf   (bundle char-smr word-smr strsexp-smr linecol-smr))
(define comps (list char-smr word-smr strsexp-smr linecol-smr))
(define a (buf "(define (fact n) (if (zero")) (define b (buf "? n) 1 (* n)))"))
(define lva (linecol-smr "ab\ncd")) (define lvb (linecol-smr "ef\ngh"))   ; raw linecol vals (not bundle-vals)
(define N 1000000)
(printf "decompose the BUNDLE combine (buf a b):\n")
(ns "full bundle combine  (buf a b)"               N (lambda () (buf a b)))
(ns "  hash build only (for/hasheq 4)"             N (lambda () (for/hasheq ([c (in-list comps)]) (values c 0))))
(printf "decompose ONE component smr call:\n")
(ns "comp combine on bundle-vals (smr a b)"        N (lambda () (linecol-smr a b)))
(ns "comp combine on RAW vals   (smr lva lvb)"     N (lambda () (linecol-smr lva lvb)))
(ns "  part->summary  (one extract)"               N (lambda () (part->summary a linecol-smr)))
(ns "  smr 0-arg = id (dispatch only)"             N (lambda () (linecol-smr)))
