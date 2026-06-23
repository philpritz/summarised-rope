#lang racket

;; Cost-attribution harness for the renderer. Four views:
;;   1. microbenchmarks (ns/op) for each summary-component combine and leaf;
;;   2. microbenchmarks for the rope/render ops;
;;   3. an ABLATION -- navigation under the full bundle vs a lite (char+linecol) bundle,
;;      isolating what the heavy syntax components cost the navigation;
;;   4. a STATISTICAL PROFILE of a real render (which functions actually dominate).
;;
;; Run:  racket scratch/render-highlight/costs.rkt

(require profile
         "../../rope-core.rkt"               ; make-rope multisect
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr
         "../../summaries/sexp-summary.rkt"  ; sexp-smr strsexp-smr
         "renderer.rkt"                      ; make-hl open-doc render line-head col (struct hl)
         "highlight.rkt")                    ; make-kw-smr

;; ns/op: run `thunk` `iters` times, report nanoseconds per call + gc time.
(define (ns label iters thunk)
  (thunk)                                    ; warm up
  (collect-garbage)
  (define-values (_ cpu real gc)
    (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ns/op   gc ~a ms / ~a iters\n"
          (~a label #:min-width 24)
          (~a (~r (/ (* real 1e6) iters) #:precision 1) #:min-width 9 #:align 'right)
          gc iters))

(define kws '("define" "lambda" "let" "if" "cond"))
(define kw-smr (make-kw-smr kws))
(define buf (bundle char-smr kw-smr strsexp-smr linecol-smr))

;; ---------- 1. component combine costs: two pre-built values -> (smr a b) ----------
(define text "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))")
(define mid  (quotient (string-length text) 2))
(define (cb smr) (let ([a (smr (substring text 0 mid))] [b (smr (substring text mid))]) (lambda () (smr a b))))
(printf "component combine  (smr a b):\n")
(ns "char-smr"        300000 (cb char-smr))
(ns "linecol-smr"     300000 (cb linecol-smr))
(ns "kw-smr"          300000 (cb kw-smr))
(ns "sexp-smr"        300000 (cb sexp-smr))
(ns "strsexp-smr"     300000 (cb strsexp-smr))
(ns "bundle (hasheq)" 300000 (cb buf))

;; ---------- leaf (summarize one line string) costs ----------
(printf "\nleaf  (smr str):\n")
(ns "sexp-smr"    100000 (lambda () (sexp-smr text)))
(ns "strsexp-smr" 100000 (lambda () (strsexp-smr text)))
(ns "bundle"      100000 (lambda () (buf text)))

;; ---------- 2. rope / render ops ----------
(printf "\nrope / render ops:\n")
(define h    (make-hl kws))
(define snip "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")
(define big  (apply string-append (make-list 800 snip)))
(define rope ((make-rope (hl-buf h)) big))
(define z    (open-doc h big))
(ns "make-rope (4000 lines)"   20 (lambda () ((make-rope (hl-buf h)) big)))
(ns "multisect (1 cut)"      3000 (lambda () ((multisect (hl-buf h) (vector (col 2000 0))) rope)))
(ns "line-head (nav+head)"   3000 (lambda () (line-head z 2000)))
(ns "render 1 line"          3000 (lambda () (render h z 2000 1)))

;; ---------- 3. ablation: navigation cost vs bundle weight ----------
(printf "\nablation -- line-head, full bundle vs lite (char+linecol):\n")
(define h-lite (hl kws kw-smr (bundle char-smr linecol-smr)))
(define z-lite (open-doc h-lite big))
(ns "full bundle (4 comps)"  3000 (lambda () (line-head z 2000)))
(ns "lite bundle (2 comps)"  3000 (lambda () (line-head z-lite 2000)))

;; ---------- 4. statistical profile of a real render ----------
(printf "\n=== statistical profile: 8 x (render 1000 lines) ===\n")
(profile-thunk (lambda () (for ([i (in-range 8)]) (render h z 0 1000))))
