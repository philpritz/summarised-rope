#lang racket
;; Where does the time go AFTER the V2 bundle? Statistical profile of a real render
;; driven by the V2 (vector fast-binary) bundle, plus the syntax-in-navigation ablation
;; re-measured under V2.
;; Run:  racket scratch/render-highlight/profile-v2.rkt

(require profile
         "../../rope-core.rkt"
         "../../summaries/summaries.rkt"     ; char-smr linecol-smr
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         "highlight.rkt"                     ; make-kw-smr
         "renderer.rkt"                      ; hl open-doc render line-head
         "bundle-fast.rkt")                  ; make-fbundle

(define (ns label iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ns/op\n" (~a label #:min-width 26)
          (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 9 #:align 'right)))

(define kws    '("define" "lambda" "let" "if" "cond"))
(define kw-smr (make-kw-smr kws))
(define (named . cs) (map cons cs (map object-name cs)))
(define full (make-fbundle (named char-smr kw-smr strsexp-smr linecol-smr)))
(define lite (make-fbundle (named char-smr linecol-smr)))

(define snip "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")
(define big  (apply string-append (make-list 800 snip)))     ; 4000 lines
(define h    (hl kws kw-smr full))
(define z    (open-doc h big))
(define z-lite (open-doc (hl kws kw-smr lite) big))

(printf "=== V2 ablation: line-head, full (4 comps) vs lite (char+linecol) ===\n")
(ns "full bundle (4 comps)" 3000 (lambda () (line-head z 2000)))
(ns "lite bundle (2 comps)" 3000 (lambda () (line-head z-lite 2000)))

(printf "\n=== statistical profile: V2 bundle, 8 x (render 1000 lines) ===\n")
(profile-thunk (lambda () (for ([i (in-range 8)]) (render h z 0 1000))))
