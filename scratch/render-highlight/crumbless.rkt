#lang racket

;; A crumb-free descent that isolates a cut's HEAD edges -- before-summary and after-summary
;; -- as standalone values, WITHOUT the zipper's crumb stack and WITHOUT multisect's rope
;; rebuilding.  It walks the tree, and at each branch folds the off-path child's CACHED
;; summary into `before` (descending right) or `after` (descending left); it never rope-joins
;; the before/after sides back into ropes.  So you get bs/as directly, ready to be the place
;; the boxes accumulate into.
;;
;;   multisect:  splits -> rebuilds 3 ropes (rope-join allocations) + you read their summaries
;;   zipper:     builds a crumb stack (closures) so you can go back UP
;;   this:       folds only the summaries, no crumbs, no rope reconstruction
;;
;; Run:  racket scratch/render-highlight/crumbless.rkt

(require racket/match
         "../../rope-core.rkt"               ; make-rope multisect
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         (submod "../../rope-core.rkt" internal))   ; leaf? branch-left branch-right leaf-text

(define buf (bundle char-smr strsexp-smr linecol-smr))

(define ((col line k) L R)
  (match-define (linecol _ ll lc) (linecol-smr L))
  (cond [(not (= ll line)) (if (< ll line) 1 -1)] [(< lc k) 1] [(> lc k) -1] [else 0]))

;; ---------- the crumb-free descent: cut -> (values before-summary after-summary) ----------
;; before/after are accumulated by COMBINING cached child summaries (no crumbs, no rope-join).
;; (uses buf-combine, which allocates a bundle-val per level -- the BOX version replaces exactly
;;  this combine with an in-place fold; see notes.)
(define (cut g rope)
  (let descend ([before (buf "")] [t rope] [after (buf "")])
    (cond
      [(leaf? t)
       (define s (leaf-text t))
       ;; the guide is monotone in the split point -> binary search it (leaf <= maxleaf)
       (define k (let bs ([lo 0] [hi (string-length s)])
                   (if (>= lo hi) lo
                       (let* ([mid (quotient (+ lo hi) 2)]
                              [d (g (buf before (substring s 0 mid)) (buf (substring s mid) after))])
                         (if (positive? d) (bs (add1 mid) hi) (bs lo mid))))))
       (values (buf before (substring s 0 k)) (buf (substring s k) after))]
      [else
       (define l (branch-left t)) (define r (branch-right t))
       (define L (buf before l))             ; before (+) summary(l)   -- (buf l) reads l's cache, O(1)
       (define R (buf r after))              ; summary(r) (+) after
       (case (g L R)
         [(1)  (descend L r after)]          ; cut in r: l joins before
         [(-1) (descend before l R)]         ; cut in l: r joins after
         [else (values L R)])])))            ; cut at this seam

;; ============================================================================
(module+ main
  (require rackunit)

  ;; ---------- correctness: bs/as match multisect's pieces, on real docs + every cut ----------
  (define (check-doc text)
    (define rope ((make-rope buf) text))
    (define n (add1 (linecol-lines (linecol-smr rope))))
    (for ([L (in-range n)])
      (define g (col L 0))
      (define-values (pre post) ((multisect buf (vector g)) rope))   ; reference: real split
      (define-values (bs as)    (cut g rope))                        ; crumb-free descent
      (check-equal? (char-smr bs) (char-smr pre)  (format "char bs @~a" L))
      (check-equal? (linecol-smr bs) (linecol-smr pre) (format "lc bs @~a" L))
      (check-equal? (strsexp-smr bs) (strsexp-smr pre) (format "sx bs @~a" L))
      (check-equal? (char-smr as) (char-smr post) (format "char as @~a" L))
      (check-equal? (strsexp-smr as) (strsexp-smr post) (format "sx as @~a" L))))
  (check-doc "(define (f x)\n  (if (> x 0)\n      (list \"(a)\" x)\n      (g x)))\n")
  (check-doc (apply string-append (make-list 40 "(define (fib n)\n  (if (< n 2) n (+ a b)))\n")))
  (printf "correctness: crumb-free bs/as == multisect pieces, every line of both docs  ok\n\n")

  ;; ---------- cost: crumb-free descent vs multisect, on a 4000-line doc ----------
  (define big (apply string-append
                     (make-list 800 "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")))
  (define rope ((make-rope buf) big))
  (define g (col 2000 0))
  (define (ns label iters thunk)
    (thunk) (collect-garbage)
    (define-values (_ c r gc) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "  ~a ~a us/cut\n" (~a label #:min-width 30) (~a (~r (/ (* r 1000.0) iters) #:precision 1) #:min-width 7 #:align 'right)))
  (printf "single line cut, mid 4000-line doc:\n")
  (ns "crumb-free descent (cut)"     5000 (lambda () (cut g rope)))
  (ns "multisect (split + rejoin)"   5000 (lambda () ((multisect buf (vector g)) rope)))

  ;; deep linear nesting: where before/after summaries are heavy
  (define (linear d) (let loop ([k 0] [a ((make-rope buf) "x")])
                       (if (= k d) a (loop (add1 k) ((make-rope buf) "(\n" a "\n)")))))
  (define deep (linear 2000))
  (define gd (col 2000 0))
  (printf "\nsingle line cut, deep linear nesting (depth 2000, 4001 lines):\n")
  (ns "crumb-free descent (cut)"     3000 (lambda () (cut gd deep)))
  (ns "multisect (split + rejoin)"   3000 (lambda () ((multisect buf (vector gd)) deep))))
