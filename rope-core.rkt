#lang racket

;; Summarised rope, variadic-polymorphic rewrite.
;;
;; Two variadic "coerce-and-fold" factories:
;;
;;   summary : (string | rope | summary)* -> summary    ; built by `summariser`
;;   roper   : (string | rope)*           -> rope        ; rope factory
;;
;; Ropes participate in Racket's display/write protocol (prop:custom-write),
;; so (~a r), (format "~a" r), (display r) and (with-output-to-string ...)
;; all yield / emit the rope's text. No bespoke rope->string in the public API.
;;
;; A summary function is built by `summariser` and is the single handle threaded
;; into rope construction (what older versions called `sys`). Threaded summary
;; handles are bound as `smr` to keep them distinct from the canonical `summary`
;; function. Every rope node is tagged with the summary it was built under, so
;; `summary` can verify (by eq?) that a rope's cached value belongs to the
;; summary now folding with it.
;;
;; Factories carry an `-er`/`-r` suffix to read as "the thing that makes X":
;; `summariser`, `roper`. Each takes its config and returns the worker function.
;; A pure, guide-free rope library: make, summarise, split (`bisect`), join
;; (`roper`), display. All guided navigation lives in `zipper-core.rkt`, built on
;; these primitives.
;;
;; Design notes: discussions/2026-05-29/2-claude.md.

(require racket/match)

(provide
 ;; the rope API
 summariser
 roper
 bisect
 ;; structural primitives the zipper builds on
 rope-algebra
 atom?
 empty-rope
 empty-rope?)

;; ---------- nodes ----------
;; Each node caches its summary value AND the summary fn it was built under.
;; `rope-summary` reads the cached value; `rope-algebra` reads the fn.

(struct leaf (text summary algebra)
  #:transparent
  #:property prop:custom-write
  (lambda (r port mode) (rope-write-text r port)))

(struct leaf-range (text start end summary algebra)
  #:transparent
  #:property prop:custom-write
  (lambda (r port mode) (rope-write-text r port)))

(struct branch (left right summary algebra)
  #:transparent
  #:property prop:custom-write
  (lambda (r port mode) (rope-write-text r port)))

(define (rope? v) (or (leaf? v) (leaf-range? v) (branch? v)))

(define (rope-summary r)
  (match r
    [(leaf _ s _)           s]
    [(leaf-range _ _ _ s _) s]
    [(branch _ _ s _)       s]))

(define (rope-algebra r)
  (match r
    [(leaf _ _ a)           a]
    [(leaf-range _ _ _ _ a) a]
    [(branch _ _ _ a)       a]))

;; ---------- summariser ----------
;; (summariser measure combine) -> the variadic `summary` fn, the single handle
;; threaded into rope construction.
;;
;;   (summary str)         = (measure str)
;;   (summary a b c ...)   = combine, folded left-to-right (order matters; a
;;                           monoid is associative but not commutative)
;;
;; Arguments interleave: strings are measured, ropes contribute their cached
;; summary (guarded same-summary), summaries pass through. Identity is
;; (summary "") -- no separate empty (relies on measure being a homomorphism).

(define (summariser measure combine)
  (define (summary . parts)
    (define (->s x)
      (cond
        [(string? x) (measure x)]
        [(rope? x)
         (if (eq? (rope-algebra x) summary)
             (rope-summary x)
             (error 'summary
                    "rope was summarised under a different summary; reconstruction unsupported"))]
        [else x]))
    (when (null? parts)
      (error 'summary "needs at least one argument"))
    (foldl (lambda (x acc) (combine acc (->s x)))
           (->s (car parts))
           (cdr parts)))
  summary)

;; ---------- leaf / piece helpers (internal) ----------

(define ((leaf-rope smr) text)
  (leaf text (smr text) smr))

(define (make-leaf-range smr text start end)
  (if (= start end)
      (empty-rope smr)
      (leaf-range text start end (smr (substring text start end)) smr)))

(define (empty-rope smr)
  (leaf "" (smr "") smr))

(define (empty-rope? r)
  (and (leaf? r) (string=? "" (leaf-text r))))

(define (piece-text piece)
  (match piece
    [(leaf text _ _)           text]
    [(leaf-range text s e _ _) (substring text s e)]))

(define (leaf-piece-bounds piece)
  (match piece
    [(leaf text _ _)           (values text 0 (string-length text))]
    [(leaf-range text s e _ _) (values text s e)]))

(define (leaf-piece-length piece)
  (define-values (_ s e) (leaf-piece-bounds piece))
  (- e s))

;; Termination guard for descent: a rope that `bisect` cannot split further
;; (a leaf of length <= 1). Branches are never atomic.
(define (atom? r)
  (and (not (branch? r)) (<= (leaf-piece-length r) 1)))

;; Bisect a non-atomic leaf/leaf-range into two ranges over the same backing
;; string (no copy). The summary is recovered from the piece.
(define (split-leaf-piece piece)
  (define smr (rope-algebra piece))
  (define-values (text start end) (leaf-piece-bounds piece))
  (define mid (+ start (quotient (- end start) 2)))
  (values (make-leaf-range smr text start mid)
          (make-leaf-range smr text mid end)))

;; Bisect a non-atomic rope into its two halves: a branch into its children, a
;; leaf/leaf-range into two adjacent ranges over the same backing string (no
;; copy). Precondition: (not (atom? r)). The rope's one split primitive -- crude
;; and guide-free; all guided descent (in the zipper) is built on it.
(define (bisect r)
  (if (branch? r)
      (values (branch-left r) (branch-right r))
      (split-leaf-piece r)))

;; Two adjacent ranges of the same backing string re-fuse into one leaf.
(define (leaf-compatible? l r)
  (and (leaf-range? l) (leaf-range? r)
       (eq? (leaf-range-text l) (leaf-range-text r))
       (= (leaf-range-end l) (leaf-range-start r))))

;; ---------- branch / concat (internal) ----------

(define ((branch-rope smr) l r)
  (branch l r (smr l r) smr))

;; Smart joiner: drops empties, re-fuses adjacent compatible ranges, else
;; branches. This is the rope "rise"/join step.
(define ((concat-rope smr) . ropes)
  (foldr (lambda (l r)
           (cond
             [(empty-rope? l) r]
             [(empty-rope? r) l]
             [(leaf-compatible? l r)
              ((leaf-rope smr) (string-append (piece-text l) (piece-text r)))]
             [else ((branch-rope smr) l r)]))
         (empty-rope smr)
         ropes))

(define (chunk-string text n)
  (for/list ([start (in-range 0 (string-length text) n)])
    (substring text start (min (string-length text) (+ start n)))))

;; ---------- roper (rope factory) ----------
;; ((roper smr [#:chunk-size n]) . parts) assembles strings (chunked into
;; leaves) and ropes (passed through) by a dumb concat fold. Subsumes the old
;; string->rope (chunk + assemble) and concat-rope (all-ropes case). Balancing
;; is deferred -- the fold is a right-leaning spine for now.

(define ((roper smr #:chunk-size [chunk 1024]) . parts)
  (define (->rope x)
    (if (string? x)
        (apply (concat-rope smr) (map (leaf-rope smr) (chunk-string x chunk)))
        x))
  (apply (concat-rope smr) (map ->rope parts)))

;; ---------- read ----------
;; Internal walk used by the prop:custom-write handler on each node type.
;; Writes leaf bytes straight to the port; leaf-range avoids substring's copy by
;; passing its bounds to write-string. (~a r), (format "~a" r), (display r), and
;; (with-output-to-string (lambda () (display r))) all route through this.

(define (rope-write-text r port)
  (match r
    [(leaf text _ _)           (write-string text port)]
    [(leaf-range text s e _ _) (write-string text port s e)]
    [(branch l r _ _)
     (rope-write-text l port)
     (rope-write-text r port)]))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; A trivial summary: summary = character count.
  (define sum (summariser string-length +))

  ;; --- build & read ---
  (define r ((roper sum) "abcdef"))
  (check-equal? (~a r) "abcdef")
  (check-equal? (sum r) 6)                         ; rope coerced -> cached summary

  ;; chunked build still round-trips and summarises
  (define r2 ((roper sum #:chunk-size 2) "hello world"))
  (check-equal? (~a r2) "hello world")
  (check-equal? (sum r2) 11)

  ;; --- interleaving strings / ropes / summaries ---
  (check-equal? (sum "ab" r "x") (+ 2 6 1))
  (check-equal? (sum 5 r)        (+ 5 6))          ; a summary value (number) passes through
  (check-equal? (sum "")         0)                ; identity = (summary "")

  ;; assembling mixed parts into a rope
  (define joined ((roper sum) "(" r ")"))
  (check-equal? (~a joined) "(abcdef)")
  (check-equal? (sum joined) 8)

  ;; --- same-summary guard ---
  (define sum2 (summariser string-length +))       ; a different summary instance
  (check-exn exn:fail? (lambda () (sum2 r)))        ; r was built under `sum`
  )
