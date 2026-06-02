#lang racket

;; Summarised rope: a persistent tree of text that caches a user-defined summary
;; at every node. Three factories make the whole surface:
;;
;;   summary : (string | tree | summary)* -> summary   ; built by `summariser`
;;   rope    : (string | tree)*           -> tree       ; built by `roper`
;;   bisect  : tree -> (values tree tree)               ; the one split primitive
;;
;; A node is a `leaf` (its whole text) or a `branch` (two subtrees); both inherit
;; from `tree`, which caches what every node shares:
;;   summary -- the cached summary value (O(1) reads)
;;   algebra -- the summary fn it was built under, so `summary` can verify (by
;;              eq?) that a tree's cached value belongs to the summary folding it
;;   size    -- char length, kept automatically for balancing. Unlike `summary`
;;              it is fixed (= string-length), never user-supplied, so it is a
;;              plain node field -- off the summary entirely.
;;
;; Nodes participate in Racket's display/write protocol (prop:custom-write on
;; `tree`, inherited), so (~a r), (display r), (with-output-to-string ...) all
;; yield/emit the text -- no bespoke rope->string.
;;
;; The summary fn is the single handle threaded into construction (bound as `smr`
;; at use sites). Building from strings needs it passed; ops on an existing tree
;; recover it from the node via `tree-algebra`.
;;
;; A pure rope library: make, summarise, split (`bisect`), join (`roper`),
;; display. Guided navigation lives in `zipper-core.rkt`, built on these.
;;
;; Design notes: discussions/2026-05-29/2-claude.md (the variadic surface),
;; discussions/2026-06-01/1-claude.md (the cleanup this file is the rewrite of).

(provide
 summariser      ; (summariser measure combine) -> the variadic `summary` fn
 roper           ; (roper smr [#:chunk-size n]) -> the rope builder
 bisect          ; the one split primitive
 atom?           ; descent termination guard (kept pending a total `bisect`)
 tree-size)      ; char length of a node -- O(1); the zipper's smr-free empty check

;; ---------- nodes ----------
;; A node is a leaf (its whole text) or a branch (two subtrees). The `tree`
;; parent caches the summary value, the summary fn it was built under, and char
;; size; the inherited accessors tree-summary / tree-algebra / tree-size read any
;; node, and prop:custom-write is inherited too.
(struct tree (summary algebra size) #:transparent
  #:property prop:custom-write (lambda (r port mode) (rope-write-text r port)))
(struct leaf   tree (text)       #:transparent)   ; ctor: (leaf summary algebra size text)
(struct branch tree (left right) #:transparent)

;; ---------- summariser ----------
;; (summariser measure combine) -> the variadic `summary` fn.
;;   (summary str)        = (measure str)
;;   (summary a b c ...)  = combine, folded left-to-right (associative, not
;;                          commutative -- order is preserved)
;; Strings are measured, trees contribute their cached summary (same-algebra
;; guarded), summary values pass through. Identity is (summary "") -- no separate
;; empty (measure is a monoid homomorphism). Knows nothing of `size`.
(define (summariser measure combine)
  (define (summary . parts)
    (define (->s x)
      (cond
        [(string? x) (measure x)]
        [(tree? x)
         (if (eq? (tree-algebra x) summary)
             (tree-summary x)
             (error 'summary
                    "tree was summarised under a different summary; reconstruction unsupported"))]
        [else x]))                              ; already a summary value
    (when (null? parts) (error 'summary "needs at least one argument"))
    (foldl (lambda (x acc) (combine acc (->s x)))
           (->s (car parts))
           (cdr parts)))
  summary)

;; ---------- construction ----------
;; leaf-rope / branch-rope stamp the cached summary and size. empty-rope is the
;; canonical empty leaf -- the only representable empty, since concat drops
;; empties before branching, so a branch is never empty.
(define ((leaf-rope smr) text)
  (leaf (smr text) smr (string-length text) text))
(define ((branch-rope smr) l r)
  (branch (smr l r) smr (+ (tree-size l) (tree-size r)) l r))
(define (empty-rope smr) (leaf (smr "") smr 0 ""))
(define (empty-rope? r)  (and (leaf? r) (zero? (tree-size r))))

;; ---------- split ----------
;; atom?: a node `bisect` cannot split further (a leaf of size <= 1). Branches
;; are never atomic. The descent termination guard.
(define (atom? r) (and (leaf? r) (<= (tree-size r) 1)))

;; Split a leaf (size >= 2) at its char midpoint into two leaves. A half's
;; summary can't be derived from the whole (combine has no inverse), so it is
;; re-measured -- which substrings anyway, so the halves are plain copies.
(define (split-leaf lf)
  (define smr  (tree-algebra lf))
  (define text (leaf-text lf))
  (define mid  (quotient (string-length text) 2))
  (values ((leaf-rope smr) (substring text 0 mid))
          ((leaf-rope smr) (substring text mid))))

;; Bisect a non-atomic node into two halves: a branch into its children, a leaf
;; into two adjacent pieces. The one split primitive -- crude and guide-free; all
;; guided descent (in the zipper) builds on it. Precondition: (not (atom? r)).
(define (bisect r)
  (if (branch? r)
      (values (branch-left r) (branch-right r))
      (split-leaf r)))

;; ---------- join ----------
;; Smart joiner: drops empties, else branches (with leaf-range gone there are no
;; adjacent views to re-fuse). The rope "rise"/join step.
(define ((concat-rope smr) . ropes)
  (foldr (lambda (l r)
           (cond
             [(empty-rope? l) r]
             [(empty-rope? r) l]
             [else ((branch-rope smr) l r)]))
         (empty-rope smr)
         ropes))

(define (chunk-string text n)
  (for/list ([start (in-range 0 (string-length text) n)])
    (substring text start (min (string-length text) (+ start n)))))

;; ---------- roper (rope factory) ----------
;; ((roper smr [#:chunk-size n]) . parts) assembles strings (chunked into leaves)
;; and trees (passed through) by a dumb concat fold. Balancing is deferred -- the
;; fold is a right-leaning spine for now.
(define ((roper smr #:chunk-size [chunk 1024]) . parts)
  (define (->rope x)
    (if (string? x)
        (apply (concat-rope smr) (map (leaf-rope smr) (chunk-string x chunk)))
        x))
  (apply (concat-rope smr) (map ->rope parts)))

;; ---------- display ----------
;; The walk behind prop:custom-write on `tree`. (~a r), (display r), and
;; (with-output-to-string (lambda () (display r))) all route through here.
(define (rope-write-text r port)
  (cond
    [(leaf? r)   (write-string (leaf-text r) port)]
    [(branch? r) (rope-write-text (branch-left r) port)
                 (rope-write-text (branch-right r) port)]))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; A trivial summary: character count.
  (define sum (summariser string-length +))

  ;; --- build & read ---
  (define r ((roper sum) "abcdef"))
  (check-equal? (~a r) "abcdef")
  (check-equal? (sum r) 6)                          ; tree coerced -> cached summary

  ;; chunked build still round-trips and summarises
  (define r2 ((roper sum #:chunk-size 2) "hello world"))
  (check-equal? (~a r2) "hello world")
  (check-equal? (sum r2) 11)

  ;; --- interleaving strings / trees / summaries ---
  (check-equal? (sum "ab" r "x") (+ 2 6 1))
  (check-equal? (sum 5 r)        (+ 5 6))           ; a summary value passes through
  (check-equal? (sum "")         0)                 ; identity = (summary "")

  ;; assembling mixed parts into a rope
  (define joined ((roper sum) "(" r ")"))
  (check-equal? (~a joined) "(abcdef)")
  (check-equal? (sum joined) 8)

  ;; --- same-summary guard ---
  (define sum2 (summariser string-length +))        ; a different summary instance
  (check-exn exn:fail? (lambda () (sum2 r)))         ; r was built under `sum`

  ;; --- bisect round-trips text and bottoms out at atoms ---
  (define-values (l rr) (bisect r))
  (check-equal? (string-append (~a l) (~a rr)) "abcdef")
  (check-true  (atom? ((roper sum) "x")))            ; size 1
  (check-true  (atom? ((roper sum) "")))             ; size 0
  (check-false (atom? r))                            ; size 6

  ;; --- size is tracked on every node, off the summary ---
  (check-equal? (tree-size r)  6)
  (check-equal? (tree-size r2) 11))
