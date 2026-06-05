#lang racket

;; Summarised rope: a persistent tree of text that caches a user-defined summary
;; at every node. Three factories make the whole surface:
;;
;;   smr        : (string | tree | summary)* -> summary  ; built by `make-summary`
;;   make-rope  : smr -> ((string | tree)* -> tree)      ; the rope factory (rebalances)
;;   bisect     : tree -> (values tree tree)             ; the one split primitive
;;
;; A node is a `leaf` (its whole text) or a `branch` (two subtrees); both inherit
;; from `tree`, which caches what every node shares:
;;   summary -- the cached summary value (O(1) reads)
;;   algebra -- the summary fn it was built under, so `smr` can verify (by
;;              eq?) that a tree's cached value belongs to the summary folding it
;;   size    -- char length, kept automatically for balancing. Unlike `summary`
;;              it is fixed (= string-length), never user-supplied, so it is a
;;              plain node field -- off the summary entirely.
;;   height  -- node height (leaf 0, branch 1 + max child); like `size`, a plain
;;              node field for balancing -- off the summary.
;;
;; Nodes participate in Racket's display/write protocol (prop:custom-write on
;; `tree`, inherited), so (~a r), (display r), (with-output-to-string ...) all
;; yield/emit the text -- no bespoke rope->string.
;;
;; The summary fn is the single handle threaded into construction (bound as `smr`
;; at use sites). Building from strings needs it passed; ops on an existing tree
;; recover it from the node via `tree-algebra`.
;;
;; A pure rope library: make, summarise, split (`bisect`), join (`make-rope`),
;; display. Guided navigation lives in `zipper-core.rkt`, built on these.
;;
;; Design notes: discussions/2026-05-29/2-claude.md (the variadic surface),
;; discussions/2026-06-01/1-claude.md (the cleanup this file is the rewrite of).

(provide
 make-summary  ; (make-summary string-summary combine) -> smr, the variadic summary fn
 make-rope     ; (make-rope smr [#:chunk-size n]) -> the rope builder (fuses, rebalances)
 bisect)       ; the one split primitive (rough-borrowing; good-enough? optional)
;; everything else is internal: leaf-rope/branch-rope/empty-rope, concat-rope, split-leaf,
;; tree-size/tree-height, within-ratio, rebalance, pathological?/log2, chunk-string,
;; rope-write-text. Emptiness is (equal? x ((make-rope smr))): the empty branch is
;; unconstructable (branch guard), so the only size-0 rope is the canonical empty leaf.

;; ---------- nodes ----------
;; A node is a leaf (its whole text) or a branch (two subtrees). The `tree`
;; parent caches the summary value, the summary fn it was built under, char size,
;; and height; the inherited accessors tree-summary / tree-algebra / tree-size /
;; tree-height read any node, and prop:custom-write is inherited too.
(struct tree (summary algebra size height) #:transparent
  #:property prop:custom-write (lambda (r port mode) (rope-write-text r port)))
(struct leaf   tree (text)       #:transparent)   ; ctor: (leaf summary algebra size height text)
(struct branch tree (left right) #:transparent
  ;; the illegal state, made unconstructable: a branch holds two NON-empty ropes, so
  ;; the only size-0 rope is ever the canonical empty leaf.
  #:guard (lambda (summary algebra size height left right _name)
            (when (or (zero? (tree-size left)) (zero? (tree-size right)))
              (error 'branch "empty child -- branches hold two non-empty ropes"))
            (values summary algebra size height left right)))

;; max-leaf: the target leaf size -- the fuse limit AND the default chunk, so
;; chunking, splitting, and fusing all agree on how big a leaf wants to be.
(define max-leaf 1024)

;; ---------- summary ----------
;; (make-summary string-summary combine) -> smr, the variadic summary fn.
;;   (smr)            = (string-summary "")   ; the empty/identity, like ((make-rope smr))
;;   (smr str)        = (string-summary str)
;;   (smr a b c ...)  = combine, folded left-to-right (associative, not
;;                      commutative -- order is preserved)
;; Strings are measured, trees contribute their cached summary (same-algebra
;; guarded), summary values pass through. Identity is (smr "") -- no separate
;; empty (string-summary is a monoid homomorphism). Knows nothing of `size`.
(define (make-summary string-summary combine)
  (define (smr . parts)
    (define (->s x)
      (cond
        [(string? x) (string-summary x)]
        [(tree? x)
         (if (eq? (tree-algebra x) smr)
             (tree-summary x)
             (error 'smr
                    "tree was summarised under a different summary; reconstruction unsupported"))]
        [else x]))                              ; already a summary value
    (foldl (lambda (x acc) (combine acc (->s x)))
           (string-summary "")
           parts))
  smr)

;; ---------- construction ----------
;; leaf-rope / branch-rope stamp the cached summary, size, and height. empty-rope
;; is the canonical empty leaf -- the only representable empty, since concat drops
;; empties before branching, so a branch is never empty.
(define ((leaf-rope smr) text)
  (leaf (smr text) smr (string-length text) 0 text))
(define ((branch-rope smr) l r)
  (branch (smr l r) smr
          (+ (tree-size l) (tree-size r))
          (add1 (max (tree-height l) (tree-height r)))
          l r))
(define (empty-rope smr) (leaf (smr "") smr 0 0 ""))
(define (empty-rope? r)  (and (leaf? r) (zero? (tree-size r))))

;; ---------- split ----------
;; Split a leaf (size >= 2) at its char midpoint into two leaves. A half's
;; summary can't be derived from the whole (combine has no inverse), so it is
;; re-measured -- which substrings anyway, so the halves are plain copies.
(define (split-leaf lf)
  (define smr  (tree-algebra lf))
  (define text (leaf-text lf))
  (define mid  (quotient (string-length text) 2))
  (values ((leaf-rope smr) (substring text 0 mid))
          ((leaf-rope smr) (substring text mid))))

;; good-enough? makers for a split: the heavier side within a ratio of the lighter
;; (plus 1 char of slack, to ignore sub-leaf granularity). The lazy heal uses a
;; loose ratio; `rebalance` a tighter one.
(define ((within-ratio a) l r)
  (<= (max (tree-size l) (tree-size r))
      (+ (* a (min (tree-size l) (tree-size r))) 1)))
(define heal-ratio?    (within-ratio 3))   ; bisect's default -- the lazy heal
(define rebuild-ratio? (within-ratio 2))   ; rebalance -- stricter, still not perfect

;; Bisect a non-atomic node into two `good-enough?` halves. A leaf splits at its
;; char midpoint; a branch rough-borrows across its boundary -- rotating the
;; boundary child to the lighter side, [L [A B]] -> [[L A] B] (and the mirror) --
;; until the sides pass `good-enough?` or a whole-subtree move would overshoot. The
;; concat invariant l ++ r = t holds throughout, so only the boundary branches are
;; rebuilt; every other subtree and every leaf is reused. All guided descent (in the
;; zipper) builds on this, healing as it goes. Total: a leaf under size 2 splits into
;; an empty rope plus the rest, so the empty rope bisects to two empties.
(define (bisect t [good-enough? heal-ratio?])
  (if (leaf? t)
      (split-leaf t)
      (let ([smr (tree-algebra t)])
        (define br (branch-rope smr))
        (define (w x) (tree-size x))
        (let loop ([l (branch-left t)] [r (branch-right t)])
          (define gap (- (w r) (w l)))                          ; >0 right-heavy, <0 left-heavy
          (cond
            [(good-enough? l r) (values l r)]
            [(and (positive? gap) (branch? r) (< (w (branch-left r))  gap))      ; borrow r->l
             (loop (br l (branch-left r)) (branch-right r))]
            [(and (negative? gap) (branch? l) (< (w (branch-right l)) (- gap)))  ; borrow l->r
             (loop (branch-left l) (br (branch-right l) r))]
            [else (values l r)])))))                            ; coarse boundary -- accept rough split

;; rebalance: rebuild t to a tighter balance by recursively bisecting with the
;; stricter `rebuild-ratio?`. Leaves are reused (never bisected); only branches are
;; rebuilt -- O(leaves * log) -- so it resets accumulated shape debt. The pathology
;; tier: a fresh load or a node that drifted too tall is run through it once.
(define (rebalance t)
  (if (leaf? t)
      t
      (let-values ([(l r) (bisect t rebuild-ratio?)])
        ((branch-rope (tree-algebra t)) (rebalance l) (rebalance r)))))

;; pathological?: height too tall for weight -- the scapegoat trigger. C=3 sits just
;; above the ~2.41 a ratio-3 tree guarantees (1/log2(4/3)), leaving the lazy heal
;; slack before a rebuild is forced; K=2 is constant slack for small trees.
(define (log2 n) (/ (log n) (log 2)))
(define (pathological? t)
  (> (tree-height t) (+ (* 3 (log2 (add1 (tree-size t)))) 2)))

;; ---------- join ----------
;; concat-rope: variadic join, folding the binary `join`. `join` drops empties,
;; fuses the seam when the two boundary leaves fit one leaf, else branches. It's the
;; inverse of bisect's split-leaf: descent splits a leaf, the rise's joins fuse it
;; back. To reach the seam it descends a boundary edge only through SMALL children
;; (< max-leaf) -- the bounded leaf tip that splitting leaves -- and stops at any real
;; subtree, so it stays O(1)-ish, never O(depth). Balance-dumb: shape is bisect's job.
(define ((concat-rope smr) . ropes)
  (define (fuse l r) ((leaf-rope smr) (string-append (leaf-text l) (leaf-text r))))
  (define (join l r)
    (cond
      [(empty-rope? l) r]
      [(empty-rope? r) l]
      [(and (leaf? l) (leaf? r))
       (if (<= (+ (tree-size l) (tree-size r)) max-leaf)
           (fuse l r)
           ((branch-rope smr) l r))]
      [(and (branch? l) (< (tree-size (branch-right l)) max-leaf))   ; small right tip of l
       ((branch-rope smr) (branch-left l) (join (branch-right l) r))]
      [(and (branch? r) (< (tree-size (branch-left r)) max-leaf))    ; small left tip of r
       ((branch-rope smr) (join l (branch-left r)) (branch-right r))]
      [else ((branch-rope smr) l r)]))
  (foldr join (empty-rope smr) ropes))

(define (chunk-string text n)
  (for/list ([start (in-range 0 (string-length text) n)])
    (substring text start (min (string-length text) (+ start n)))))

;; ---------- rope (factory) ----------
;; ((make-rope smr [#:chunk-size n]) . parts) assembles strings (chunked into leaves) and
;; trees (passed through) by a dumb concat fold, then `rebalance`s the result if it
;; came out pathologically tall (a fresh load folds to a right-leaning spine).
(define ((make-rope smr #:chunk-size [chunk max-leaf]) . parts)
  (define (->rope x)
    (if (string? x)
        (apply (concat-rope smr) (map (leaf-rope smr) (chunk-string x chunk)))
        x))
  (define t (apply (concat-rope smr) (map ->rope parts)))
  (if (pathological? t) (rebalance t) t))

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
  (define sum (make-summary string-length +))

  ;; an explicit right-leaning spine of one-char leaves -- bypasses concat's fuse, so
  ;; bisect/rebalance get a genuinely unbalanced tree to chew on.
  (define (spine str)
    (let loop ([cs (string->list str)])
      (if (null? (cdr cs))
          ((leaf-rope sum) (string (car cs)))
          ((branch-rope sum) ((leaf-rope sum) (string (car cs))) (loop (cdr cs))))))

  ;; --- build & read ---
  (define r ((make-rope sum) "abcdef"))
  (check-equal? (~a r) "abcdef")
  (check-equal? (sum r) 6)                          ; tree coerced -> cached summary

  ;; chunked build still round-trips and summarises
  (define r2 ((make-rope sum #:chunk-size 2) "hello world"))
  (check-equal? (~a r2) "hello world")
  (check-equal? (sum r2) 11)

  ;; --- interleaving strings / trees / summaries ---
  (check-equal? (sum "ab" r "x") (+ 2 6 1))
  (check-equal? (sum 5 r)        (+ 5 6))           ; a summary value passes through
  (check-equal? (sum "")         0)                 ; identity = (smr "")
  (check-equal? (sum)            0)                 ; (smr) = (string-summary "") -- empty, like ((make-rope smr))

  ;; assembling mixed parts into a rope
  (define joined ((make-rope sum) "(" r ")"))
  (check-equal? (~a joined) "(abcdef)")
  (check-equal? (sum joined) 8)

  ;; --- same-summary guard ---
  (define sum2 (make-summary string-length +))            ; a different summary instance
  (check-exn exn:fail? (lambda () (sum2 r)))         ; r was built under `sum`

  ;; --- bisect round-trips text ---
  (define-values (l rr) (bisect r))
  (check-equal? (string-append (~a l) (~a rr)) "abcdef")

  ;; --- bisect is total: the empty rope splits into two empties ---
  (let-values ([(a b) (bisect ((make-rope sum)))])
    (check-true (equal? a ((make-rope sum))))
    (check-true (equal? b ((make-rope sum)))))

  ;; --- suppose an empty IS produced (bisecting an atom): concat reabsorbs it, never
  ;;     branching it -- the empty-drop runs before any branch in `join` ---
  (let-values ([(lh rh) (bisect ((make-rope sum) "x"))])   ; an atom -> one half is empty
    (check-equal? (~a ((concat-rope sum) lh rh)) "x"))
  (let ([e ((make-rope sum))] [ab ((make-rope sum) "ab")])
    (check-true   (equal? ((concat-rope sum) e e) e))     ; empties only -> empty
    (check-equal? (~a ((concat-rope sum) e ab e)) "ab"))  ; empties around content -> dropped

  ;; --- bisect rough-borrows a spine toward weight-even (within ratio 3) halves ---
  (let-values ([(sl sr) (bisect (spine "abcdefgh"))])
    (check-equal? (string-append (~a sl) (~a sr)) "abcdefgh")          ; content preserved
    (check-true (<= (max (tree-size sl) (tree-size sr))                ; within ratio
                    (+ (* 3 (min (tree-size sl) (tree-size sr))) 1))))

  ;; --- rope rebalances a load that folds to a pathological spine ---
  (define big ((make-rope sum) (make-string 65536 #\x)))   ; 64 max-leaf chunks -> a tall spine
  (check-equal? (~a big) (make-string 65536 #\x))                  ; content intact
  (check-false (pathological? big))                               ; came out balanced, not a spine

  ;; --- rebalance turns a pathological spine into a non-pathological tree ---
  (let ([sp (spine "abcdefghijklmnop")])             ; 16-leaf right spine
    (check-true   (pathological? sp))                 ; tall for its weight
    (define b (rebalance sp))
    (check-false  (pathological? b))                  ; now balanced
    (check-equal?  (~a b) "abcdefghijklmnop"))        ; content preserved

  ;; --- concat fuses small adjacent leaves into one ---
  (let ([j ((concat-rope sum) ((make-rope sum) "ab") ((make-rope sum) "cd"))])
    (check-true   (leaf? j))                                          ; one leaf, not a branch
    (check-equal? (~a j) "abcd"))

  ;; --- seam-fuse: a small remainder split across a big subtree recombines (not scatters) ---
  (let* ([lf   (leaf-rope sum)]
         [br   (branch-rope sum)]
         [tree ((make-rope sum) (make-string 3000 #\z))]                   ; a real multi-leaf tree
         [frag (br (lf "c") (br tree (lf "d")))]                      ; "c" stranded at the tree's left
         [whole ((concat-rope sum) (lf "ab") frag)])
    (define (leftmost t) (if (leaf? t) t (leftmost (branch-left t))))
    (check-equal? (~a whole) (string-append "abc" (make-string 3000 #\z) "d"))  ; content
    (check-equal? (leaf-text (leftmost whole)) "abc"))                          ; "ab"+"c" fused

  ;; --- a chunk-1 build coalesces into max-leaf leaves, not a spine of singletons ---
  (let ([t ((make-rope sum #:chunk-size 1) (make-string 3000 #\a))])
    (define (leaf-count t) (if (leaf? t) 1 (+ (leaf-count (branch-left t)) (leaf-count (branch-right t)))))
    (check-equal? (~a t) (make-string 3000 #\a))
    (check-true   (<= (leaf-count t) 8)))                            ; ~3 leaves, not 3000

  ;; --- size is tracked on every node, off the summary ---
  (check-equal? (tree-size r)  6)
  (check-equal? (tree-size r2) 11))
