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
;; `summariser`, `roper`, `splitter`, `rope-splitter`, `seg-splitter`. Each takes
;; its config and returns the worker function.
;;
;; Design notes: discussions/2026-05-29/2-claude.md.

(require racket/match)

(provide
 ;; rope core
 summariser
 roper
 bisect
 splitter
 rope-splitter
 seg-splitter
 ;; zipper
 start
 navigate
 with-guide
 to-root
 edit-head
 insert
 delete
 text
 at-gap?
 at-root?)

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

;; Termination guard for descent: a rope that `splitter` cannot make progress on
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
;; copy). Precondition: (not (atom? r)). The single structural step shared by
;; `splitter` and the zipper's `cut`.
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

;; ---------- splitter: one-level guided eliminator ----------
;; Precondition: (not (atom? mr)). Does exactly one structural level: derive the
;; two children with their contexts threaded, read the guide at the split point,
;; dispatch to one handler. No recursion of its own. The summary is recovered
;; from mr.
;;
;;   ((splitter guide on-l on-r on-here) before mr after)
;;     guide   : left-total-summary right-total-summary -> -1 | 0 | 1
;;     on-l    : before L after-of-L R-sibling  -> a   ; boundary in the left child
;;     on-r    : L-sibling before-of-R R after  -> a   ; boundary in the right child
;;     on-here : L R                            -> a   ; boundary between the children

(define ((splitter guide on-l on-r on-here) before mr after)
  (define smr (rope-algebra mr))
  (define-values (L R) (bisect mr))
  (case (guide (smr before L) (smr R after))
    [(-1) (on-l before L (smr R after) R)]
    [(1)  (on-r L (smr before L) R after)]
    [(0)  (on-here L R)]
    [else (error 'splitter "guide must return -1, 0, or 1")]))

;; ---------- rope-splitter: partition at one boundary ----------
;; ((rope-splitter guide) before mr after) -> (values left right)
;; Recurses via `splitter`, reassembling the untouched sibling on the correct side.

(define ((rope-splitter guide) before mr after)
  (define smr (rope-algebra mr))
  (define (cat . rs) (apply (roper smr) rs))
  (let walk ([before before] [mr mr] [after after])
    (cond
      [(atom? mr)
       (if (positive? (guide before (smr mr after)))
           (values mr (empty-rope smr))
           (values (empty-rope smr) mr))]
      [else
       ((splitter guide
          (lambda (b L a R)
            (define-values (ll lr) (walk b L a))
            (values ll (cat lr R)))
          (lambda (L b R a)
            (define-values (rl rr) (walk b R a))
            (values (cat L rl) rr))
          (lambda (L R) (values L R)))
        before mr after)])))

;; ---------- seg-splitter: select a segment between two boundaries ----------
;; ((seg-splitter seg-guide) before mr after) -> (values left mid right)
;; Two rope-splitter passes: the -1 cut finds the left edge, the +1 cut the right.

(define ((seg-splitter seg-guide) before mr after)
  (define smr (rope-algebra mr))
  (define ((bound off) sl sr) (sgn (+ (seg-guide sl sr) off)))
  (define-values (l rest) ((rope-splitter (bound -1)) before mr after))
  (define-values (m r)    ((rope-splitter (bound 1)) (smr before l) rest after))
  (values l m r))

;; ============================================================================
;; Zipper: structured navigation and editing over a rope.
;;
;; A focus is a `head` -- a sub-rope `t` plus the *summaries* of everything to
;; its left (`before`) and right (`after`) in the document. Descent bisects the
;; focus and steps into one half (or stops between the halves); the displaced
;; sibling is stashed in a `crumb` -- a repair closure `head -> parent-head` --
;; so rising is just pop-and-apply. No bespoke crumb datatype and no lens
;; menagerie: every step is one bisection arranged three ways.
;;
;;   3-way split of t into (L, R) -- which slot the focus sits in:
;;     go-left  = (empty, L, R)   focus L, R stashed right
;;     go-right = (L, R, empty)   focus R, L stashed left
;;     gap      = (L, empty, R)   focus nothing, sit between -- the cursor
;;
;; `roper`'s empty-drop rebuilds t identically from all three, so a gap (empty
;; focus) and a segment (non-empty focus) need no separate treatment: an edit
;; just swaps the focus rope and the repair stack rebuilds the document around it.

(struct head (before rope after) #:transparent)
(struct zipper (guide head crumbs) #:transparent)

;; ---------- helpers (operate on unpacked head / crumbs) ----------

;; step: focus on `m`, with `ls`/`rs` ropes stashed either side. Returns the new
;; head -- anchors extended by the stashed summaries -- and the crumb that undoes
;; it, rebuilding the parent focus as roper(ls, <focus>, rs) with parent anchors
;; restored. Empty stashes vanish under roper, so this serves all three splits.
(define (step smr b ls m rs a)
  (values (head (smr b ls) m (smr rs a))
          (lambda (h*) (head b ((roper smr) ls (head-rope h*) rs) a))))

;; cut: one guided descent step on a non-atomic focus. Bisect once, read the
;; guide at the L|R boundary, then either descend into a child (on-descend) or
;; stop in the gap between them (on-stop). Precondition: (not (atom? (head-rope h))).
;;   on-descend, on-stop : head crumb -> _
(define ((cut guide) h on-descend on-stop)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (define mt (empty-rope smr))
  (define-values (L R) (bisect t))
  (case (guide (smr b L) (smr R a))
    [(-1) (call-with-values (lambda () (step smr b mt L R a)) on-descend)]
    [(1)  (call-with-values (lambda () (step smr b L R mt a)) on-descend)]
    [(0)  (call-with-values (lambda () (step smr b L mt R a)) on-stop)]
    [else (error 'cut "guide must return -1, 0, or 1")]))

;; descender: drive `cut` down until the guide stops in a gap, or the focus is
;; atomic (a single element -- can't bisect, so the focus lands *on* it). Either
;; way the resting focus is where an edit applies. Each descent pushes a crumb.
(define ((descender guide) h crumbs)
  (if (atom? (head-rope h))
      (values h crumbs)
      ((cut guide) h
        (lambda (h* c) ((descender guide) h* (cons c crumbs)))
        (lambda (h* c) (values h* (cons c crumbs))))))

;; rise: pop one crumb and apply it, reconstructing the parent focus. Takes no
;; guide -- the repair is purely structural.
(define (rise h crumbs)
  (values ((car crumbs) h) (cdr crumbs)))

;; contains?: does the guide's target lie within this focus? Tested off the
;; focus's own anchors -- the target is left of focus if the guide pushes left
;; past the whole thing, right of focus if it pushes right past it. At a gap
;; (empty t) both probes collapse to (zero? (guide before after)): the gap *is*
;; the target spot.
(define ((contains? guide) h)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (and (not (negative? (guide b (smr t a))))
       (not (positive? (guide (smr b t) a)))))

;; ascender: rise until the focus contains the target (or we reach the root). A
;; focus that already contains the target -- in particular a gap sitting on it --
;; is left untouched, so re-navigating to the same spot is a no-op.
(define ((ascender guide) h crumbs)
  (cond
    [(null? crumbs)        (values h crumbs)]
    [((contains? guide) h) (values h crumbs)]
    [else (let-values ([(h* c*) (rise h crumbs)])
            ((ascender guide) h* c*))]))

;; ---------- public ops (take/return a zipper; guide travels in its state) ----------

;; start: a zipper rooted on the whole document, focus = the entire rope.
(define ((start guide) rope)
  (define smr (rope-algebra rope))
  (zipper guide (head (smr "") rope (smr "")) '()))

;; navigate: re-aim at the stored guide's target -- ascend until the focus
;; contains it, then descend to the gap (or element) it points at.
(define (navigate z)
  (match-define (zipper guide h crumbs) z)
  (let*-values ([(h1 c1) ((ascender guide) h crumbs)]
                [(h2 c2) ((descender guide) h1 c1)])
    (zipper guide h2 c2)))

;; with-guide: swap the guide in place. Crumbs are guide-agnostic, so the next
;; navigate simply re-ascends and re-descends under the new target.
(define (with-guide z guide) (struct-copy zipper z [guide guide]))

;; to-root: rise to the top, leaving the whole document as a single focus.
(define (to-root z)
  (match-define (zipper guide h crumbs) z)
  (let loop ([h h] [crumbs crumbs])
    (if (null? crumbs)
        (zipper guide h crumbs)
        (let-values ([(h* c*) (rise h crumbs)]) (loop h* c*)))))

;; edit-head: replace the focus rope by (f focus); the repair stack rebuilds the
;; document around it. f : rope -> (rope | string).
(define (edit-head z f)
  (match-define (zipper guide h crumbs) z)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (zipper guide (head b ((roper smr) (f t)) a) crumbs))

;; insert / delete in terms of edit-head. At a gap (empty focus) insert is a true
;; insertion; on an element it replaces. delete empties the focus, which roper
;; drops on rebuild, leaving a gap where the element was. (Whether insert should
;; force a gap first rather than replace is left open -- see discussion notes.)
(define (insert z content) (edit-head z (lambda (t) content)))
(define (delete z)         (edit-head z (lambda (t) "")))

;; text: the whole document as a string, via the root focus and the print protocol.
(define (text z) (~a (head-rope (zipper-head (to-root z)))))

(define (at-gap?  z) (empty-rope? (head-rope (zipper-head z))))
(define (at-root? z) (null? (zipper-crumbs z)))

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

  ;; --- rope-splitter: cut at character position k ---
  (define ((at k) left right)
    (cond [(> left k) -1] [(< left k) 1] [else 0]))
  (define e (sum ""))
  (let-values ([(l rr) ((rope-splitter (at 3)) e r e)])
    (check-equal? (~a l)  "abc")
    (check-equal? (~a rr) "def"))
  (let-values ([(l rr) ((rope-splitter (at 2)) e r e)])
    (check-equal? (~a l)  "ab")
    (check-equal? (~a rr) "cdef"))
  (let-values ([(l rr) ((rope-splitter (at 0)) e r e)])
    (check-equal? (~a l)  "")
    (check-equal? (~a rr) "abcdef"))
  (let-values ([(l rr) ((rope-splitter (at 6)) e r e)])
    (check-equal? (~a l)  "abcdef")
    (check-equal? (~a rr) ""))

  ;; split on a chunked (multi-leaf) rope
  (let-values ([(l rr) ((rope-splitter (at 5)) e r2 e)])
    (check-equal? (~a l)  "hello")
    (check-equal? (~a rr) " world"))

  ;; --- seg-splitter: select window [a, b) by character position ---
  ;; seg-guide returns sgn(a-left) + sgn(b-left); (bound -1)/(bound +1) cut at a/b.
  (define ((seg a b) left right) (+ (sgn (- a left)) (sgn (- b left))))
  (let-values ([(l m rr) ((seg-splitter (seg 2 5)) e r e)])
    (check-equal? (~a l)  "ab")
    (check-equal? (~a m)  "cde")
    (check-equal? (~a rr) "f"))
  (let-values ([(l m rr) ((seg-splitter (seg 0 6)) e r e)])  ; whole thing
    (check-equal? (~a l)  "")
    (check-equal? (~a m)  "abcdef")
    (check-equal? (~a rr) ""))
  ;; Note: an *empty* selection (a = b) is a gap, not a segment. The ±1-offset
  ;; seg machinery has a 2-wide dead zone and cannot express a zero-width window;
  ;; a point cursor is rope-splitter's job, not seg-splitter's.
  )

;; ---------- zipper ----------
;; (Merged into the same `test` submodule, so a `let` keeps these names off the
;; rope-core block's.)
(module+ test
  (let ()
    (define sum (summariser string-length +))
    ;; guide for "cursor after exactly k characters": 0 exactly at offset k.
    (define ((at k) left right)
      (cond [(> left k) -1] [(< left k) 1] [else 0]))

    ;; --- start / text round-trip ---
    (define z0 ((start (at 3)) ((roper sum) "abcdef")))
    (check-equal? (text z0) "abcdef")
    (check-true  (at-root? z0))

    ;; --- navigate lands in the gap at an interior boundary; insert is a true insert ---
    (define z3 (navigate z0))
    (check-true (at-gap? z3))                          ; offset 3 is a bisection boundary
    (check-equal? (text (insert z3 "XYZ")) "abcXYZdef")
    (check-equal? (text z3) "abcdef")                  ; navigate/insert leave z3 unmutated

    ;; --- navigate is text-preserving at every offset; to-root rebuilds exactly ---
    (for ([k (in-range 0 7)])
      (define zk (navigate ((start (at k)) ((roper sum) "abcdef"))))
      (check-equal? (text zk) "abcdef")
      (check-true  (at-root? (to-root zk))))

    ;; --- at the extremes the guide drills to one element: the focus lands *on* it ---
    (define zL (navigate ((start (at 0)) ((roper sum) "abcdef"))))
    (check-false  (at-gap? zL))                         ; focused on the element "a"
    (check-equal? (text (delete zL)) "bcdef")           ; delete removes it
    (check-equal? (text (insert zL "Q")) "Qbcdef")      ; insert replaces it

    (define zR (navigate ((start (at 6)) ((roper sum) "abcdef"))))
    (check-equal? (text (delete zR)) "abcde")

    ;; --- delete at a gap removes nothing (empty focus -> empty) ---
    (check-equal? (text (delete z3)) "abcdef")

    ;; --- re-aim with a new guide: ascend to root, descend to the new target ---
    (define z5 (navigate (with-guide z3 (at 5))))
    (check-true   (at-gap? z5))
    (check-equal? (text (insert z5 "_")) "abcde_f")

    ;; --- a chunked, multi-leaf rope navigates and edits the same way ---
    (define hw ((roper sum #:chunk-size 2) "hello world"))
    (for ([k (in-range 0 12)])
      (check-equal? (text (navigate ((start (at k)) hw))) "hello world"))
    (define zc (navigate ((start (at 5)) hw)))
    (check-true   (at-gap? zc))
    (check-equal? (text (insert zc ",")) "hello, world")

    ;; --- empty document: the one position is a gap; insert seeds it ---
    (define ze (navigate ((start (at 0)) ((roper sum) ""))))
    (check-true   (at-gap? ze))
    (check-equal? (text (insert ze "hi")) "hi")))
