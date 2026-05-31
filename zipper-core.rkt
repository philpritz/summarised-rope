#lang racket

;; Zipper: structured navigation and editing over a summarised rope.
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
;;
;; This is where guides live. `rope-core.rkt` is guide-free -- it only knows how
;; to bisect / roper / summarise; everything that reads a guide (descent, ascent,
;; segment carving) is here, built on the rope's `bisect`.

(require racket/match
         "rope-core.rkt")

(provide
 start
 navigate
 select-seg
 with-guide
 to-root
 edit-head
 insert
 delete
 text
 at-gap?
 at-root?)

(struct head (before rope after) #:transparent)
(struct zipper (guide head crumbs) #:transparent)

;; ---------- helpers (operate on unpacked head / crumbs) ----------

;; arrange: focus on `m`, with `ls`/`rs` ropes stashed either side. Returns the
;; new head -- anchors extended by the stashed summaries -- and the `put` that
;; undoes it, rebuilding the parent focus as roper(ls, <focus>, rs) with parent
;; anchors restored. Empty stashes vanish under roper, so this serves all three
;; arrangements (left / right / gap). A crumb is exactly such a put.
(define (arrange smr b ls m rs a)
  (values (head (smr b ls) m (smr rs a))
          (lambda (h*) (head b ((roper smr) ls (head-rope h*) rs) a))))

;; pick: curried over the guide. Bisect once, read the guide at the L|R boundary,
;; and arrange the bisection accordingly -- returning the descent step
;; (head', put). The three guide-free arrangements are inlined; nothing else
;; names them. Precondition: (not (atom? (head-rope h))).
(define ((pick guide) h)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (define mt  (empty-rope smr))
  (define-values (L R) (bisect t))
  (case (guide (smr b L) (smr R a))
    [(-1) (arrange smr b mt L R a)]    ; split-left  : (.., L, R)
    [(1)  (arrange smr b L R mt a)]    ; split-right : (L, R, ..)
    [(0)  (arrange smr b L mt R a)]    ; split-gap   : (L, .., R)
    [else (error 'pick "guide must return -1, 0, or 1")]))

;; descender: step down until the focus can't split -- the guide stopped in a gap
;; (empty focus), or it drilled to a single element (atom). Either way the
;; resting focus is where an edit applies. Each step pushes a crumb.
(define ((descender guide) h crumbs)
  (define step (pick guide))
  (let loop ([h h] [crumbs crumbs])
    (if (atom? (head-rope h))
        (values h crumbs)
        (let-values ([(h* put) (step h)])
          (loop h* (cons put crumbs))))))

;; split-at: descend `t` to the exact boundary the guide marks, returning the two
;; sides as ropes. The refined cut -- recursive bisect + guide, built only on the
;; rope's crude `bisect`. `b`/`a` are the surrounding-context summaries.
(define ((split-at guide) b t a)
  (define smr (rope-algebra t))
  (let walk ([b b] [t t] [a a])
    (cond
      [(atom? t)
       (if (positive? (guide b (smr t a)))
           (values t (empty-rope smr))
           (values (empty-rope smr) t))]
      [else
       (define-values (L R) (bisect t))
       (case (guide (smr b L) (smr R a))
         [(-1) (let-values ([(ll lr) (walk b L (smr R a))])
                 (values ll ((roper smr) lr R)))]
         [(1)  (let-values ([(rl rr) (walk (smr b L) R a)])
                 (values ((roper smr) L rl) rr))]
         [(0)  (values L R)]
         [else (error 'split-at "guide must return -1, 0, or 1")])])))

;; carve: the segment between a seg-guide's two boundaries, as (l, m, r) ropes.
;; Two split-at passes: the -1 cut finds the left edge, the +1 the right. A
;; seg-guide is 5-valued -- sgn(a-left)+sgn(b-left) -- offset to a 3-valued
;; boundary guide for each edge.
(define ((carve seg-guide) b t a)
  (define smr (rope-algebra t))
  (define ((bound off) sl sr) (sgn (+ (seg-guide sl sr) off)))
  (define-values (l rest) ((split-at (bound -1)) b t a))
  (define-values (m r)    ((split-at (bound 1)) (smr b l) rest a))
  (values l m r))

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

;; select-seg: focus the seg-guide's segment as the head's middle, stashing the
;; left/right context into one crumb. carve at the current focus, then arrange the
;; three-way (a non-empty middle is a segment, an empty one a gap). The window must
;; lie within the current focus; to-root or ascend first if it doesn't.
(define (select-seg z seg-guide)
  (match-define (zipper guide h crumbs) z)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (define-values (l m r) ((carve seg-guide) b t a))
  (let-values ([(h* put) (arrange smr b l m r a)])
    (zipper guide h* (cons put crumbs))))

;; text: the whole document as a string, via the root focus and the print protocol.
(define (text z) (~a (head-rope (zipper-head (to-root z)))))

(define (at-gap?  z) (empty-rope? (head-rope (zipper-head z))))
(define (at-root? z) (null? (zipper-crumbs z)))

;; ============================================================================
(module+ test
  (require rackunit)
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
    (check-equal? (text (insert ze "hi")) "hi")

    ;; --- split-at: the refined single-boundary cut, built on bisect ---
    (define r ((roper sum) "abcdef"))
    (define e (sum ""))
    (let-values ([(l rr) ((split-at (at 3)) e r e)])
      (check-equal? (~a l) "abc")    (check-equal? (~a rr) "def"))
    (let-values ([(l rr) ((split-at (at 0)) e r e)])
      (check-equal? (~a l) "")       (check-equal? (~a rr) "abcdef"))
    (let-values ([(l rr) ((split-at (at 6)) e r e)])
      (check-equal? (~a l) "abcdef") (check-equal? (~a rr) ""))
    (let-values ([(l rr) ((split-at (at 5)) e ((roper sum #:chunk-size 2) "hello world") e)])
      (check-equal? (~a l) "hello")  (check-equal? (~a rr) " world"))

    ;; --- carve: select a window [a, b) -- two split-at passes ---
    (define ((seg a b) left right) (+ (sgn (- a left)) (sgn (- b left))))
    (let-values ([(l m rr) ((carve (seg 2 5)) e r e)])
      (check-equal? (~a l) "ab") (check-equal? (~a m) "cde") (check-equal? (~a rr) "f"))

    ;; --- select-seg: focus a span; the span is the focus, delete removes it ---
    (define zs (select-seg ((start (at 0)) ((roper sum) "abcdef")) (seg 2 5)))
    (check-equal? (text zs) "abcdef")          ; selecting doesn't change the text
    (check-false  (at-gap? zs))                 ; focus is the span "cde"
    (check-equal? (text (delete zs)) "abf")     ; delete removes the span
    (check-equal? (text (insert zs "X")) "abXf")))
