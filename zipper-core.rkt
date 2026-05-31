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
 nav
 gap-mode
 seg-mode
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

;; The zipper's "guide" slot holds a `nav`: a two-mode switch over a point guide
;; (gap -- drives `descender`) and a span guide (seg -- drives `carve`). The live
;; `mode` says which one navigates; edits flip it -- delete -> gap (collapsed to a
;; point), insert -> seg (a span).
(struct nav (gap seg mode) #:transparent)   ; mode is 'gap or 'seg

(define (to-gap n) (struct-copy nav n [mode 'gap]))
(define (to-seg n) (struct-copy nav n [mode 'seg]))
(define (gap-mode z) (struct-copy zipper z [guide (to-gap (zipper-guide z))]))
(define (seg-mode z) (struct-copy zipper z [guide (to-seg (zipper-guide z))]))

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

;; start: a zipper rooted on the whole document, focus = the entire rope. Takes a
;; `nav`; a bare guide is shorthand for a gap-mode nav (no seg guide).
(define ((start g) rope)
  (define n (if (nav? g) g (nav g #f 'gap)))
  (define smr (rope-algebra rope))
  (zipper n (head (smr "") rope (smr "")) '()))

;; navigate: re-aim at the live mode's target. gap mode -- ascend until the focus
;; contains the point, then descend to it. seg mode -- carve the span out of the
;; whole document. The nav is carried through unchanged.
(define (navigate z)
  (match-define (zipper n h crumbs) z)
  (case (nav-mode n)
    [(gap)
     (let*-values ([(h1 c1) ((ascender (nav-gap n)) h crumbs)]
                   [(h2 c2) ((descender (nav-gap n)) h1 c1)])
       (zipper n h2 c2))]
    [(seg)
     (match-define (zipper _ rh rc) (to-root z))
     (match-define (head b t a) rh)
     (define smr (rope-algebra t))
     (define-values (l m r) ((carve (nav-seg n)) b t a))
     (let-values ([(h* put) (arrange smr b l m r a)])
       (zipper n h* (cons put rc)))]
    [else (error 'navigate "nav mode must be 'gap or 'seg")]))

;; with-guide: replace the point (gap) guide in place. Crumbs are guide-agnostic,
;; so the next gap-mode navigate re-ascends and re-descends under the new target.
(define (with-guide z g)
  (struct-copy zipper z [guide (struct-copy nav (zipper-guide z) [gap g])]))

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

;; insert / delete: edit the focus, then flip the mode to match what it now is.
;; insert puts content in -- the result is a span -> seg mode (the inserted text
;; is the selection). delete empties the focus (roper drops it on rebuild) -> gap
;; mode (a point where the edit was). The switch keeps the focus and the live
;; guide in harmony without re-navigating.
(define (insert z content) (seg-mode (edit-head z (lambda (t) content))))
(define (delete z)         (gap-mode (edit-head z (lambda (t) ""))))

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

;; ============================================================================
;; A runnable example: `racket zipper-core.rkt`. A char-count summary, a point
;; (gap) guide "at offset k", and a span (seg) guide "window [a, b)". The cursor
;; is shown inline -- | marks a gap, [..] a selected segment.
(module+ main
  (define sum (summariser string-length +))
  (define ((at k)       l r) (cond [(> l k) -1] [(< l k) 1] [else 0]))   ; point at k
  (define ((window a b) l r) (+ (sgn (- a l)) (sgn (- b l))))            ; span [a, b)

  (define (show tag z)
    (define h     (zipper-head z))
    (define off   (head-before h))         ; chars to the left of the focus
    (define foc   (~a (head-rope h)))
    (define whole (text z))
    (printf "~a~a\n"
            (~a tag #:min-width 20)
            (if (at-gap? z)
                (string-append (substring whole 0 off) "|" (substring whole off))
                (string-append (substring whole 0 off) "[" foc "]"
                               (substring whole (+ off (string-length foc)))))))

  (define doc ((roper sum) "hello world"))
  (define z0 ((start (nav (at 6) (window 0 5) 'gap)) doc))
  (show "start (whole doc):" z0)

  (define z1 (navigate z0))                ; gap mode: descend to the point at 6
  (show "navigate to 6:" z1)

  (define z2 (insert z1 "brave "))         ; insert -> seg mode, inserted span selected
  (show "insert \"brave \":" z2)

  (define z3 (delete z2))                  ; delete the selection -> gap mode
  (show "delete:" z3)

  (define z4 (navigate (seg-mode z3)))     ; seg mode: select the window [0, 5)
  (show "select [0,5):" z4)

  (define z5 (delete z4))                  ; delete the selection
  (show "delete selection:" z5)

  (define z6 (insert z5 "HEY"))            ; insert at the gap -> selected
  (show "insert \"HEY\":" z6)

  (define z7 (navigate (with-guide (gap-mode z6) (at 3))))  ; back to a point at 3
  (show "navigate to 3:" z7))
