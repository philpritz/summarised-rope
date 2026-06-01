#lang racket

;; Zipper: structured navigation and editing over a summarised rope.
;;
;; A focus is a `head` -- a sub-rope `t` plus the *summaries* of everything to
;; its left (`before`) and right (`after`) in the document. Descent bisects the
;; focus and steps into one half (or stops between them); the displaced sibling
;; is stashed in a `crumb` -- a repair closure `head -> parent-head` -- so rising
;; is pop-and-apply.
;;
;; The cursor has two states (a Vim-style move / edit split):
;;
;;   move-state : the zipper's `span` is #f. The cursor is a *gap*, navigated by
;;                a single-coordinate `guide` (cheap; only the from-the-left
;;                coordinate). Movement never needs the right anchor.
;;   seg-state  : `span` is a *seg-index* (both-ends). The cursor is a span,
;;                carved by the guide. Editing happens here: anchoring both sides
;;                is what makes an edit safe (it can't drift the cursor).
;;
;; `to-seg` is the explicit toggle ("plant"): it reads the right summary at the
;; gap and resolves the single coordinate into a both-ends seg-index. Only then
;; is `insert`/`delete` safe. The seg-index is stable across edits: re-carving it
;; against the live rope is what makes insert cover exactly what you typed and
;; delete leave a gap at the hole (so delete-then-reinsert round-trips).
;;
;; A `guide` is a callable struct (prop:procedure = its movement face), so the
;; descent machinery here treats it as a plain (before after) -> sign function
;; and never sees its `resolve`/`carve` faces. The concrete guides (char, sexp,
;; ...) live in `summaries.rkt`; this file is summary-agnostic.

(require racket/match
         "rope-core.rkt")

(provide
 ;; the guide: a callable cursor spec (prop:procedure = movement)
 (struct-out guide)
 ;; generic guide pieces -- concrete guides (in summaries) build on these
 point copoint local-span carve2 split-at
 ;; index edits
 with-index move-index
 ;; ops
 start navigate to-seg select insert delete to-root edit-head text
 at-gap? at-seg? at-root?
 ;; cursor inspection (for rendering / clients)
 zipper-guide zipper-span zipper-head head-before head-rope head-after)

(struct head (before rope after) #:transparent)
;; span: #f in move-state (a gap, navigated by the guide); a seg-index in
;; seg-state (planted -- carved by the guide's carve).
(struct zipper (guide span head crumbs) #:transparent)

;; A guide bundles one summary dimension's cursor logic:
;;   index   : the gap address it navigates to (move-state)
;;   move    : index -> (before after) -> {-1,0,+1}   ; navigate to the gap
;;   resolve : zipper-at-gap -> seg-index              ; plant: gap -> both-ends seg
;;   carve   : seg-index -> (b t a) -> (l m r)         ; carve the span (editing)
;; prop:procedure exposes `move` so the descent machinery calls a guide as a
;; plain (before after) -> sign function; it never sees resolve/carve.
(struct guide (index move resolve carve) #:transparent
  #:property prop:procedure
  (lambda (g before after) (((guide-move g) (guide-index g)) before after)))

(define (with-index g i) (struct-copy guide g [index i]))
(define (move-index g f) (struct-copy guide g [index (f (guide-index g))]))

;; ---------- generic guide pieces ----------
;; point: the left/gap edge -- the cut where `field` of the LEFT context = i.
(define ((point field) i)
  (lambda (l r) (cond [(> (field l) i) -1] [(< (field l) i) 1] [else 0])))
;; copoint: the right edge -- the cut where `field` of the RIGHT context = j,
;; the mirror of point. Reading the right context is how a seg's right edge is
;; pinned from the right (the "read it off the after-summary" of the design).
(define ((copoint field) j)
  (lambda (l r) (cond [(> (field r) j) 1] [(< (field r) j) -1] [else 0])))
;; local-span: carve the both-ends span (start, end) in `field` units, measured
;; from the frame's own start / end -- the offsets are read off the b/a it is
;; handed, so a flat metric (frame = whole doc, b/a empty) and a nested frame
;; (b/a non-empty) use the very same carver.
(define ((local-span field start end) b t a)
  (carve2 ((point field)   (+ (field b) start))
          ((copoint field) (+ (field a) end))
          b t a))

;; ---------- low-level descent / carve (guide-function based) ----------

;; arrange: focus on `m`, with `ls`/`rs` ropes stashed either side. Returns the
;; new head (anchors extended by the stashed summaries) and the `put` that undoes
;; it. Empty stashes vanish under roper, so this serves all arrangements. A crumb
;; is exactly such a put.
(define (arrange smr b ls m rs a)
  (values (head (smr b ls) m (smr rs a))
          (lambda (h*) (head b ((roper smr) ls (head-rope h*) rs) a))))

;; pick: bisect once, read the guide at the L|R boundary, arrange accordingly.
;; Precondition: (not (atom? (head-rope h))).
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

;; atom->gap: at a single element descent can't bisect further, so place it on
;; whichever side the guide points and leave an empty gap (gap-mode lands between
;; elements, never on one).
(define (atom->gap guide h)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (define mt  (empty-rope smr))
  (if (positive? (guide b (smr t a)))
      (arrange smr b t mt mt a)     ; element on the left  -> gap after it
      (arrange smr b mt mt t a)))   ; element on the right -> gap before it

;; descender: step down until the focus is a gap (empty). Each step pushes a crumb.
(define ((descender guide) h crumbs)
  (define step (pick guide))
  (let loop ([h h] [crumbs crumbs])
    (define t (head-rope h))
    (cond
      [(empty-rope? t) (values h crumbs)]
      [(atom? t) (let-values ([(h* put) (atom->gap guide h)])
                   (values h* (cons put crumbs)))]
      [else (let-values ([(h* put) (step h)])
              (loop h* (cons put crumbs)))])))

;; split-at: descend `t` to the exact boundary the guide marks, returning the two
;; sides as ropes. `b`/`a` are the surrounding-context summaries.
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

;; carve2: the span between two boundary guides -- left edge then right edge.
;; A *carver* is `(b t a) -> (values l m r)`.
(define (carve2 left-guide right-guide b t a)
  (define smr (rope-algebra t))
  (define-values (l rest) ((split-at left-guide) b t a))
  (define-values (m r)    ((split-at right-guide) (smr b l) rest a))
  (values l m r))

;; rise: pop one crumb and apply it, reconstructing the parent focus.
(define (rise h crumbs)
  (values ((car crumbs) h) (cdr crumbs)))

;; contains?: does the guide's target lie within this focus? At a gap both probes
;; collapse to (zero? (guide before after)): the gap *is* the target spot.
(define ((contains? guide) h)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (and (not (negative? (guide b (smr t a))))
       (not (positive? (guide (smr b t) a)))))

;; ascender: rise until the focus contains the target (or we reach the root).
(define ((ascender guide) h crumbs)
  (cond
    [(null? crumbs)        (values h crumbs)]
    [((contains? guide) h) (values h crumbs)]
    [else (let-values ([(h* c*) (rise h crumbs)])
            ((ascender guide) h* c*))]))

;; ---------- public ops ----------

;; start: a zipper rooted on the whole document, in move-state.
(define ((start g) rope)
  (define smr (rope-algebra rope))
  (zipper g #f (head (smr "") rope (smr "")) '()))

;; navigate: move-state. Ascend until the focus contains the gap, then descend to
;; it. Drops any span (movement is single-coordinate).
(define (navigate z)
  (match-define (zipper g _ h crumbs) z)
  (let*-values ([(h1 c1) ((ascender g) h crumbs)]
                [(h2 c2) ((descender g) h1 c1)])
    (zipper g #f h2 c2)))

;; to-root: rise to the top, leaving the whole document as a single focus.
(define (to-root z)
  (match-define (zipper g sp h crumbs) z)
  (let loop ([h h] [crumbs crumbs])
    (if (null? crumbs)
        (zipper g sp h crumbs)
        (let-values ([(h* c*) (rise h crumbs)]) (loop h* c*)))))

;; edit-head: replace the focus rope by (f focus); the repair stack rebuilds the
;; document around it. Safe because before/after are untouched.
(define (edit-head z f)
  (match-define (zipper g sp h crumbs) z)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (zipper g sp (head b ((roper smr) (f t)) a) crumbs))

;; carve-span: realign in seg-state -- re-carve the stored seg-index from the
;; root and focus the span. The seg-index never changes; this is what makes the
;; cursor track an edit (insert covers, delete leaves the hole).
(define (carve-span z)
  (match-define (zipper g sp _ _) z)
  (match-define (zipper _ _ rh rc) (to-root z))
  (match-define (head b t a) rh)
  (define smr (rope-algebra t))
  (define-values (l m r) (((guide-carve g) sp) b t a))
  (let-values ([(h* put) (arrange smr b l m r a)])
    (zipper g sp h* (cons put rc))))

;; to-seg: the explicit plant. Navigate to the gap, read the right summary, and
;; resolve the single coordinate into a both-ends seg-index. Now editable.
(define (to-seg z)
  (define zg (navigate z))
  (define seg-idx ((guide-resolve (zipper-guide zg)) zg))
  (carve-span (struct-copy zipper zg [span seg-idx])))

;; select: enter seg-state on a given seg-index directly (a selection / text
;; object), carving it as the focus.
(define (select z seg-idx)
  (carve-span (struct-copy zipper z [span seg-idx])))

;; insert: plant if at a gap, then replace the focus by `content` and realign.
;; The result is a seg over exactly what was inserted.
(define (insert z content)
  (define zs (if (zipper-span z) z (to-seg z)))
  (carve-span (edit-head zs (lambda (t) content))))

;; delete: remove the focused seg and realign -- the seg-index collapses to a gap
;; at the hole. At a gap (no span) there is nothing to delete.
(define (delete z)
  (if (zipper-span z)
      (carve-span (edit-head z (lambda (t) "")))
      z))

;; text: the whole document as a string.
(define (text z) (~a (head-rope (zipper-head (to-root z)))))

(define (at-gap?  z) (empty-rope? (head-rope (zipper-head z))))
(define (at-seg?  z) (and (zipper-span z) #t))
(define (at-root? z) (null? (zipper-crumbs z)))

;; ============================================================================
;; Tests use a trivial inline char guide (the char-count summary IS the offset,
;; so the projection is identity) -- no dependency on summaries.rkt.
(module+ test
  (require rackunit)
  (define cc (summariser string-length +))
  (define (doc s) ((roper cc) s))
  (define (focus z) (~a (head-rope (zipper-head z))))
  ;; a char guide: move = point on the count; resolve reads both anchors;
  ;; carve = local-span over the whole doc.
  (define (cg i)
    (guide i
           (lambda (idx) ((point values) idx))
           (lambda (z) (let ([h (zipper-head z)])
                         (list (head-before h) (head-after h))))
           (lambda (seg) (local-span values (first seg) (second seg)))))

  ;; --- movement lands in a gap and preserves text ---
  (for ([k (in-range 0 12)])
    (define z (navigate ((start (cg k)) (doc "hello world"))))
    (check-true  (at-gap? z))
    (check-equal? (text z) "hello world"))

  ;; --- insert at a gap covers exactly what was typed; index is stable ---
  (define z5 (navigate ((start (cg 5)) (doc "hello world"))))
  (define zi (insert z5 "XYZ"))
  (check-equal? (text zi)  "helloXYZ world")
  (check-equal? (focus zi) "XYZ")              ; the seg is exactly the insert
  (check-equal? (text z5)  "hello world")      ; navigate/insert leave z5 unmutated

  ;; --- insert at the extremes ---
  (check-equal? (text (insert (navigate ((start (cg 0))  (doc "abc"))) "Q")) "Qabc")
  (check-equal? (text (insert (navigate ((start (cg 3))  (doc "abc"))) "Z")) "abcZ")

  ;; --- a chunked, multi-leaf rope behaves the same ---
  (define hw ((roper cc #:chunk-size 2) "hello world"))
  (check-equal? (focus (insert (navigate ((start (cg 5)) hw)) ", ")) ", ")
  (check-equal? (text  (insert (navigate ((start (cg 5)) hw)) ", ")) "hello,  world")

  ;; --- empty document: the one position is a gap; insert seeds it ---
  (check-equal? (text (insert (navigate ((start (cg 0)) (doc ""))) "hi")) "hi")

  ;; --- select a range, delete it, reinsert -> round-trips exactly ---
  (define zs (select ((start (cg 0)) (doc "hello world")) (list 6 0)))
  (check-equal? (focus zs) "world")            ; [6, len-0) = "world"
  (check-equal? (text zs)  "hello world")      ; selecting doesn't change text
  (check-equal? (text (delete zs)) "hello ")   ; gap at the hole
  (check-equal? (text (insert (delete zs) "world")) "hello world")  ; reinsert restores

  ;; --- insert over a selection replaces it and covers the new text ---
  (define zr (select ((start (cg 0)) (doc "hello world")) (list 0 6)))  ; "hello"
  (check-equal? (focus zr) "hello")
  (check-equal? (text  (insert zr "hi")) "hi world")
  (check-equal? (focus (insert zr "hi")) "hi")

  ;; --- delete at a gap is a no-op (nothing selected) ---
  (check-equal? (text (delete z5)) "hello world")

  ;; --- the generic pieces directly ---
  (define e (cc ""))
  (define r (doc "abcdef"))
  (let-values ([(l rr) ((split-at ((point values) 3)) e r e)])
    (check-equal? (~a l) "abc") (check-equal? (~a rr) "def"))
  (let-values ([(l rr) ((split-at ((copoint values) 2)) e r e)])
    (check-equal? (~a l) "abcd") (check-equal? (~a rr) "ef"))   ; 2 chars on the right
  (let-values ([(l m rr) ((local-span values 2 1) e r e)])
    (check-equal? (~a l) "ab") (check-equal? (~a m) "cde") (check-equal? (~a rr) "f")))
