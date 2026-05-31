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
 ;; nav + guide construction
 nav axis run-axis point span
 ;; mode / index / guide edits
 gap-mode seg-mode with-index move with-guides with-axis
 ;; ops
 start navigate select-seg to-root edit-head insert delete text at-gap? at-root?)

(struct head (before rope after) #:transparent)
(struct zipper (guide head crumbs) #:transparent)

;; The zipper's slot holds a `nav`: index-first deciders for the two modes, a
;; *shared* index, and the live mode. `gap`/`seg` are deciders -- index -> guide
;; -- so both modes read the one index; their alignment is in their bodies. The
;; live guide is the decider applied to the index. Edits flip the mode: delete ->
;; gap (collapsed to a point), insert -> seg (a span).
(struct nav (gap seg index mode) #:transparent)   ; gap, seg : index -> guide
(define (live-gap n) ((nav-gap n) (nav-index n)))
(define (live-seg n) ((nav-seg n) (nav-index n)))

;; Guide construction: deciders over a summary projection `field` (summary ->
;; number). They share the index -- the gap is the boundary where `field` of the
;; left context reaches the index; the seg selects the unit [index, index+1) in
;; `field`'s units (so the gap is the seg's left edge -- that is their alignment).
;; A seg decider returns a *carver* `(b t a) -> (values l m r)`.
(define ((point field) i)
  (lambda (l r) (define x (field l)) (cond [(> x i) -1] [(< x i) 1] [else 0])))
(define ((span field) i)
  (lambda (l r) (define x (field l)) (+ (sgn (- i x)) (sgn (- (add1 i) x)))))
(define (span-carver field) (lambda (i) (carve ((span field) i))))
;; axis: a nav along one summary dimension -- point gap, span seg.
(define (axis field i mode) (nav (point field) (span-carver field) i mode))

;; runs: guides over maximal runs of some char class, parameterised by three
;; projections -- count (# runs strictly left), starts? (chunk begins mid-run),
;; ends? (chunk ends mid-run). The gap is before run i; the seg selects run i
;; exactly, its edges read off the flags (the old word-start / word-end shape).
(define ((runs-gap count starts? ends?) i)
  (lambda (l r)
    (cond [(< (count l) i) 1]
          [(> (count l) i) -1]
          [(ends? l)   1]      ; left ends mid-run i -> right
          [(starts? r) 0]      ; right begins run i -> here (gap before run i)
          [else        1])))   ; in the delimiters before run i -> right
(define ((runs-end count starts? ends?) i)
  (define target (add1 i))
  (lambda (l r)
    (cond [(< (count l) target) 1]
          [(> (count l) target) -1]
          [(starts? r) 1]
          [(ends? l)   0]      ; left ends run i -> here (gap after run i)
          [else       -1])))
;; run-axis: a nav along a runs dimension -- gap before run i, seg = run i exactly.
(define (run-axis count starts? ends? i mode)
  (define lg (runs-gap count starts? ends?))
  (define eg (runs-end count starts? ends?))
  (nav lg (lambda (j) (lambda (b t a) (carve2 (lg j) (eg j) b t a))) i mode))

;; Editing the nav (plain struct-copy -- no lens library): flip the mode, set or
;; move the shared index, or replace the deciders (re-aim onto another dimension).
(define (edit-nav z f) (struct-copy zipper z [guide (f (zipper-guide z))]))
(define (to-gap n) (struct-copy nav n [mode 'gap]))
(define (to-seg n) (struct-copy nav n [mode 'seg]))
(define (gap-mode z) (edit-nav z to-gap))
(define (seg-mode z) (edit-nav z to-seg))
(define (with-index z i) (edit-nav z (lambda (n) (struct-copy nav n [index i]))))
(define (move z f)       (edit-nav z (lambda (n) (struct-copy nav n [index (f (nav-index n))]))))
(define (with-guides z gap seg)
  (edit-nav z (lambda (n) (struct-copy nav n [gap gap] [seg seg]))))
;; with-axis: re-aim onto a span dimension (point gap, span seg), keeping index/mode.
(define (with-axis z field)
  (edit-nav z (lambda (n) (struct-copy nav n [gap (point field)] [seg (span-carver field)]))))

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

;; atom->gap: at a single element the descent can't bisect further, so place the
;; element on whichever side the guide points and leave an empty gap. This is the
;; explicit "switch to a gap" -- gap-mode navigation lands *between* elements,
;; never on one. (Mirrors `split-at`'s atom case.)
(define (atom->gap guide h)
  (match-define (head b t a) h)
  (define smr (rope-algebra t))
  (define mt  (empty-rope smr))
  (if (positive? (guide b (smr t a)))
      (arrange smr b t mt mt a)     ; element on the left  -> gap after it
      (arrange smr b mt mt t a)))   ; element on the right -> gap before it

;; descender: step down until the focus is a gap (empty). A bisection stops in a
;; gap when the guide returns 0; reaching a single element, `atom->gap` sets it
;; aside so the cursor still lands in a gap. Each step pushes a crumb.
(define ((descender guide) h crumbs)
  (define step (pick guide))
  (let loop ([h h] [crumbs crumbs])
    (define t (head-rope h))
    (cond
      [(empty-rope? t) (values h crumbs)]               ; a gap -- done
      [(atom? t) (let-values ([(h* put) (atom->gap guide h)])
                   (values h* (cons put crumbs)))]      ; element -> gap beside it
      [else (let-values ([(h* put) (step h)])
              (loop h* (cons put crumbs)))])))

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

;; carve2: the span between two boundary guides -- left edge then right edge, two
;; split-at passes. A *carver* is `(b t a) -> (values l m r)`.
(define (carve2 left-guide right-guide b t a)
  (define smr (rope-algebra t))
  (define-values (l rest) ((split-at left-guide) b t a))
  (define-values (m r)    ((split-at right-guide) (smr b l) rest a))
  (values l m r))

;; carve: a carver from one 5-valued seg-guide -- sgn(a-left)+sgn(b-left) offset
;; +/-1 into the two boundary guides (a symmetric window).
(define ((carve seg-guide) b t a)
  (define ((bound off) sl sr) (sgn (+ (seg-guide sl sr) off)))
  (carve2 (bound -1) (bound 1) b t a))

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
;; `nav` -- e.g. (axis field index mode).
(define ((start n) rope)
  (define smr (rope-algebra rope))
  (zipper n (head (smr "") rope (smr "")) '()))

;; navigate: re-aim at the live mode's target -- the decider applied to the shared
;; index. gap mode: ascend until the focus contains the point, then descend to it.
;; seg mode: carve the span out of the whole document. The nav rides through.
(define (navigate z)
  (match-define (zipper n h crumbs) z)
  (case (nav-mode n)
    [(gap)
     (let*-values ([(h1 c1) ((ascender (live-gap n)) h crumbs)]
                   [(h2 c2) ((descender (live-gap n)) h1 c1)])
       (zipper n h2 c2))]
    [(seg)
     (match-define (zipper _ rh rc) (to-root z))
     (match-define (head b t a) rh)
     (define smr (rope-algebra t))
     (define-values (l m r) ((live-seg n) b t a))     ; live-seg is a carver
     (let-values ([(h* put) (arrange smr b l m r a)])
       (zipper n h* (cons put rc)))]
    [else (error 'navigate "nav mode must be 'gap or 'seg")]))

;; realign: re-navigate to repair the focus/guide invariant after an edit -- the
;; cursor re-materialises against the live guide on the edited rope. Same step as
;; navigate; named for intent at the edit sites.
(define realign navigate)

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

;; insert / delete: edit the focus, flip the mode, then `realign` so the cursor
;; re-materialises against the live guide on the edited rope. insert -> seg mode,
;; so it re-selects the unit the insert landed in (e.g. with a symbol guide,
;; typing into a symbol re-selects the whole symbol). delete -> gap mode,
;; collapsing to the point where the edit was.
(define (insert z content) (realign (seg-mode (edit-head z (lambda (t) content)))))
(define (delete z)         (realign (gap-mode (edit-head z (lambda (t) "")))))

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
    (define (off s) s)                                       ; char-count projection
    (define (g i [m 'gap]) (axis off i m))                   ; a char nav at index i
    (define ((win a b) l r) (+ (sgn (- a l)) (sgn (- b l)))) ; raw window guide [a, b)

    ;; --- start / text round-trip ---
    (define z0 ((start (g 3)) ((roper sum) "abcdef")))
    (check-equal? (text z0) "abcdef")
    (check-true  (at-root? z0))

    ;; --- navigate lands in the gap at an interior boundary; insert is a true insert ---
    (define z3 (navigate z0))
    (check-true (at-gap? z3))                          ; offset 3 is a bisection boundary
    (check-equal? (text (insert z3 "XYZ")) "abcXYZdef")
    (check-equal? (text z3) "abcdef")                  ; navigate/insert leave z3 unmutated

    ;; --- navigate is text-preserving at every offset; to-root rebuilds exactly ---
    (for ([k (in-range 0 7)])
      (define zk (navigate ((start (g k)) ((roper sum) "abcdef"))))
      (check-equal? (text zk) "abcdef")
      (check-true  (at-root? (to-root zk))))

    ;; --- gap mode always lands in a gap, even at the extremes (atom->gap) ---
    (define zL (navigate ((start (g 0)) ((roper sum) "abcdef"))))
    (check-true  (at-gap? zL))                          ; a gap *before* "a", not on it
    (check-equal? (text (insert zL "Q")) "Qabcdef")     ; insert at the gap
    (check-equal? (text (delete zL)) "abcdef")          ; delete at a gap is a no-op

    (define zR (navigate ((start (g 6)) ((roper sum) "abcdef"))))
    (check-true  (at-gap? zR))                          ; a gap *after* "f"
    (check-equal? (text (insert zR "Z")) "abcdefZ")

    ;; --- delete at a gap removes nothing (empty focus -> empty) ---
    (check-equal? (text (delete z3)) "abcdef")

    ;; --- edit the shared index: set it (with-index) or move it (move) ---
    (define z5 (navigate (with-index z3 5)))
    (check-true   (at-gap? z5))
    (check-equal? (text (insert z5 "_")) "abcde_f")
    (check-equal? (text (insert (navigate (move z3 add1)) "*")) "abcd*ef")  ; 3 -> 4

    ;; --- a chunked, multi-leaf rope navigates and edits the same way ---
    (define hw ((roper sum #:chunk-size 2) "hello world"))
    (for ([k (in-range 0 12)])
      (check-equal? (text (navigate ((start (g k)) hw))) "hello world"))
    (define zc (navigate ((start (g 5)) hw)))
    (check-true   (at-gap? zc))
    (check-equal? (text (insert zc ",")) "hello, world")

    ;; --- empty document: the one position is a gap; insert seeds it ---
    (define ze (navigate ((start (g 0)) ((roper sum) ""))))
    (check-true   (at-gap? ze))
    (check-equal? (text (insert ze "hi")) "hi")

    ;; --- split-at: the refined single-boundary cut, built on bisect ---
    (define r ((roper sum) "abcdef"))
    (define e (sum ""))
    (let-values ([(l rr) ((split-at ((point off) 3)) e r e)])
      (check-equal? (~a l) "abc")    (check-equal? (~a rr) "def"))
    (let-values ([(l rr) ((split-at ((point off) 0)) e r e)])
      (check-equal? (~a l) "")       (check-equal? (~a rr) "abcdef"))
    (let-values ([(l rr) ((split-at ((point off) 6)) e r e)])
      (check-equal? (~a l) "abcdef") (check-equal? (~a rr) ""))
    (let-values ([(l rr) ((split-at ((point off) 5)) e ((roper sum #:chunk-size 2) "hello world") e)])
      (check-equal? (~a l) "hello")  (check-equal? (~a rr) " world"))

    ;; --- carve: select a window [a, b) -- two split-at passes ---
    (let-values ([(l m rr) ((carve (win 2 5)) e r e)])
      (check-equal? (~a l) "ab") (check-equal? (~a m) "cde") (check-equal? (~a rr) "f"))

    ;; --- seg-mode navigate: the span decider selects the unit [i, i+1) at the index ---
    (define zsp (navigate (seg-mode ((start (g 2)) ((roper sum) "abcdef")))))
    (check-false  (at-gap? zsp))                 ; focus is the element "c"
    (check-equal? (text (delete zsp)) "abdef")   ; delete the selected element

    ;; --- select-seg: focus a span; the span is the focus, delete removes it ---
    (define zs (select-seg ((start (g 0)) ((roper sum) "abcdef")) (win 2 5)))
    (check-equal? (text zs) "abcdef")          ; selecting doesn't change the text
    (check-false  (at-gap? zs))                 ; focus is the span "cde"
    (check-equal? (text (delete zs)) "abf")     ; delete removes the span
    (check-equal? (text (insert zs "X")) "abXf")

    ;; deleting a single element is a seg op: select [0,1) and delete it
    (check-equal? (text (delete (select-seg ((start (g 0)) ((roper sum) "abcdef"))
                                            (win 0 1))))
                  "bcdef")))

;; ============================================================================
;; A runnable example: `racket zipper-core.rkt`. The cursor is shown inline --
;; | marks a gap, [..] a selected segment. `show` projects the before-summary to
;; a character offset (off-of) and optionally annotates it (extra, e.g. depth).
(module+ main
  (require "summaries.rkt")

  (define ((show off-of [extra (lambda (b) "")]) tag z)
    (define h     (zipper-head z))
    (define b     (head-before h))
    (define o     (off-of b))
    (define foc   (~a (head-rope h)))
    (define whole (text z))
    (printf "~a~a~a\n"
            (~a tag #:min-width 20)
            (if (at-gap? z)
                (string-append (substring whole 0 o) "|" (substring whole o))
                (string-append (substring whole 0 o) "[" foc "]"
                               (substring whole (+ o (string-length foc)))))
            (extra b)))

  ;; ===== char-count: offset guides, an edit, and moving the shared index =====
  (printf "--- char-count: offset guides ---\n")
  (define shc (show values))                                   ; the summary IS the offset
  (define c0 ((start (axis values 3 'gap)) ((roper char-count) "hello world")))
  (define c1 (navigate c0))                                    (shc "navigate to 3:" c1)
  (define c2 (navigate (move c1 (lambda (i) (+ i 4)))))        (shc "move index +4:" c2)
  (define c3 (insert c2 "X"))                                  (shc "insert \"X\":" c3)
  (define c4 (delete c3))                                      (shc "delete:" c4)

  ;; ===== sexp: paren structure -- navigate by char / open paren, show depth ====
  (printf "\n--- sexp: paren structure (depth d=N) ---\n")
  (define shx (show sx-chars (lambda (b) (format "   d=~a" (sx-depth b)))))
  (define doc ((roper sexp) "(a (b c) d)"))
  (define x1 (navigate ((start (axis sx-chars 4 'gap)) doc)))   (shx "char offset 4:" x1)
  ;; re-aim onto the OPENS dimension with `with-axis`, walk it by open-paren index
  (define x2 (navigate (with-axis (with-index x1 1) sx-opens))) (shx "inside 1st list:" x2)
  (define x3 (navigate (with-index x2 2)))                      (shx "inside 2nd list:" x3)

  ;; ===== sexp: symbol-aware -- insert absorbs into the symbol, delete removes it =
  (printf "\n--- sexp: symbol-aware editing ---\n")
  (define (syms i mode) (run-axis sx-atoms sx-starts-atom? sx-ends-atom? i mode))
  (define y0 (navigate ((start (syms 1 'gap)) doc)))   (shx "before atom #1 (b):" y0)
  (define y1 (insert y0 "B"))                          (shx "insert \"B\":" y1)
  (define y2 (navigate (with-index y1 2)))             (shx "select atom #2 (c):" y2)
  (define y3 (delete y2))                              (shx "delete it:" y3))
