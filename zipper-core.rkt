#lang racket

;; Zipper: structured navigation over a summarised rope, built as a *stack
;; machine*. The cursor is two values threaded together -- a `head` and a crumb
;; stack `k`:
;;
;;   head : (before rope after)   the working register -- the focus sub-rope plus
;;                                the cached summaries of everything to its left
;;                                (before) and right (after) in the document.
;;   k    : (listof crumb)        the stack; each crumb a repair closure
;;                                head -> head that rebuilds the parent focus.
;;
;; Machine ops thread (head stack) -> (values head stack). `descend` pushes a crumb
;; per step; `rise` pops and applies one; `to-root` folds the whole stack.
;;
;; The PUBLIC surface wraps these in a `zipper` struct -- (head, stack, smr) bundled
;; into one opaque value -- so callers thread a single zipper and never pass `smr`
;; (the summary fn), which is captured at `start`. Only the operations are exported
;; (start navigate over to-root focus gap?); the head, stack, and struct stay private.
;;
;; A guide is a callable (left-total right-total) -> {-2..2}: the position of the
;; target segment relative to a cut (+2 before . +1 at start . 0 inside . -1 at end
;; . -2 after). A *gap* guide is the degenerate -1|0|1 (start = end); pass it as the
;; guide and it rides through unchanged, since sgn(g) = g. `smr` rides inside the
;; zipper; the guide is the per-call argument to `navigate`.
;;
;; `descend` strips toward the minimal node. At each step it bisects the focus and
;; reads sgn(guide) at the SEAM: +1 means the whole seg is right of the seam (strip
;; the left subtree, descend R), -1 left (strip right, descend L), 0 means the seg
;; straddles the seam -- the minimal containing node, so halt. The edge reads are
;; only out-of-focus guards (precedes/follows -> ascend further). Termination at an
;; extreme edge (a gap pinned at an atom boundary, which never reaches an interior
;; seam) is the "nothing left to strip" halt: the side we would strip came back empty.
;;
;; Scope: descend halts at the minimal node bracketing the target; it no longer
;; lands an empty focus -- placing the gap (and extracting a seg slice) is `carve`,
;; designed but not built yet (see the discussion notes).

(require racket/match
         "rope-core.rkt")          ; make-summary make-rope bisect

;; The public surface: the zipper operations. Everything else (the head struct, the
;; lens/splitter machine, ascend/descend, the zipper struct itself) is internal.
(provide start navigate over to-root focus gap?)

;; A focus: a sub-rope plus the summaries bracketing it in the whole document.
(struct head (before rope after) #:transparent)

(define (empty smr) ((make-rope smr)))      ; the canonical empty rope

;; at-gap?: the head's focus is empty -- i.e. equal to the canonical empty rope. (The
;; empty branch is unconstructable in rope-core, so the only empty is that one leaf.)
;; The public `gap?` (below) wraps this for a zipper.
(define (at-gap? smr h) (equal? (head-rope h) (empty smr)))

;; lens: a splitter -- split : rope -> (values ls m rs) with ls.m.rs = the focus --
;; becomes a refocusing of a head. ((lens smr) split) : head -> (values focus put)
;; is the concrete (Store-coalgebra) form of a lens. It applies `split` to the focus
;; t, makes the middle m the new focus, and summarises the stashes ls/rs into the
;; anchors; `put` rebuilds the parent focus from a (possibly edited) middle. The
;; returned (focus put) pair is the store the lens factors through; a crumb is that
;; put, and it closes over smr (so rise/over need none).
(define ((lens smr) split h)
  (match-define (head b t a) h)
  (define-values (ls m rs) (split t))
  (values (head (smr b ls) m (smr rs a))
          (lambda (h*) (head b ((make-rope smr) ls (head-rope h*) rs) a))))

;; Splitters: rope -> (values ls m rs), ls.m.rs = the rope -- the partition `lens`
;; consumes. `descend` uses only the half-* pair (make one of `bisect`'s two halves the
;; focus, stash the other). The edge/seam builders place an empty focus (a gap) at a
;; boundary -- they are the gap LANDING, which now belongs to `carve` (not built yet),
;; so they are unused by `toward` for the moment. `mt` is the empty rope.
(define ((edge-l mt) t) (values mt mt t))             ; gap at the left edge   (for carve)
(define ((edge-r mt) t) (values t mt mt))             ; gap at the right edge  (for carve)
(define (seam   l r mt) (lambda (_) (values l mt r))) ; gap at the seam        (for carve)
(define (half-l l r mt) (lambda (_) (values mt l r))) ; focus the left half
(define (half-r l r mt) (lambda (_) (values l r mt))) ; focus the right half

;; rise: pop one crumb and apply it, reconstructing the parent focus.
(define (rise h k) (values ((car k) h) (cdr k)))

;; contains?: does the guide's target lie within this focus? (No bisect.)
(define ((contains? g smr) h)
  (match-define (head b t a) h)
  (and (not (negative? (g b (smr t a))))
       (not (positive? (g (smr b t) a)))))

;; ascend: rise until the focus contains the target (or we reach the root).
(define ((ascend g smr) h k)
  (if (or (null? k) ((contains? g smr) h)) (values h k)
      ((compose (ascend g smr) rise) h k)))

;; toward: one descent step, a full machine move (head stack) -> (values head stack).
;; Bisect the focus, read T = sgn(guide) at the left edge | seam | right edge, and let
;; the SEAM decide: +1 the whole seg is right of the seam, so the left subtree is
;; unnecessary -- strip it and descend R; -1 strip right, descend L; 0 the seg straddles
;; the seam, so neither half can go -- this is the minimal node, halt (return unchanged).
;; The edges are only out-of-focus guards (-1 at the left = precedes, +1 at the right =
;; follows -> ascend further). The one non-probe bit is termination: a gap pinned at an
;; atom's edge never reaches an interior seam, so the strip side comes back empty (`lt`
;; or `rt` = the empty rope); that "nothing left to strip" is also a halt.
(define ((toward q smr) h k)
  (match-define (head b t a) h)
  (define mt (empty smr))
  (define (T l r) (sgn (q (smr b l) (smr r a))))
  (define-values (lt rt) (bisect t))
  (define (into split)                                   ; apply a splitter, push its crumb
    (let-values ([(h* c) ((lens smr) split h)])
      (values h* (cons c k))))
  (match* ((T mt t) (T lt rt) (T t mt))                  ; left edge | SEAM | right edge
    [(-1 _  _) (error 'toward "seg precedes the focus -- ascend further")]
    [(_  _  1) (error 'toward "seg follows the focus -- ascend further")]
    [(_  0  _) (values h k)]                              ; straddle / gap at seam -> halt (minimal)
    [(_  1  _) (if (equal? lt mt) (values h k)            ; seg right of seam -> strip LEFT subtree
                   (into (half-r lt rt mt)))]             ;   (atom edge: nothing to strip -> halt)
    [(_ -1  _) (if (equal? rt mt) (values h k)            ; seg left of seam  -> strip RIGHT subtree
                   (into (half-l lt rt mt)))]))           ;   (atom edge -> halt)

;; descend: iterate `toward` to a fixpoint -- keep stripping until a step halts (returns
;; the head unchanged). The halt leaves the focus at the minimal node bracketing the
;; target; landing the gap / extracting the slice is `carve` (not built yet).
(define ((descend q smr) h k)
  (let loop ([h h] [k k])
    (let-values ([(h* k*) ((toward q smr) h k)])
      (if (eq? h* h) (values h k) (loop h* k*)))))

;; ---------------------------------------------------------------------------
;; Public zipper API. A `zipper` bundles the internal (head, crumb stack) cursor with
;; the summary algebra smr, so callers thread one opaque value and the guide g is the
;; only per-call extra. The struct is private; only the operations below ship.
(struct zipper (head stack smr) #:transparent)

;; start: a zipper on the whole document. smr (the summary the rope was built with)
;; is captured here and rides inside the zipper from now on.
(define (start smr rope) (zipper (head (smr "") rope (smr "")) '() smr))

;; navigate: move the cursor to guide g's target -- ascend to a focus containing it,
;; then descend to its gap.
(define (navigate g z)
  (match-define (zipper h k smr) z)
  (let-values ([(h* k*) ((compose (descend g smr) (ascend g smr)) h k)])
    (zipper h* k* smr)))

;; over: edit the focus rope in place (f : rope -> rope). The anchors and stack are
;; untouched, so the edit is safe.
(define (over f z)
  (match-define (zipper (head b t a) k smr) z)
  (zipper (head b (f t) a) k smr))

;; to-root: rise to the top, leaving the whole document as the focus -- fold every
;; crumb (head -> head) into the head, top of stack first, ending with an empty stack.
(define (to-root z)
  (match-define (zipper h k smr) z)
  (zipper (foldl (lambda (crumb h) (crumb h)) h k) '() smr))

;; focus: the focused sub-rope of the zipper (the whole document once at the root).
(define (focus z) (head-rope (zipper-head z)))

;; gap?: is the zipper's focus a gap (the empty rope)?
(define (gap? z) (at-gap? (zipper-smr z) (zipper-head z)))

;; ============================================================================
;; A char gap-guide drives the tests: the char-count summary IS the offset, so the
;; projection is the identity. `point i` marks the gap at offset i. Tests thread a
;; single zipper through the public API.
;;
;; descend now HALTS at the minimal node bracketing the target; it no longer lands an
;; empty focus -- that (and inserting at the gap) is `carve`, not built yet. So the
;; descent contract tested here is: (1) the text round-trips (descend discards nothing),
;; and (2) the focus span [before, before+size] brackets the offset.
(module+ test
  (require rackunit)
  (define cc (make-summary string-length +))
  (define ((point i) l r) (cond [(> l i) -1] [(< l i) 1] [else 0]))
  (define (mk s)  ((make-rope cc) s))
  (define (mk2 s) ((make-rope cc #:chunk-size 2) s))     ; multi-leaf
  (define (doc-text z) (~a (focus (to-root z))))    ; the whole document as a string

  ;; the focus's span brackets the offset (cc(focus) = its char size)
  (define (brackets? z i)
    (define before (head-before (zipper-head z)))
    (<= before i (+ before (cc (focus z)))))

  ;; --- descent round-trips and brackets the offset, single leaf, every offset ---
  (for ([i (in-range 0 12)])
    (define z (navigate (point i) (start cc (mk "hello world"))))
    (check-equal? (doc-text z) "hello world"   (format "round-trip at ~a" i))
    (check-true   (brackets? z i)              (format "brackets ~a" i)))

  ;; --- a chunked, multi-leaf rope behaves identically ---
  (for ([i (in-range 0 12)])
    (define z (navigate (point i) (start cc (mk2 "hello world"))))
    (check-equal? (doc-text z) "hello world"   (format "mk2 round-trip at ~a" i))
    (check-true   (brackets? z i)              (format "mk2 brackets ~a" i)))

  ;; --- empty document: descend halts immediately, focus brackets offset 0 ---
  (let ([z (navigate (point 0) (start cc (mk "")))])
    (check-equal? (doc-text z) "")
    (check-true   (brackets? z 0)))

  ;; --- sequential navigation from a deep, non-empty stack exercises ascend ---
  ;; (white-box: peeks at the internal stack/head)
  (let* ([z1 (navigate (point 8) (start cc (mk2 "hello world")))]
         [z2 (navigate (point 2) z1)])
    (check-true   (positive? (length (zipper-stack z1))))  ; descend left a stack to rise from
    (check-false  (brackets? z1 2))                        ; z1 sits away from offset 2 -> ascend needed
    (check-equal? (doc-text z2) "hello world")
    (check-true   (brackets? z2 2)))                       ; ascend + re-descend brackets offset 2

  ;; NOTE: gap landing (empty focus) and insert-at-gap are deferred to `carve` (the
  ;; degenerate gap-carve); until then descend stops at the minimal bracketing node.
  )
