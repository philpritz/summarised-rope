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
;; A guide is a callable (left-total right-total) -> sign. A *gap* guide returns
;; -1 | 0 | 1 -- where the target boundary sits relative to the cursor. `smr` rides
;; inside the zipper; the guide g is the per-call argument to `navigate`.
;;
;; `descend` is a carry binary search. It brackets the focus by its two boundary
;; reads -- L = before|rope, R = rope|after -- and stops the instant either reads
;; 0: the gap sits on that boundary. Otherwise it bisects, reads the new seam,
;; routes to the side the target is on, carries the outer edge and slots the seam
;; into the inner one. Because a gap on a boundary is caught by the edge read,
;; there is NO atom special case (an atom's only gap positions are its two edges),
;; and a gap at the document edge is caught at the root with no descent at all.
;;
;; Scope: navigation by a gap guide. Seg guides (-2..2) and selection/editing
;; (`carve`) are designed but not built here yet (see the discussion notes).

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
;; consumes. The edge builders place an empty focus (a gap) at a boundary of the
;; whole rope; the bisect family takes a node's two halves l,r (from one `bisect`)
;; and either gaps their seam or makes one half the focus, the rest the stash. `mt`
;; is the empty rope.
(define ((edge-l mt) t) (values mt mt t))             ; gap at the left edge
(define ((edge-r mt) t) (values t mt mt))             ; gap at the right edge
(define (seam   l r mt) (lambda (_) (values l mt r))) ; gap at the seam between l,r
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

;; toward: the guide-aware chooser-and-applier -- one descent step. Bisect, read the
;; guide at the focus's left edge | seam | right edge, and return the lens pair
;; (focus . put) for the move the target selects. The seam routes (-1 left, 0 gap, 1
;; right); an edge reading 0 is a gap landing there. The two outward edge reads --
;; left edge -1 (target sits in `before`) or right edge 1 (in `after`) -- mean the
;; target isn't in this focus: ascend didn't climb far enough, so they raise. The
;; half-* rows pin their inward edge (Le=1 / Re=-1) so those outward cases fall
;; through to the errors rather than being routed; seam=0 never occurs in an error,
;; so the zero rows need no pin.
(define ((toward g smr) h)
  (match-let*-values ([((head b t a)) h]
                      [(mt)      (empty smr)]
                      [(refocus) (lens smr)]
                      [(probe)   (lambda (l r) (g (smr b l) (smr r a)))]
                      [(lt rt)   (bisect t)])
    (match* ((probe mt t) (probe lt rt) (probe t mt))   ; left edge | seam | right edge
      [(0 _  _) (refocus (edge-l mt)       h)]   ; gap at the left edge
      [(_ _  0) (refocus (edge-r mt)       h)]   ; gap at the right edge
      [(_ 0  _) (refocus (seam   lt rt mt) h)]   ; gap at the seam
      [(1 -1 _) (refocus (half-l lt rt mt) h)]   ; into the left half   (left edge inward)
      [(_ 1 -1) (refocus (half-r lt rt mt) h)]   ; into the right half  (right edge inward)
      [(-1 _ _) (error 'toward "target precedes the focus -- ascend further")]
      [(_ _  1) (error 'toward "target follows the focus -- ascend further")])))

;; drive: iterate a step (head -> (values focus put)) onto the crumb stack until the
;; focus is a gap. Guide- and lens-agnostic: just "step, stack, repeat."
(define ((drive step smr) h k)
  (let loop ([h h] [k k])
    (if (at-gap? smr h)
        (values h k)
        (let-values ([(h* c) (step h)])
          (loop h* (cons c k))))))

;; descend: walk the focus to the gap by driving the per-step `toward` chooser.
(define (descend g smr) (drive (toward g smr) smr))

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
(module+ test
  (require rackunit)
  (define cc (make-summary string-length +))
  (define ((point i) l r) (cond [(> l i) -1] [(< l i) 1] [else 0]))
  (define (mk s)  ((make-rope cc) s))
  (define (mk2 s) ((make-rope cc #:chunk-size 2) s))     ; multi-leaf
  (define (doc-text z) (~a (focus (to-root z))))    ; the whole document as a string

  ;; --- movement lands in a gap and preserves the text, at every offset ---
  (for ([i (in-range 0 12)])
    (define z (navigate (point i) (start cc (mk "hello world"))))
    (check-true   (gap? z)                     (format "gap at ~a" i))
    (check-equal? (doc-text z) "hello world"   (format "text at ~a" i)))

  ;; --- insert at a gap via `over` (a rope -> rope edit on the focus) ---
  (define (insert-at i content src)
    (define z (navigate (point i) (start cc src)))
    (doc-text (over (lambda (_) ((make-rope cc) content)) z)))
  (check-equal? (insert-at 5 "XYZ" (mk "hello world")) "helloXYZ world")
  (check-equal? (insert-at 0 "Q"   (mk "abc"))          "Qabc")
  (check-equal? (insert-at 3 "Z"   (mk "abc"))          "abcZ")
  (check-equal? (insert-at 0 "hi"  (mk ""))             "hi")        ; empty doc: one gap

  ;; --- a chunked, multi-leaf rope behaves identically ---
  (let ([z (navigate (point 5) (start cc (mk2 "hello world")))])
    (check-true   (gap? z))
    (check-equal? (doc-text z) "hello world"))
  (check-equal? (insert-at 5 ", " (mk2 "hello world")) "hello,  world")

  ;; --- sequential navigation from a deep, non-empty stack exercises ascend ---
  ;; (white-box: peeks at the internal stack/head to check depth and landing offset)
  (let* ([z1 (navigate (point 8) (start cc (mk2 "hello world")))]
         [z2 (navigate (point 2) z1)])
    (check-true   (> (length (zipper-stack z1)) 1))    ; the settle left a real stack to ascend
    (check-true   (gap? z2))
    (check-equal? (head-before (zipper-head z2)) 2)    ; landed exactly at offset 2
    (check-equal? (doc-text z2) "hello world")))
