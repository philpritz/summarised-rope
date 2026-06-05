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
;; A guide is a callable (left-total right-total) -> sign. A *gap* guide returns
;; -1 | 0 | 1 -- where the target boundary sits relative to the cursor. `smr` (the
;; summary fn) rides alongside the guide as a fixed parameter the caller already
;; holds (it built the rope with `(rope smr)`), seeded at `start`.
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
         "rope-core.rkt")          ; summary rope bisect

(provide
 (struct-out head)
 arrange gap? rise over            ; machine primitives
 contains? ascend descend navigate ; guide-driven (smr rides alongside g)
 start to-root)

;; A focus: a sub-rope plus the summaries bracketing it in the whole document.
(struct head (before rope after) #:transparent)

(define (empty smr) ((rope smr)))      ; the canonical empty rope

;; gap?: the focus is empty -- i.e. equal to the canonical empty rope. (The empty
;; branch is unconstructable in rope-core, so the only empty is that one leaf.)
(define (gap? smr h) (equal? (head-rope h) (empty smr)))

;; arrange: focus `m`, stashing ropes `ls`/`rs` either side. Returns the new head
;; (anchors extended by the stashed summaries) and the `put` that undoes it -- a
;; crumb is exactly such a put, and it closes over smr (so rise/over need none).
(define (arrange smr b ls m rs a)
  (values (head (smr b ls) m (smr rs a))
          (lambda (h*) (head b ((rope smr) ls (head-rope h*) rs) a))))

;; rise: pop one crumb and apply it, reconstructing the parent focus.
(define (rise h k) (values ((car k) h) (cdr k)))

;; over: edit the focus in place; the stack is untouched (anchors fixed -> safe).
(define ((over f) h k) (values (f h) k))

;; contains?: does the guide's target lie within this focus? (No bisect.)
(define ((contains? g smr) h)
  (match-define (head b t a) h)
  (and (not (negative? (g b (smr t a))))
       (not (positive? (g (smr b t) a)))))

;; ascend: rise until the focus contains the target (or we reach the root).
(define ((ascend g smr) h k)
  (if (or (null? k) ((contains? g smr) h)) (values h k)
      ((compose (ascend g smr) rise) h k)))

;; descend: the carry binary search -- walk the focus to the gap, a crumb per step.
;; Carry the two edge reads L/R; stop when either reads 0 (the gap is on that
;; boundary); else bisect, route by the seam s, carry the outer edge and slot the
;; seam into the inner one. No atom case: an atom's gap is one of its edges.
(define ((descend g smr) h k)
  (define mt (empty smr))
  (match-define (head b t a) h)
  (let loop ([h h] [k k] [L (g b (smr t a))] [R (g (smr b t) a)])
    (match-define (head b t a) h)
    (define (go ls m rs) (let-values ([(h* c) (arrange smr b ls m rs a)]) (values h* (cons c k))))
    (cond
      [(zero? L) (go mt mt t)]                       ; gap at t's left edge
      [(zero? R) (go t mt mt)]                        ; gap at t's right edge
      [else
       (define-values (lt rt) (bisect t))
       (define s (g (smr b lt) (smr rt a)))           ; one fresh read: the seam
       (cond
         [(zero? s)     (go lt mt rt)]                                            ; gap at the seam
         [(negative? s) (let-values ([(h k) (go mt lt rt)]) (loop h k L s))]      ; left:  carry L, R<-s
         [else          (let-values ([(h k) (go lt rt mt)]) (loop h k s R))])]))) ; right: L<-s, carry R

;; navigate: ascend to a focus containing the target, then descend to its gap.
(define (navigate g smr) (compose (descend g smr) (ascend g smr)))

;; start: a cursor on the whole document; smr is GIVEN (the caller built `rope`).
(define (start smr rope) (values (head (smr "") rope (smr "")) '()))

;; to-root: rise to the top, leaving the whole document as one focus.
(define (to-root h k)
  (if (null? k) (values h k)
      (let-values ([(h* k*) (rise h k)]) (to-root h* k*))))

;; ============================================================================
;; A char gap-guide drives the tests: the char-count summary IS the offset, so the
;; projection is the identity. `point i` marks the gap at offset i.
(module+ test
  (require rackunit)
  (define cc (summary string-length +))
  (define ((point i) l r) (cond [(> l i) -1] [(< l i) 1] [else 0]))
  (define (mk s)  ((rope cc) s))
  (define (mk2 s) ((rope cc #:chunk-size 2) s))     ; multi-leaf
  (define (doc-text h k) (let-values ([(rh _) (to-root h k)]) (~a (head-rope rh))))

  ;; --- movement lands in a gap and preserves the text, at every offset ---
  (for ([i (in-range 0 12)])
    (define-values (h0 k0) (start cc (mk "hello world")))
    (define-values (h k) ((navigate (point i) cc) h0 k0))
    (check-true  (gap? cc h)                    (format "gap at ~a" i))
    (check-equal? (doc-text h k) "hello world"  (format "text at ~a" i)))

  ;; --- insert at a gap via `over` ---
  (define (insert-at i content src)
    (define-values (h0 k0) (start cc src))
    (define-values (h k) ((navigate (point i) cc) h0 k0))
    (define-values (h2 k2)
      ((over (lambda (hd) (head (head-before hd) ((rope cc) content) (head-after hd)))) h k))
    (doc-text h2 k2))
  (check-equal? (insert-at 5 "XYZ" (mk "hello world")) "helloXYZ world")
  (check-equal? (insert-at 0 "Q"   (mk "abc"))          "Qabc")
  (check-equal? (insert-at 3 "Z"   (mk "abc"))          "abcZ")
  (check-equal? (insert-at 0 "hi"  (mk ""))             "hi")        ; empty doc: one gap

  ;; --- a chunked, multi-leaf rope behaves identically ---
  (let*-values ([(h0 k0) (start cc (mk2 "hello world"))]
                [(h k)   ((navigate (point 5) cc) h0 k0)])
    (check-true   (gap? cc h))
    (check-equal? (doc-text h k) "hello world"))
  (check-equal? (insert-at 5 ", " (mk2 "hello world")) "hello,  world")

  ;; --- sequential navigation from a deep, non-empty stack exercises ascend ---
  (let*-values ([(q0 qk0) (start cc (mk2 "hello world"))]
                [(q1 qk1) ((navigate (point 8) cc) q0 qk0)]
                [(q2 qk2) ((navigate (point 2) cc) q1 qk1)])
    (check-true   (> (length qk1) 1))            ; the settle left a real stack to ascend
    (check-true   (gap? cc q2))
    (check-equal? (head-before q2) 2)            ; landed exactly at offset 2
    (check-equal? (doc-text q2 qk2) "hello world")))
