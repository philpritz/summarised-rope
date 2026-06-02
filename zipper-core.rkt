#lang racket

;; Zipper: structured navigation over a summarised rope, built as a *stack
;; machine*. The cursor is two values threaded together -- a `head` and a crumb
;; stack `k`:
;;
;;   head : (before rope after)   the working register -- the focus sub-rope plus
;;                                the cached summaries of everything to its left
;;                                (`before`) and right (`after`) in the document.
;;   k    : (listof crumb)        the stack; each crumb a repair closure
;;                                `head -> head` that rebuilds the parent focus.
;;
;; Machine ops thread `(head stack) -> (values head stack)`. Descent pushes a
;; crumb; `rise` pops and applies one; `to-root` folds the whole stack. The head's
;; before/after are a memoized fold of the stack -- a cache for O(1) guide reads,
;; not core state. Guide-curried ops compose by Racket's multiple-value `compose`.
;;
;; smr (the summary fn) rides *alongside the guide* g -- the curried ops are
;; `(op g smr ...)`. It never enters the machine state: it is a fixed parameter the
;; caller already holds (it built the rope with `(roper smr)`), seeded at `start`.
;; This replaces recovering smr from a focus rope (the old `rope-algebra`), which
;; the pruned rope-core no longer exposes. Only `lens` and `contains?` actually
;; read it; `search`/`ascend`/`navigate` merely route it; `rise`/`descend`/`over`/
;; `gap?` never touch it (the crumbs close over smr at descend time, via `arrange`).
;;
;; Scope: this is the navigation (move/gap) core. Segs (selection / editing) are
;; designed but NOT built yet -- see discussions/2026-06-02/1-claude.md.

(require racket/match
         "rope-core.rkt")          ; summariser roper bisect atom? tree-size

(provide
 (struct-out head)
 arrange gap? rise descend over     ; machine primitives
 lens contains? search ascend navigate   ; guide-driven (smr alongside g)
 start to-root)                     ; entry / exit

;; A focus: a sub-rope plus the summaries bracketing it in the whole document.
(struct head (before rope after) #:transparent)

(define (empty smr) ((roper smr)))      ; the canonical empty rope (no-arg roper)

;; gap?: the focus is empty. O(1) and smr-free via the cached size field.
(define (gap? h) (zero? (tree-size (head-rope h))))

;; arrange: focus on `m`, stashing ropes `ls`/`rs` either side. Returns the new
;; head (anchors extended by the stashed summaries) and the `put` that undoes it.
;; Empty stashes vanish under roper, so this serves every arrangement; a crumb is
;; exactly such a put, and it closes over smr -- which is why rise/to-root/over
;; never need smr again.
(define (arrange smr b ls m rs a)
  (values (head (smr b ls) m (smr rs a))
          (lambda (h*) (head b ((roper smr) ls (head-rope h*) rs) a))))

;; rise: pop one crumb and apply it, reconstructing the parent focus.
(define (rise h k) (values ((car k) h) (cdr k)))

;; descend: a lens splits the focus into (child-head, crumb); push the crumb.
;;   lens : head -> (values head crumb)
(define ((descend lens) h k)
  (define-values (h* c) (lens h))
  (values h* (cons c k)))

;; over: edit the focus in place; the stack is untouched (anchors fixed -> safe).
(define ((over f) h k) (values (f h) k))

;; lens: the descent step for a guide. Bisect once, read the guide at the L|R
;; seam, and arrange accordingly -- or, at an atom, drop to a gap beside it. smr
;; rides alongside g.
(define ((lens g smr) h)
  (match-define (head b t a) h)
  (define mt (empty smr))
  (cond
    [(atom? t)
     (if (positive? (g b (smr t a)))
         (arrange smr b t mt mt a)      ; element on the left  -> gap after it
         (arrange smr b mt mt t a))]    ; element on the right -> gap before it
    [else
     (define-values (L R) (bisect t))
     (case (g (smr b L) (smr R a))
       [(-1) (arrange smr b mt L R a)]  ; target left  : focus L, stash R
       [(1)  (arrange smr b L R mt a)]  ; target right : focus R, stash L
       [(0)  (arrange smr b L mt R a)]  ; at the seam  : empty middle = the gap
       [else (error 'lens "guide must return -1, 0, or 1")])]))

;; contains?: does the guide's target lie within this focus? No bisect -- it reads
;; the focus against its own anchors. At a gap both probes collapse to
;; (zero? (g before after)): the gap *is* the target spot.
(define ((contains? g smr) h)
  (match-define (head b t a) h)
  (and (not (negative? (g b (smr t a))))
       (not (positive? (g (smr b t) a)))))

;; search: guided descent to the gap -- (descend (lens g smr)), iterated. The
;; recursion is the consumer/left of `compose`, so it stays a tail call.
(define ((search g smr) h k)
  (if (gap? h) (values h k)
      ((compose (search g smr) (descend (lens g smr))) h k)))

;; ascend: rise until the focus contains the target (or we reach the root).
(define ((ascend g smr) h k)
  (if (or (null? k) ((contains? g smr) h)) (values h k)
      ((compose (ascend g smr) rise) h k)))

;; navigate: ascend to a focus containing the target, then search down to its gap.
(define (navigate g smr) (compose (search g smr) (ascend g smr)))

;; start: a cursor on the whole document. smr is GIVEN (the caller built `rope`
;; with it), not recovered from the rope.
(define (start g smr rope) (values (head (smr "") rope (smr "")) '()))

;; to-root: rise to the top, leaving the whole document as one focus.
(define (to-root h k)
  (if (null? k) (values h k)
      (let-values ([(h* k*) (rise h k)]) (to-root h* k*))))

;; ============================================================================
;; A char guide drives the tests: the char-count summary IS the offset, so the
;; projection is the identity. `point i` marks the gap at offset i.
(module+ test
  (require rackunit)
  (define cc (summariser string-length +))
  (define ((point i) l r) (cond [(> l i) -1] [(< l i) 1] [else 0]))
  (define (mk s)  ((roper cc) s))
  (define (mk2 s) ((roper cc #:chunk-size 2) s))     ; multi-leaf
  (define (doc-text h k) (let-values ([(rh _) (to-root h k)]) (~a (head-rope rh))))

  ;; --- movement lands in a gap and preserves the text, at every offset ---
  (for ([i (in-range 0 12)])
    (define-values (h0 k0) (start (point i) cc (mk "hello world")))
    (define-values (h k) ((navigate (point i) cc) h0 k0))
    (check-true  (gap? h)                       (format "gap at ~a" i))
    (check-equal? (doc-text h k) "hello world"  (format "text at ~a" i)))

  ;; --- insert at a gap via `over` -> the rebuilt doc (threaded smr in the crumbs) ---
  (define (insert-at i content src)
    (define-values (h0 k0) (start (point i) cc src))
    (define-values (h k) ((navigate (point i) cc) h0 k0))
    (define-values (h2 k2)
      ((over (lambda (hd) (head (head-before hd) ((roper cc) content) (head-after hd)))) h k))
    (doc-text h2 k2))
  (check-equal? (insert-at 5 "XYZ" (mk "hello world")) "helloXYZ world")
  (check-equal? (insert-at 0 "Q"   (mk "abc"))          "Qabc")
  (check-equal? (insert-at 3 "Z"   (mk "abc"))          "abcZ")
  (check-equal? (insert-at 0 "hi"  (mk ""))             "hi")        ; empty doc: one gap

  ;; --- a chunked, multi-leaf rope behaves identically ---
  (let*-values ([(h0 k0) (start (point 5) cc (mk2 "hello world"))]
                [(h k)   ((navigate (point 5) cc) h0 k0)])
    (check-true   (gap? h))
    (check-equal? (doc-text h k) "hello world"))
  (check-equal? (insert-at 5 ", " (mk2 "hello world")) "hello,  world")

  ;; --- sequential navigation from a deep, non-empty stack exercises ascend/contains? ---
  (let*-values ([(q0 qk0) (start (point 8) cc (mk2 "hello world"))]
                [(q1 qk1) ((navigate (point 8) cc) q0 qk0)]
                [(q2 qk2) ((navigate (point 2) cc) q1 qk1)])
    (check-true   (> (length qk1) 1))            ; the settle left a real stack to ascend
    (check-true   (gap? q2))
    (check-equal? (head-before q2) 2)            ; landed exactly at offset 2
    (check-equal? (doc-text q2 qk2) "hello world")))
