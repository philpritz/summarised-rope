#lang racket

;; Zipper: structured navigation + editing over a summarised rope, as a stack machine.
;;
;; The cursor is a `head` (before-summary · focus-rope · after-summary) plus a crumb stack --
;; each crumb a closure head -> head that rebuilds the parent focus.  An op is
;;
;;   config -> smr -> ((head stack) -> (values head stack))
;;
;; and (zipper-lift op ...) hands each op the zipper's own smr, composes them (rightmost
;; runs first, like compose), NAVIGATES with the installed guides, and reseals the run
;; into a zipper -- every lifted run lands with the cursor standing where the guides
;; point on the new state.  The machine ops:
;;
;;   ascend    rise (pop+apply crumbs) until the focus contains the whole segment
;;   descend   strip whole subtrees (balance halve + guide reads) to the minimal node
;;   carve     cut the focus EXACTLY at the boundaries (`multisect`), middle -> focus
;;   navigate  = carve . descend . ascend as ONE op -- the lift's permanent last op
;;
;; A guide is a comparator (L R) -> {-1,0,1}: +1 if the target boundary is right of the
;; cut, -1 left, 0 at it.  A cursor is a 2-guide vector (start end); a gap is start = end
;; (an empty focus), a seg is start < end.
;;
;; The surface is two three-faced accessors and the lifecycle pair:
;;   guide   read | install | modify the cursor    -- moving
;;   focus   read | swap | transform the content   -- editing
;;   start / to-root                                -- in, home
;; Both accessors' write faces go through the lift, so EVERY WRITE NAVIGATES; delete is
;; ((focus "") z), insert is a swap at a gap, and edits chain by composition.  An index
;; swap (an anchor flip) and a guide swap (a move) are the same operation.  `to-root` is
;; deliberately outside the lift: homing must not navigate back down; guides survive it.
;;
;; zipper-core is guide-AGNOSTIC: it only ever calls a guide, never names its kind.  Structural
;; guides (sexp, char, ...) live in their own files.

(require racket/match
         srfi/26                ; cut  -- (cut <> smr) feeds ops their smr; (cut zipper ...) reseals
         "rope-core.rkt")       ; make-summary make-rope multisect frame

(provide start to-root               ; lifecycle: in, home
         guide                       ; the navigation accessor: read | install | modify
         focus                       ; the editing accessor: read | swap | transform
         on-edges)                   ; the two edge cuts, spread over f g, combined by c

;; ---------- focus ----------
(struct head (before rope after) #:transparent)

(define (empty smr) ((make-rope smr)))

;; ---------- lens ----------
;; ((lens smr) split h): a splitter  split : rope -> (values ls m rs)  refocuses a head onto m,
;; summarising ls/rs into the anchors; returns (values focus-head put-crumb).
(define ((lens smr) split h)
  (match-define (head b t a) h)
  (define-values (ls m rs) (split t))
  (values (head (smr b ls) m (smr rs a))
          (lambda (h*) (head b ((make-rope smr) ls (head-rope h*) rs) a))))

;; ---------- machine ----------
(define (rise h k) (values ((car k) h) (cdr k)))        ; one step: pop a crumb, apply it

;; contains?: does the focus bracket the whole segment?  start watches the left edge, end the right.
(define (((contains? guides) smr) h)
  (match-let* ([(head b t a)   h]
               [(vector gs ge) guides])
    (and (not (negative? (gs b (smr t a))))      ; start not left of the focus's left edge
         (not (positive? (ge (smr b t) a))))))   ; end not right of the focus's right edge

;; ascend: rise until the focus contains the segment (recursive step: rise then ascend).
(define (((ascend guides) smr) h k)
  (if (or (null? k) (((contains? guides) smr) h))
      (values h k)
      ((compose ((ascend guides) smr) rise) h k)))

;; toward: one descent step.  Halve the focus; an empty half (either side) means the
;; focus is atomic -- nothing to strip, carve does within-atom -- so halt.  Else frame
;; the guides into within-focus probes and route by the seam reads:
;;   gs@seam = +1  -> whole seg right of seam -> descend R
;;   ge@seam = -1  -> whole seg left  of seam -> descend L
;;   otherwise (straddle / gap / boundary on seam) -> halt; carve places the exact cut.
;; The edge reads (gs@left, ge@right) are the out-of-focus guards (= ascend's containment test).
(define (((toward guides) smr) h k)
  (match-let*-values ([((head b t a))   h]
                      [((vector gs ge)) (vector-map (frame smr b a) guides)]  ; framed: read within-focus
                      [(mt)             (empty smr)]
                      [(lt rt)          ((multisect) t)]                      ; the balance halve
                      [(atom?)          (or (equal? lt mt) (equal? rt mt))]   ; an empty half, either side
                      [(into)           (lambda (split)
                                          (let-values ([(h* c) ((lens smr) split h)])
                                            (values h* (cons c k))))])
    (if atom?
        (values h k)
        (match* ((gs mt t) (gs lt rt) (ge lt rt) (ge t mt))  ; left edge | seam | seam | right edge
          [(-1 _ _ _) (error 'toward "start precedes the focus -- ascend further")]
          [(_ _ _ 1)  (error 'toward "end follows the focus -- ascend further")]
          [(_ 1 _ _)  (into (lambda (_) (values lt rt mt)))] ; whole seg right of seam -> lt | rt | ()
          [(_ _ -1 _) (into (lambda (_) (values mt lt rt)))] ; whole seg left  of seam -> () | lt | rt
          [(_ _ _ _)  (values h k)]))))                      ; straddle / gap / boundary -> halt

;; descend: iterate toward to a fixpoint (halt = head returned unchanged).
(define (((descend guides) smr) h k)
  (let loop ([h h] [k k])
    (let-values ([(h* k*) (((toward guides) smr) h k)])
      (if (eq? h* h) (values h k) (loop h* k*)))))

;; carve: cut the focus at the 2 boundaries via multisect (guides framed by the head's
;; context), middle piece -> focus (empty = gap).
(define (((carve guides) smr) h k)
  (match-define (head b _ a) h)
  (let-values ([(h* c) ((lens smr) (multisect (vector-map (frame smr b a) guides)) h)])
    (values h* (cons c k))))

;; navigate: the navigation pipeline as ONE op -- ascend, then descend, then carve.
(define ((navigate guides) smr)
  (compose ((carve guides) smr) ((descend guides) smr) ((ascend guides) smr)))

;; ---------- public zipper ----------
;; prop:custom-write: a zipper prints as its document with the cursor marked
;; (see zipper-show below), like ropes print as their text.
(struct zipper (head stack smr guides) #:transparent
  #:property prop:custom-write (lambda (z port mode) (zipper-show z port)))

(define (start smr rope) (zipper (head (smr "") rope (smr "")) '() smr #f))

;; zipper-lift: hand each op the zipper's own smr, compose (rightmost runs first)
;; with `navigate` as the permanent last op, reseal.  Every write funnels through
;; the lift, so every write lands with the cursor standing where the installed
;; guides point on the new state; (zipper-lift) with no ops is plain re-navigation.
(define ((zipper-lift . ops) z)
  (match-define (zipper h k smr gs) z)
  ((apply compose (cut zipper <> <> smr gs)
          (map (cut <> smr) (cons (navigate gs) ops)))
   h k))

;; guide: the navigation accessor, three faces dispatched by type.
;;   (guide z)       read the installed pair
;;   ((guide gs) z)  install a 2-vector      }  both write faces
;;   ((guide f) z)   install (f current)     }  navigate
;; The faces are disjoint at this level (zipper | procedure | 2-vector); modify =
;; install what f makes of the read.  Composed accessors reach the zipper only
;; through the write faces, so a composite write navigates exactly once, at the
;; outermost face.
(define guide
  (letrec ([install (curry (lambda (gs z)
                             (match-define (zipper h k smr _) z)
                             ((zipper-lift) (zipper h k smr gs))))]
           [modify  (curry (lambda (f z) ((install (f (zipper-guides z))) z)))])
    (match-lambda
      [(? zipper? z)         (zipper-guides z)]
      [(? procedure? f)      (modify f)]
      [(and gs (vector _ _)) (install gs)])))

;; focus: the editing accessor, guide's twin.
;;   (focus z)       read the focus rope
;;   ((focus c) z)   swap in content c (string | rope)   }  both write faces
;;   ((focus f) z)   swap in (f current)                 }  navigate
;; delete = ((focus "") z); insert = a swap at a gap.  set = the lift composed
;; with the op that swaps the head's rope (make-rope coerces; anchors and stack
;; pass through untouched, so the edit is safe until navigation lands it).
(define focus
  (letrec ([read   (lambda (z) (head-rope (zipper-head z)))]
           [set    (compose zipper-lift
                            (curry (lambda (c smr h k)
                                     (match-let ([(head b _ a) h])
                                       (values (head b ((make-rope smr) c) a) k)))))]
           [modify (curry (lambda (f z) ((set (f (read z))) z)))])
    (match-lambda
      [(? zipper? z)    (read z)]
      [(? procedure? f) (modify f)]
      [c                (set c)])))

;; to-root: fold every crumb back into the head -- the focus becomes the whole
;; document.  Deliberately OUTSIDE the lift: homing must not navigate back down;
;; the guides survive for the next install.
(define (to-root z)
  (match-define (zipper h k smr gs) z)
  (zipper (foldl (lambda (crumb h) (crumb h)) h k) '() smr gs))

;; on-edges: the cursor's two edges as cuts, spread over f and g, combined by c
;; (the spread-combine shape).  Each edge of the focus is a cut on the document;
;; the focus folds onto the side the edge doesn't face, with the zipper's own smr:
;;   ((on-edges c f g) z) = (c (f b (smr m a)) (g (smr b m) a))
;;                              '- left edge -'  '- right edge -'
(define ((on-edges c f g) z)
  (match-define (zipper (head b m a) _ smr _) z)
  (c (f b (smr m a)) (g (smr b m) a)))

;; ---------- printing ----------
;; The zipper prints as its document with the cursor marked inline:
;;   gap -> before‸after        seg -> before⟦focus⟧after
;; The pieces are read by RE-CUTTING: `multisect` with the installed guides over
;; the root document.  This LEANS ON THE GUIDE--FOCUS ALIGNMENT: every write
;; re-navigates, so the cursor stands exactly where its guides point and the
;; re-cut reproduces the focus.  True by the invariant, but an extra load on it
;; -- a guide-free reconstruction (off the crumbs) was sketched and not taken
;; for now.  No guides installed -> the bare document.
(define (zipper-show z port)
  (define gs   (guide z))
  (define smr  (zipper-smr z))
  (define root (focus (to-root z)))
  (if gs
      (let-values ([(b m a) ((multisect gs) root)])
        (if (equal? m (empty smr))
            (fprintf port "~a‸~a" b a)
            (fprintf port "~a⟦~a⟧~a" b m a)))
      (display root port)))

;; ============================================================================
(module+ test
  (require rackunit)
  (define cc (make-summary string-length +))
  ;; char cursor: a boundary at offset n.  L = char count left of the cut (+1 = boundary right).
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define (gap n)   (vector (at n) (at n)))
  (define (seg i j) (vector (at i) (at j)))
  (define (gap? z)  (equal? (focus z) ((make-rope cc))))
  (define (doc z)   (~a (focus (to-root z))))
  (define delete (focus ""))
  (define rope ((make-rope cc) "hello world"))
  (define z0 (start cc rope))

  ;; the lens, read face: installing is remembered; a fresh zipper has no guides
  (check-false (guide z0))
  (let ([g5 (gap 5)])
    (check-eq? (guide ((guide g5) z0)) g5))

  ;; gap: installing navigates to an empty focus; a swap at a gap = insert
  (let ([z ((guide (gap 5)) z0)])
    (check-true  (gap? z))
    (check-equal? (~a (focus z)) "")
    (check-equal? (doc ((focus "XYZ") z)) "helloXYZ world"))

  ;; seg: the slice is the focus; a write navigates to where the guide points
  ;; on the NEW text (char guides re-resolve by offset, hence "HI wo")
  (let ([z ((guide (seg 0 5)) z0)])
    (check-false (gap? z))
    (check-equal? (~a (focus z)) "hello")
    (let ([z* ((focus "HI") z)])
      (check-equal? (~a (focus z*)) "HI wo")
      (check-equal? (doc z*) "HI world"))
    (check-equal? (doc (delete z)) " world"))

  ;; the lens, modify face: f sees the old pair -- change one edge, keep the other
  (let* ([z  ((guide (seg 0 5)) z0)]
         [z* ((guide (lambda (gs) (vector (vector-ref gs 0) (at 11)))) z)])
    (check-equal? (~a (focus z*)) "hello world"))

  ;; a seg in the middle; wrapping is focus's modify face around the current focus
  (let ([z ((guide (seg 6 11)) z0)])
    (check-equal? (~a (focus z)) "world")
    (check-equal? (doc ((focus (lambda (m) ((make-rope cc) "[" m "]"))) z)) "hello [world]"))

  ;; writes compose: one navigate-edit-home pipeline
  (check-equal? (~a (focus ((compose to-root (focus "HI") (guide (seg 0 5))) z0)))
                "HI world")

  ;; --- on-edges: each edge cut through its own function, results combined ---
  (let ([z ((guide (seg 6 11)) z0)])                          ; focus "world"
    (check-equal? ((on-edges list list list) z) '((6 5) (11 0)))   ; b | m+a . b+m | a
    (check-equal? ((on-edges + - -) z) (+ (- 6 5) (- 11 0))))      ; spread-combine shape
  (let ([z ((guide (gap 5)) z0)])                             ; a gap: both edges agree
    (check-equal? ((on-edges list list list) z) '((5 6) (5 6))))

  ;; --- printing: a zipper displays as its marked document ---
  (check-equal? (~a ((guide (gap 5)) z0)) "hello‸ world")     ; gap = caret
  (check-equal? (~a ((guide (seg 0 5)) z0)) "⟦hello⟧ world")  ; seg = bracketed focus
  (check-equal? (~a z0) "hello world")                        ; no guides -> bare document
  (check-equal? (~a ((focus "HI") ((guide (seg 0 5)) z0)))    ; navigated cursor shows
                "⟦HI wo⟧rld")
  (let ([z ((guide (gap 6)) (start cc ((make-rope cc) "ab\ncd\nef")))])
    (check-equal? (~a z) "ab\ncd\n‸ef")))                     ; marks sit at the cut, multi-line
