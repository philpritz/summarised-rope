#lang racket

;; Zipper: structured navigation + editing over a summarised rope, as a stack machine.
;;
;; The cursor is a `head` (before-summary · focus-rope · after-summary) plus a crumb stack --
;; each crumb a closure head -> head that rebuilds the parent focus.  An op is
;;
;;   config -> smr -> ((head stack) -> (values head stack))
;;
;; and (zipper-lift op ...) hands each op the zipper's own smr, composes them (rightmost
;; runs first, like compose), and reseals the run into a zipper.  The machine ops:
;;
;;   ascend   rise (pop+apply crumbs) until the focus contains the whole segment
;;   descend  strip whole subtrees (balance halve + guide reads) to the minimal node
;;   carve    cut the focus EXACTLY at the boundaries (`multisect`), middle piece -> focus
;;   navigate = (zipper-lift carve descend ascend)
;;
;; A guide is a comparator (L R) -> {-1,0,1}: +1 if the target boundary is right of the cut,
;; -1 left, 0 at it.  A cursor is a 2-guide vector (start end); a gap is start = end (an empty
;; focus), a seg is start < end.  Editing verbs are ops over the focus (`over*` and friends),
;; lifted the same way -- ((replace content) z) -> z -- and `to-root` folds the crumbs back
;; into the whole document, so an edit is: navigate, replace, to-root.
;;
;; zipper-core is guide-AGNOSTIC: it only ever calls a guide, never names its kind.  Structural
;; guides (sexp, char, ...) live in their own files.

(require racket/match
         srfi/26                ; cut  -- (cut <> smr) feeds ops their smr; (cut zipper ...) reseals
         "rope-core.rkt")       ; make-summary make-rope multisect frame

(provide start navigate to-root      ; lifecycle: in, move, home
         replace insert              ; editing verbs (config -> zipper -> zipper)
         delete                      ; the empty replace, pre-configured (zipper -> zipper)
         peek)                       ; read-out: (peek z) -> (values before focus after)

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
(define (rise h k) (values ((car k) h) (cdr k)))        ; pop a crumb, apply it

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

;; ---------- machine verbs ----------
;; over* is the one focus-toucher: it runs f on the focus rope, anchors and stack
;; untouched (so the edit is safe).  replace* just configures it.
(define (((over* f) smr) h k)
  (match-let ([(head b m a) h])
    (values (head b (f m) a) k)))

(define ((replace* content) smr)
  ((over* (const ((make-rope smr) content))) smr))   ; make-rope coerces string | rope

;; to-root*: fold every crumb back into the head; the focus becomes the whole document.
(define ((to-root* smr) h k)
  (values (foldl (lambda (crumb h) (crumb h)) h k) '()))

;; ---------- public zipper ----------
(struct zipper (head stack smr) #:transparent)

(define (start smr rope) (zipper (head (smr "") rope (smr "")) '() smr))

;; zipper-lift: hand each op the zipper's own smr, compose (rightmost runs first), reseal.
;; The one place a zipper is opened; `start` and the accessors below are the only others
;; that touch its insides.
(define ((zipper-lift . ops) z)
  (match-define (zipper h k smr) z)
  ((apply compose (cut zipper <> <> smr)
          (map (cut <> smr) ops))
   h k))

;; navigate = carve . descend . ascend, sealed by the lift.
(define (navigate guides)
  (unless (= (vector-length guides) 2)
    (error 'navigate "cursor needs exactly 2 guides (start end); got ~a" (vector-length guides)))
  (zipper-lift (carve guides) (descend guides) (ascend guides)))

;; editing verbs: configured, then lifted -- ((replace content) z) -> z, so they chain by
;; composition.  insert is replace, named for a gap (an empty focus); delete is the empty
;; replace.  WHERE a gap sits (its gravity) was fixed by the guide at navigate time.
(define (replace content) (zipper-lift (replace* content)))
(define insert            replace)
(define delete            (replace ""))
(define to-root           (zipper-lift to-root*))

;; peek: the cursor's view -- the bracketing summaries and the focused rope.
(define (peek z)
  (match-let ([(zipper (head b m a) _ _) z])
    (values b m a)))

;; ============================================================================
(module+ test
  (require rackunit)
  (define cc (make-summary string-length +))
  ;; char cursor: a boundary at offset n.  L = char count left of the cut (+1 = boundary right).
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define (gap n)   (vector (at n) (at n)))
  (define (seg i j) (vector (at i) (at j)))
  (define (focus z) (let-values ([(b m a) (peek z)]) m))
  (define (gap? z)  (equal? (focus z) ((make-rope cc))))
  (define (doc z)   (~a (focus (to-root z))))
  (define rope ((make-rope cc #:chunk-size 2) "hello world"))
  (define z0 (start cc rope))

  ;; gap: empty focus, insert lands at the offset
  (let ([z ((navigate (gap 5)) z0)])
    (check-true  (gap? z))
    (check-equal? (~a (focus z)) "")
    (check-equal? (doc ((insert "XYZ") z)) "helloXYZ world"))

  ;; seg: the slice is the focus; replace / delete rebuild the whole document
  (let ([z ((navigate (seg 0 5)) z0)])
    (check-false (gap? z))
    (check-equal? (~a (focus z)) "hello")
    (check-equal? (doc ((replace "HI") z)) "HI world")
    (check-equal? (doc (delete z)) " world"))

  ;; a seg in the middle; wrapping is replace around the peeked focus
  (let ([z ((navigate (seg 6 11)) z0)])
    (check-equal? (~a (focus z)) "world")
    (check-equal? (doc ((replace ((make-rope cc) "[" (focus z) "]")) z)) "hello [world]"))

  ;; verbs compose: one navigate-edit-home pipeline
  (check-equal? (~a (focus ((compose to-root (replace "HI") (navigate (seg 0 5))) z0)))
                "HI world"))
