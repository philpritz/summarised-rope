#lang racket

;; Zipper: structured navigation + editing over a summarised rope, as a stack machine.
;; The cursor is a `head` (before-summary · focus-rope · after-summary) + a crumb stack,
;; each crumb a closure head -> head that rebuilds the parent focus.
;; The surface: two lenses (read/edit via viewer, setter, updater) and the lifecycle pair.
;;   zipper-guide   lens onto the cursor       -- moving
;;   zipper-focus   lens onto the focus rope   -- editing
;;   start / to-root                                       -- in, home
;; EVERY WRITE NAVIGATES: each lens's put goes through the lift, so the cursor lands where the
;; installed guides point on the new state (delete = (setter zipper-focus ""), insert = set at a gap).
;; A guide is a comparator (L R) -> {-1,0,1}; a cursor is a 2-guide vector (start end), gap =
;; start=end, seg = start<end. zipper-core is guide-AGNOSTIC. The machine is in scribble/zipper-core.scrbl.

(require racket/match
         "rope-core.rkt"
         "helper-algebras.rkt")

(provide
 (contract-out
  [start        (-> smr/c rope? guide-pair/c zipper?)]
  [to-root      cmd/c]
  [zipper-guide lens/c]
  [zipper-focus lens/c]
  [on-edges     (-> binop/c binop/c binop/c (-> zipper? any))])
 viewer setter updater)                ; the lens ops (helper-algebras), re-exported for callers

;; dev tooling -- NOT the navigation/editing API; reach via (require (submod "zipper-core.rkt" internal)).
;; run-chain's contract guards direct calls only; chain's expansion stays module-internal.
(module+ internal
  (provide chain
           (contract-out
            [run-chain (-> zipper? (listof (cons/c any/c cmd/c)) zipper?)])))

;; ---------- the machine: head, peek, navigate pipeline ----------

(struct head (before rope after) #:transparent)

(define (empty smr) ((make-rope smr)))

;; peek -- the store coalgebra: refocus a head onto a sub-rope. split: rope -> (values ls m rs);
;; returns (values focus put) -- the descended head and the put-back the stack keeps as a crumb.
(define ((peek smr) split h)
  (match-define (head b t a) h)
  (define-values (ls m rs) (split t))
  (define focus (head (smr b ls) m (smr rs a)))
  (define (put h*) (head b ((make-rope smr) ls (head-rope h*) rs) a))
  (values focus put))

;; climb one level; unchanged once the focus contains the segment -- ascend's fixpoint halt.
(define (rise smr guides)
  (define (contains? h)
    (match-let* ([(head b t a)   h]
                 [(vector gs ge) guides])
      (and (not (negative? (gs b (smr t a))))      ; start not left of the focus's left edge
           (not (positive? (ge (smr b t) a))))))   ; end not right of the focus's right edge
  (lambda (h k)
    (if (or (null? k) (contains? h))
        (values h k)
        (values ((car k) h) (cdr k)))))

;; descend one level; unchanged at a straddle/gap/boundary -- descend's fixpoint halt.
(define ((toward smr guides) h k)
  (match-let*-values ([((head b t a))   h]
                      [((vector gs ge)) (vector-map (frame smr b a) guides)]  ; framed: read within-focus
                      [(mt)             (empty smr)]
                      [(lt rt)          ((multisect smr) t)]                  ; the balance halve
                      [(atom?)          (or (equal? lt mt) (equal? rt mt))]   ; an empty half, either side
                      [(into)           (lambda (split)
                                          (let-values ([(focus put) ((peek smr) split h)])
                                            (values focus (cons put k))))])
    (if atom?
        (values h k)
        (match* ((gs mt t) (gs lt rt) (ge lt rt) (ge t mt))  ; left edge | seam | seam | right edge
          [(-1 _ _ _) (error 'toward "start precedes the focus -- ascend further")]
          [(_ _ _ 1)  (error 'toward "end follows the focus -- ascend further")]
          [(_ 1 _ _)  (into (lambda (_) (values lt rt mt)))] ; whole seg right of seam -> lt | rt | ()
          [(_ _ -1 _) (into (lambda (_) (values mt lt rt)))] ; whole seg left  of seam -> () | lt | rt
          [(_ _ _ _)  (values h k)]))))                      ; straddle / gap / boundary -> halt

(define (navigate smr guides)
  (define ascend  (fixed (rise   smr guides) eq? (arg 0)))   ; rise   to a fixpoint
  (define descend (fixed (toward smr guides) eq? (arg 0)))   ; toward to a fixpoint
  (define (uncrossed h k)                                    ; reject a crossed cursor
    (match-define (head b t a) h)
    (match-define (vector gs ge) (vector-map (frame smr b a) guides))
    (let-values ([(ls rs) ((multisect smr (vector gs)) t)])
      (when (negative? (ge ls rs))
        (error 'navigate "crossed cursor -- end precedes start")))
    (values h k))
  (define (carve h k)                                        ; the exact cut
    (match-define (head b _ a) h)
    (match-define (vector gs ge) (vector-map (frame smr b a) guides))
    (let-values ([(focus put) ((peek smr) (multisect smr (vector gs ge)) h)])
      (values focus (cons put k))))
  (compose carve descend uncrossed ascend))

;; ---------- the public surface ----------

(struct zipper (smr guides head stack) #:transparent      ; fixed pair leads -- reseal is (curry zipper smr gs)
  #:property prop:custom-write (lambda (z port mode) (zipper-show z port)))

;; contracts -- defined here, below the struct, because they mention zipper?.
(define smr/c        procedure?)
(define guide-pair/c (vector/c procedure? procedure? #:flat? #t)) ; shape only; the -1/0/1 codomain is
;;   enforced downstream where guides are called (rope-core's multisect)
(define cmd/c        (-> zipper? zipper?))
(define binop/c      (procedure-arity-includes/c 2))
;; a (van Laarhoven) lens: (a -> f a) -> (z -> f z).  Just a procedure -- the functor structure
;; isn't a flat contract, and the ops (viewer/setter/updater) enforce the shape in use.
(define lens/c       procedure?)

;; start: a fresh zipper -- whole rope as focus, cursor installed but not yet navigated.
(define (start smr rope gs) (zipper smr gs (head (smr "") rope (smr "")) '()))

;; zipper-lift: thread each op the zipper's (smr gs), compose (rightmost runs first) with navigate
;; as the permanent last op, reseal -- so every write lands where the guides point. (zipper-lift) with
;; no ops = plain re-navigation. `pass` (helper-algebras) is the thrush: ((pass smr gs) op) = (op smr gs).
(define ((zipper-lift . ops) z)
  (match-define (zipper smr gs h k) z)
  ((apply compose (curry zipper smr gs)              ; curry reseals -- no cut
          (map (pass smr gs) (cons navigate ops)))   ; pass threads each op the (smr gs) pair
   h k))

;; zipper-guide: the lens onto the cursor -- zipper -> (values guide-pair put); the put installs the
;; new pair and re-navigates (the lift), so a write lands where the new guides point.
(define zipper-guide
  (make-lens (lambda (z)
             (match-define (zipper smr gs h k) z)
             (values gs (lambda (gs*) ((zipper-lift) (zipper smr gs* h k)))))))

;; zipper-focus: zipper-guide's twin, the lens onto the focus rope; the put swaps in content
;; (make-rope coerces) and re-navigates.  delete = (setter zipper-focus "").
(define zipper-focus
  (make-lens (lambda (z)
             (match-define (zipper smr gs (head b t a) k) z)
             (values t (lambda (c) ((zipper-lift) (zipper smr gs (head b ((make-rope smr) c) a) k)))))))

;; to-root: fold every crumb back into the head -- the focus becomes the whole document.
;; Outside the lift: homing must not navigate back down; the guides survive.
(define (to-root z)
  (match-define (zipper smr gs h k) z)
  (zipper smr gs (foldl (lambda (crumb h) (crumb h)) h k) '()))

;; on-edges: the cursor's two edges as cuts, spread over f and g and combined by c:
;;   ((on-edges c f g) z) = (c (f b (smr m a)) (g (smr b m) a))   -- left edge . right edge
(define ((on-edges c f g) z)
  (match-define (zipper smr _ (head b m a) _) z)
  (c (f b (smr m a)) (g (smr b m) a)))

;; ---------- printing ----------

;; prints as the marked document: gap -> before‸after, seg -> before⟦focus⟧after.
;; pieces re-cut from the root with the guides -- relies on the guide-focus alignment every write keeps.
(define (zipper-show z port)
  (define gs   ((viewer zipper-guide) z))
  (define smr  (zipper-smr z))
  (define root ((viewer zipper-focus) (to-root z)))
  (let-values ([(b m a) ((multisect smr gs) root)])
    (if (equal? m (empty smr))
        (fprintf port "~a‸~a" b a)
        (fprintf port "~a⟦~a⟧~a" b m a))))

;; ---------- editing traces ----------

;; chain: pipe z0 through the commands, printing each command's source beside the zipper it
;; produces. The macro captures the source (only a macro can); run-chain threads it.
(define (run-chain z0 steps)
  (printf "~a~a\n" (~a "(start)" #:min-width 30) z0)
  (for/fold ([z z0]) ([step (in-list steps)])
    (match-define (cons label cmd) step)
    (define z* (cmd z))
    (printf "~a~a\n" (~a (~v label) #:min-width 30) z*)
    z*))
(define-syntax-rule (chain z0 op ...)
  (run-chain z0 (list (cons (quote op) op) ...)))

(module+ test
  (require rackunit)
  (define cc (make-summary string-length +))
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define (gap n)   (vector (at n) (at n)))
  (define (seg i j) (vector (at i) (at j)))
  (define (gap? z)  (equal? ((viewer zipper-focus) z) ((make-rope cc))))
  (define (doc z)   (~a ((viewer zipper-focus) (to-root z))))
  (define delete (setter zipper-focus ""))
  (define rope ((make-rope cc) "hello world"))
  (define g0 (gap 0))
  (define z0 (start cc rope g0))

  (check-eq? ((viewer zipper-guide) z0) g0)
  (let ([g5 (gap 5)])
    (check-eq? ((viewer zipper-guide) ((setter zipper-guide g5) z0)) g5))

  (let ([z ((setter zipper-guide (gap 5)) z0)])
    (check-true  (gap? z))
    (check-equal? (~a ((viewer zipper-focus) z)) "")
    (check-equal? (doc ((setter zipper-focus "XYZ") z)) "helloXYZ world"))

  (let ([z ((setter zipper-guide (seg 0 5)) z0)])
    (check-false (gap? z))
    (check-equal? (~a ((viewer zipper-focus) z)) "hello")
    (let ([z* ((setter zipper-focus "HI") z)])
      (check-equal? (~a ((viewer zipper-focus) z*)) "HI wo")
      (check-equal? (doc z*) "HI world"))
    (check-equal? (doc (delete z)) " world"))

  (let* ([z  ((setter zipper-guide (seg 0 5)) z0)]
         [z* ((updater zipper-guide (lambda (gs) (vector (vector-ref gs 0) (at 11)))) z)])
    (check-equal? (~a ((viewer zipper-focus) z*)) "hello world"))

  (let ([z ((setter zipper-guide (seg 6 11)) z0)])
    (check-equal? (~a ((viewer zipper-focus) z)) "world")
    (check-equal? (doc ((updater zipper-focus (lambda (m) ((make-rope cc) "[" m "]"))) z)) "hello [world]"))

  (check-equal? (~a ((viewer zipper-focus) ((compose to-root
                                                     (setter zipper-focus "HI")
                                                     (setter zipper-guide (seg 0 5))) z0)))
                "HI world")

  (let ([z ((setter zipper-guide (seg 6 11)) z0)])
    (check-equal? ((on-edges list list list) z) '((6 5) (11 0)))
    (check-equal? ((on-edges + - -) z) (+ (- 6 5) (- 11 0))))
  (let ([z ((setter zipper-guide (gap 5)) z0)])
    (check-equal? ((on-edges list list list) z) '((5 6) (5 6))))

  (check-equal? (~a ((setter zipper-guide (gap 5)) z0)) "hello‸ world")
  (check-equal? (~a ((setter zipper-guide (seg 0 5)) z0)) "⟦hello⟧ world")
  (check-equal? (~a z0) "‸hello world")
  (check-equal? (~a ((setter zipper-focus "HI") ((setter zipper-guide (seg 0 5)) z0)))
                "⟦HI wo⟧rld")
  (let ([z ((setter zipper-guide (gap 6)) (start cc ((make-rope cc) "ab\ncd\nef") (gap 0)))])
    (check-equal? (~a z) "ab\ncd\n‸ef"))

  (check-exn #rx"crossed cursor" (lambda () ((setter zipper-guide (seg 5 2)) z0)))
  (check-not-exn (lambda () ((setter zipper-guide (gap 5)) z0)))
  (check-not-exn (lambda () ((setter zipper-guide (seg 2 5)) z0))))
