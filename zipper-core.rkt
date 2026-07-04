#lang racket

;; Zipper: structured navigation + editing over a summarised rope, as a stack machine.
;; A cursor is two guides, start gs + end ge (gap = gs=ge, seg = gs<ge); the surface:
;;   start         smr rope gs ge -> zipper      -- whole rope as focus, cursor installed
;;   zipper-guide  opt onto the cursor as the list (gs ge)  -- moving both edges
;;   zipper-edge   (zipper-edge i): opt onto edge i (0 = start, 1 = end) -- moving one
;;   zipper-focus  opt onto the focus rope                  -- editing the content
;;   edge-sides    (edge-sides i): opt onto edge i's summary cut as two foci L R, off a zipper
;;   to-root       fold the crumbs back -- the focus becomes the whole document
;;   on-edges      read the two edges as cuts, spread and combine
;; opt-get/opt-set/opt-update (algebra) drive the opts; re-exported for callers.
;; EVERY WRITE NAVIGATES (every opt put goes through the lift); guide-AGNOSTIC. Machine,
;; pipeline, and the printing/lift rationale: scribble/zipper-core.scrbl.

(require racket/match
         "rope-core.rkt"
         "toolbox/main.rkt")

(provide
 (contract-out
  [start        (-> smr/c rope? guide/c guide/c zipper?)]
  [to-root      cmd/c]
  [zipper-guide opt/c]
  [zipper-edge  (-> (or/c 0 1) opt/c)]
  [zipper-focus opt/c]
  [edge-sides   (-> (or/c 0 1) opt/c)]
  [on-edges     (-> binop/c binop/c binop/c (-> zipper? any))])
 opt-get opt-set opt-update compose-opt)   ; the opt ops (algebra), re-exported for callers

;; dev tooling -- NOT the navigation/editing API; reach via (require (submod "zipper-core.rkt" internal)).
;; run-chain's contract guards direct calls only; chain's expansion stays module-internal.
(module+ internal
  (provide chain
           (contract-out
            [run-chain (-> zipper? (listof (cons/c any/c cmd/c)) zipper?)])))

;; ---------- the machine: head, peek, navigate pipeline ----------

(struct head (before rope after) #:transparent)

(define (empty smr) ((make-rope smr)))

;; refocus a head onto a sub-rope given split: rope -> (values ls m rs). Returns the descended
;; head and the put-back -- a crumb (head -> head) the stack keeps to rebuild this level.
(define ((peek smr) split h)
  (match-define (head b t a) h)
  (define-values (ls m rs) (split t))
  (define focus (head (smr b ls) m (smr rs a)))
  (define (put h*) (head b ((make-rope smr) ls (head-rope h*) rs) a))
  (values focus put))

;; climb one level; unchanged once the focus contains the segment -- ascend's fixpoint halt.
(define (rise smr gs ge)
  (define (contains? h)
    (match-let ([(head b t a) h])
      (and (not (negative? (gs b (smr t a))))      ; start not left of the focus's left edge
           (not (positive? (ge (smr b t) a))))))   ; end not right of the focus's right edge
  (lambda (h k)
    (if (or (null? k) (contains? h))
        (values h k)
        (values ((car k) h) (cdr k)))))

;; descend one level; unchanged at a straddle/gap/boundary -- descend's fixpoint halt.
(define ((toward smr gs0 ge0) h k)
  (match-let*-values ([((head b t a))   h]
                      [(fr)             (frame smr b a)]    ; framed: read within-focus
                      [(gs)             (fr gs0)]
                      [(ge)             (fr ge0)]
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

(define (navigate smr gs ge)
  (define ascend  (fixed (rise   smr gs ge) eq? (arg 0)))    ; rise   to a fixpoint
  (define descend (fixed (toward smr gs ge) eq? (arg 0)))    ; toward to a fixpoint
  (define (uncrossed h k)                                    ; reject a crossed cursor
    (match-define (head b t a) h)
    (define fr (frame smr b a))                              ; framed: read within-focus
    (let-values ([(ls rs) ((multisect smr (fr gs)) t)])
      (when (negative? ((fr ge) ls rs))
        (error 'navigate "crossed cursor -- end precedes start")))
    (values h k))
  (define (carve h k)                                        ; the exact cut
    (match-define (head b _ a) h)
    (define fr (frame smr b a))
    (let-values ([(focus put) ((peek smr) (multisect smr (fr gs) (fr ge)) h)])
      (values focus (cons put k))))
  (compose carve descend uncrossed ascend))

;; ---------- the public surface ----------

(struct zipper (smr gs ge hd stack) #:transparent         ; fixed leads -- reseal is (curry zipper smr gs ge)
                                                          ; field `hd` (not `head`): frees zipper-head for the head lens
  #:property prop:custom-write (lambda (z port mode) (zipper-show z port)))

;; contracts -- defined here, below the struct, because they mention zipper?.
(define smr/c        procedure?)
(define guide/c      procedure?)       ; shape only; the -1/0/1 codomain is enforced where a guide
                                       ; is called (rope-core's multisect)
(define cmd/c        (-> zipper? zipper?))
(define binop/c      (procedure-arity-includes/c 2))
(define opt/c        opt?)

;; start: a fresh zipper -- whole rope as focus, cursor installed but not yet navigated.
(define (start smr rope gs ge) (zipper smr gs ge (head (smr "") rope (smr "")) '()))

;; zipper-lift: compose the ops (rightmost first) with navigate fixed as the permanent last op,
;; then reseal -- so every write lands where the guides point. No ops = plain re-navigation.
(define ((zipper-lift . ops) z)
  (match-define (zipper smr gs ge h k) z)
  ((apply compose (curry zipper smr gs ge)              ; curry reseals -- no cut
          (map (pass smr gs ge) (cons navigate ops)))   ; pass threads each op the (smr gs ge)
   h k))

;; zipper-guide: the opt onto the cursor as the guide list (list gs ge) -- a SINGLE focus (the
;; list); the put installs both guides and re-navigates. sexp-edit's verbs ride it via list-of/lref.
(define zipper-guide
  (opt-from-peek (lambda (z)
             (match-define (zipper smr gs ge h k) z)
             (values (lambda (p) ((zipper-lift) (zipper smr (first p) (second p) h k)))
                     (list gs ge)))))

;; zipper-edge: one opt parameterised by edge i (0 = start, 1 = end) -- the opt onto that single
;; guide; the put installs it (the other edge untouched) and re-navigates.
(define (zipper-edge i)
  (opt-from-peek (lambda (z)
             (match-define (zipper smr gs ge h k) z)
             (if (zero? i)
                 (values (lambda (g) ((zipper-lift) (zipper smr g  ge h k))) gs)
                 (values (lambda (g) ((zipper-lift) (zipper smr gs g  h k))) ge)))))

;; zipper-head: the opt onto the machine head (before . focus . after); the put reinstalls it,
;; coercing the focus to a rope (smr in scope here) and re-navigating. The coercion boundary: scribble.
(define zipper-head
  (opt-from-peek (lambda (z)
             (match-define (zipper smr gs ge h k) z)
             (values (lambda (h*)
                       (match-define (head b t a) h*)
                       ((zipper-lift) (zipper smr gs ge (head b ((make-rope smr) t) a) k)))
                     h))))

;; head-focus: the opt onto a head's focus rope -- the middle of (before . focus . after); the put
;; swaps the field, leaving the flanking summaries. Coercion + navigation are zipper-head's job.
(define head-focus
  (opt-from-peek (lambda (h)
             (match-define (head b t a) h)
             (values (lambda (t*) (head b t* a)) t))))

;; zipper-focus: the opt onto the focus rope, zipper-guide's twin; the put swaps in content and
;; re-navigates, delete = ((opt-set zipper-focus) ""). Decomposition + coercion: scribble Internals.
(define zipper-focus (compose-opt zipper-head head-focus))

;; edge-sides: opt onto edge i's summary cut as two foci L R, read off a zipper; the put writes a
;; gap at the cut and re-navigates. Fold the view with (compose k (opt-get ..)), k receiving L R.
(define (edge-sides i)
  (opt-from-peek
   (lambda (z)
     (match-define (zipper smr _ _ (head b m a) _) z)
     (define mt ((make-rope smr) ""))
     (define (put L R) (((opt-set zipper-head) (head L mt R)) z))   ; gap at the cut, re-navigated
     (if (zero? i)
         (values put b         (smr m a))                        ; start edge: foci L R
         (values put (smr b m) a)))))                            ; end edge:   foci L R

;; to-root: fold every crumb back into the head -- the focus becomes the whole document.
;; Outside the lift: homing must not navigate back down; the guides survive.
(define (to-root z)
  (match-define (zipper smr gs ge h k) z)
  (zipper smr gs ge (foldl (lambda (crumb h) (crumb h)) h k) '()))

;; on-edges: the cursor's two edges as cuts, spread over f and g and combined by c:
;;   ((on-edges c f g) z) = (c (f b (smr m a)) (g (smr b m) a))   -- left edge . right edge
(define ((on-edges c f g) z)
  (match-define (zipper smr _ _ (head b m a) _) z)
  (c (f b (smr m a)) (g (smr b m) a)))

;; ---------- printing ----------

;; prints as the marked document: gap -> before‸after, seg -> before⟦focus⟧after. pieces re-cut from
;; the root with the guides, so it leans on the guide-focus alignment every write keeps (see scribble).
(define (zipper-show z port)
  (match-define (list gs ge) ((opt-get zipper-guide) z))
  (define smr  (zipper-smr z))
  (define root ((opt-get zipper-focus) (to-root z)))
  (let-values ([(b m a) ((multisect smr gs ge) root)])
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
  (define (gap n)   (list (at n) (at n)))
  (define (seg i j) (list (at i) (at j)))
  (define (gap? z)  (equal? ((opt-get zipper-focus) z) ((make-rope cc))))
  (define (doc z)   (~a ((opt-get zipper-focus) (to-root z))))
  (define (place rope p) (start cc rope (first p) (second p)))   ; place a cursor list, unnavigated
  (define delete ((opt-set zipper-focus) ""))
  (define rope ((make-rope cc) "hello world"))
  (define g0 (gap 0))
  (define z0 (place rope g0))

  (check-equal? ((opt-get zipper-guide) z0) g0)
  (let ([g5 (gap 5)])
    (check-equal? ((opt-get zipper-guide) (((opt-set zipper-guide) g5) z0)) g5))

  (let ([z (((opt-set zipper-guide) (gap 5)) z0)])
    (check-true  (gap? z))
    (check-equal? (~a ((opt-get zipper-focus) z)) "")
    (check-equal? (doc (((opt-set zipper-focus) "XYZ") z)) "helloXYZ world"))

  (let ([z (((opt-set zipper-guide) (seg 0 5)) z0)])
    (check-false (gap? z))
    (check-equal? (~a ((opt-get zipper-focus) z)) "hello")
    (let ([z* (((opt-set zipper-focus) "HI") z)])
      (check-equal? (~a ((opt-get zipper-focus) z*)) "HI wo")
      (check-equal? (doc z*) "HI world"))
    (check-equal? (doc (delete z)) " world"))

  (let* ([z  (((opt-set zipper-guide) (seg 0 5)) z0)]
         [z* (((opt-set (zipper-edge 1)) (at 11)) z)])        ; move just the end edge to 11
    (check-equal? (~a ((opt-get zipper-focus) z*)) "hello world"))

  ;; edge-sides reads edge i's cut as two foci L R straight off the zipper; fold the view
  ;; with (compose k (opt-get ..)) -- k = list collects, a 2-arg k picks. (zipper-head still
  ;; drives it, internally -- not exported.)
  (let ([z (((opt-set zipper-guide) (seg 0 5)) z0)])          ; focus "hello", before "", after " world"
    (check-equal? (~a (head-rope ((opt-get zipper-head) z))) "hello")         ; zipper-head: internal, white-box
    (check-equal? ((compose list (opt-get (edge-sides 0))) z) '(0 11))   ; start: 0 | "hello world"
    (check-equal? ((compose list (opt-get (edge-sides 1))) z) '(5 6))    ; end:   "hello" | "world"
    (check-equal? ((compose (lambda (L R) L) (opt-get (edge-sides 0))) z) 0)  ; k receives L R -> L
    (check-equal? ((compose (lambda (L R) R) (opt-get (edge-sides 1))) z) 6))

  (let ([z (((opt-set zipper-guide) (seg 6 11)) z0)])
    (check-equal? (~a ((opt-get zipper-focus) z)) "world")
    (check-equal? (doc ((opt-update zipper-focus (lambda (m) ((make-rope cc) "[" m "]"))) z)) "hello [world]"))

  (check-equal? (~a ((opt-get zipper-focus) ((compose to-root
                                                      ((opt-set zipper-focus) "HI")
                                                      ((opt-set zipper-guide) (seg 0 5))) z0)))
                "HI world")

  (let ([z (((opt-set zipper-guide) (seg 6 11)) z0)])
    (check-equal? ((on-edges list list list) z) '((6 5) (11 0)))
    (check-equal? ((on-edges + - -) z) (+ (- 6 5) (- 11 0))))
  (let ([z (((opt-set zipper-guide) (gap 5)) z0)])
    (check-equal? ((on-edges list list list) z) '((5 6) (5 6))))

  (check-equal? (~a (((opt-set zipper-guide) (gap 5)) z0)) "hello‸ world")
  (check-equal? (~a (((opt-set zipper-guide) (seg 0 5)) z0)) "⟦hello⟧ world")
  (check-equal? (~a z0) "‸hello world")
  (check-equal? (~a (((opt-set zipper-focus) "HI") (((opt-set zipper-guide) (seg 0 5)) z0)))
                "⟦HI wo⟧rld")
  (let ([z (((opt-set zipper-guide) (gap 6)) (place ((make-rope cc) "ab\ncd\nef") (gap 0)))])
    (check-equal? (~a z) "ab\ncd\n‸ef"))

  (check-exn #rx"crossed cursor" (lambda () (((opt-set zipper-guide) (seg 5 2)) z0)))
  (check-not-exn (lambda () (((opt-set zipper-guide) (gap 5)) z0)))
  (check-not-exn (lambda () (((opt-set zipper-guide) (seg 2 5)) z0))))
