#lang racket

;; Zipper: structured navigation + editing over a summarised rope, as a stack machine.
;; The cursor is a `head` (before-summary · focus-rope · after-summary) plus a crumb
;; stack -- each crumb a closure head -> head that rebuilds the parent focus. The machine
;; (the lift, the navigate pipeline, the lens) is documented in scribble/zipper-core.scrbl.
;;
;; The surface: two three-faced accessors and the lifecycle pair.
;;   zipper-guide   read | install | modify the cursor   -- moving
;;   zipper-focus   read | swap    | transform content    -- editing
;;   start / to-root                                       -- in, home
;; Both accessors' write faces go through the lift (`zipper-lift`), so EVERY WRITE
;; NAVIGATES: the cursor lands where the installed guides point on the new state. delete is
;; ((zipper-focus "") z), insert is a swap at a gap, and edits chain by composition.
;;
;; A guide is a comparator (L R) -> {-1,0,1}: +1 if the target boundary is right of the
;; cut, -1 left, 0 at it. A cursor is a 2-guide vector (start end); a gap is start = end
;; (an empty focus), a seg is start < end. zipper-core is guide-AGNOSTIC -- it only ever
;; calls a guide, never names its kind; structural guides (sexp, char, ...) live elsewhere.

(require racket/match
         "rope-core.rkt"            ; make-summary make-rope multisect frame rope?
         "helper-algebras.rkt")     ; fixed arg pass

;; The contracted surface.  The vocabulary and the two accessor contracts are
;; defined below, after the zipper struct, since they mention zipper?.
(provide
 (contract-out
  [start        (-> smr/c rope? guide-pair/c zipper?)]   ; lifecycle: in (a cursor is required)
  [to-root      cmd/c]                                   ; lifecycle: home
  [zipper-guide guide-accessor/c]                        ; read pair | install | modify
  [zipper-focus focus-accessor/c]                        ; read rope | swap   | modify
  [on-edges     (-> binop/c binop/c binop/c (-> zipper? any))]))      ; the two edge cuts (c may multi-value)

;; ---------- internals (dev tooling) ----------
;; A submodule for dev tooling -- NOT the navigation/editing API. Reach it with
;; (require (submod "zipper-core.rkt" internal)). The editing traces: `chain` (a macro,
;; provided plain) and the `run-chain` it expands to. run-chain's contract guards DIRECT
;; calls only -- chain's expansion stays module-internal, never crossing that boundary.
(module+ internal
  (provide chain
           (contract-out
            [run-chain (-> zipper? (listof (cons/c any/c cmd/c)) zipper?)])))

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
;; rise: climb one level (pop a crumb, rebuild the parent focus) unless the focus already
;; contains the segment, or the stack is empty -- that no-op is ascend's fixpoint halt.
;; contains? (rise's private test): does the focus bracket the whole segment?
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

;; toward: one descent step. Halve the focus; an empty half means an atomic focus (halt --
;; carve does within-atom). Else frame the guides within-focus and route by the seam reads
;; (the match clauses below). The edge reads are the out-of-focus guards (= ascend's containment test).
(define ((toward smr guides) h k)
  (match-let*-values ([((head b t a))   h]
                      [((vector gs ge)) (vector-map (frame smr b a) guides)]  ; framed: read within-focus
                      [(mt)             (empty smr)]
                      [(lt rt)          ((multisect smr) t)]                  ; the balance halve
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

;; navigate: the navigation pipeline as ONE op -- ascend . uncrossed . descend . carve
;; (see scribble/zipper-core.scrbl). The four stages are navigate's own locals: ascend/
;; descend are fixpoints of rise/toward (keyed on the head via (arg 0)); uncrossed/carve
;; frame the head's context into the guides, then read / cut the focus.
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
    (let-values ([(h* c) ((lens smr) (multisect smr (vector gs ge)) h)])
      (values h* (cons c k))))
  (compose carve descend uncrossed ascend))

;; ---------- public zipper ----------
;; prop:custom-write: a zipper prints as its document with the cursor marked
;; (see zipper-show below), like ropes print as their text.
(struct zipper (smr guides head stack) #:transparent      ; fixed pair leads -- reseal is (curry zipper smr gs)
  #:property prop:custom-write (lambda (z port mode) (zipper-show z port)))

;; ---------- contract vocabulary ----------
;; Private (referenced by the contract-out at the top).  Defined here, below the
;; zipper struct, because they mention zipper?.
(define smr/c        procedure?)
(define content/c    (or/c string? rope?))                        ; focus content
(define guide-pair/c (vector/c procedure? procedure? #:flat? #t)) ; a cursor: 2 guides (shape only;
;;   the -1/0/1 codomain is enforced downstream where guides are called -- rope-core's multisect)
(define cmd/c        (-> zipper? zipper?))                         ; a navigating command
(define binop/c      (procedure-arity-includes/c 2))              ; on-edges' c/f/g
;; a three-faced accessor: a zipper reads `read/c`; any other face builds a command.
(define (accessor/c domain read/c)
  (->i ([x domain]) [result (x) (if (zipper? x) read/c cmd/c)]))
(define guide-accessor/c (accessor/c (or/c zipper? guide-pair/c procedure?) guide-pair/c))
(define focus-accessor/c (accessor/c (or/c zipper? content/c    procedure?) rope?))

;; start: a fresh zipper -- the whole rope as focus, the given cursor installed but NOT yet
;; navigated (the first write/install navigates). A cursor is required (no guideless zipper).
(define (start smr rope gs) (zipper smr gs (head (smr "") rope (smr "")) '()))

;; zipper-lift: thread each op the zipper's own (smr gs), compose (rightmost runs first)
;; with `navigate` as the permanent last op, reseal -- so every write lands where the
;; guides point (see scribble). (zipper-lift) with no ops is plain re-navigation.
;; `pass` (helper-algebras) is the thrush: ((pass smr gs) op) = (op smr gs).

(define ((zipper-lift . ops) z)
  (match-define (zipper smr gs h k) z)
  ((apply compose (curry zipper smr gs)              ; curry reseals -- no cut
          (map (pass smr gs) (cons navigate ops)))   ; pass threads each op the (smr gs) pair
   h k))

;; zipper-guide: the navigation accessor -- three faces by type (see scribble):
;;   (zipper-guide z) read | ((zipper-guide gs) z) install | ((zipper-guide f) z) modify.
;; modify = install what f makes of the read. Composed accessors reach the zipper only
;; through the write faces, so a composite write navigates exactly once, at the outermost face.
(define zipper-guide
  (local [(define ((install gs) z)
            (match-define (zipper smr _ h k) z)
            ((zipper-lift) (zipper smr gs h k)))
          (define ((modify f) z) ((install (f (zipper-guides z))) z))]
    (match-lambda
      [(? zipper? z)         (zipper-guides z)]
      [(? procedure? f)      (modify f)]
      [(and gs (vector _ _)) (install gs)])))

;; zipper-focus: the editing accessor, zipper-guide's twin -- three faces (see scribble):
;;   (zipper-focus z) read | ((zipper-focus c) z) swap content | ((zipper-focus f) z) modify.
;; delete = ((zipper-focus "") z); insert = a swap at a gap. The swap edits the head's rope
;; (make-rope coerces); anchors and stack pass through, so the edit is safe until navigation lands it.
(define zipper-focus
  (local [(define (read z) (head-rope (zipper-head z)))
          (define (((swap c) smr guides) h k)          ; guides unused -- carries the slot for the lift's shape
            (match-let ([(head b _ a) h])
              (values (head b ((make-rope smr) c) a) k)))
          (define (set c) (zipper-lift (swap c)))
          (define ((modify f) z) ((set (f (read z))) z))]
    (match-lambda
      [(? zipper? z)    (read z)]
      [(? procedure? f) (modify f)]
      [c                (set c)])))

;; to-root: fold every crumb back into the head -- the focus becomes the whole document.
;; Deliberately OUTSIDE the lift: homing must not navigate back down; the guides survive.
(define (to-root z)
  (match-define (zipper smr gs h k) z)
  (zipper smr gs (foldl (lambda (crumb h) (crumb h)) h k) '()))

;; on-edges: the cursor's two edges as cuts, spread over f and g and combined by c (see
;; scribble). Each edge folds the focus onto the side it doesn't face, with the zipper's smr:
;;   ((on-edges c f g) z) = (c (f b (smr m a)) (g (smr b m) a))
;;                              '- left edge -'  '- right edge -'
(define ((on-edges c f g) z)
  (match-define (zipper smr _ (head b m a) _) z)
  (c (f b (smr m a)) (g (smr b m) a)))

;; ---------- printing ----------
;; The zipper prints as its document with the cursor marked inline:
;;   gap -> before‸after        seg -> before⟦focus⟧after
;; The pieces are read by RE-CUTTING the root with the installed guides -- which leans on
;; the guide-focus alignment every write maintains (see scribble; a guide-free
;; reconstruction off the crumbs was sketched and parked).
(define (zipper-show z port)
  (define gs   (zipper-guide z))
  (define smr  (zipper-smr z))
  (define root (zipper-focus (to-root z)))
  (let-values ([(b m a) ((multisect smr gs) root)])
    (if (equal? m (empty smr))
        (fprintf port "~a‸~a" b a)
        (fprintf port "~a⟦~a⟧~a" b m a))))

;; ---------- editing traces ----------
;; chain: pipe z0 through the commands, printing each command's source beside the zipper it
;; produces; returns the final zipper. Guide-AGNOSTIC like the rest of the file. The macro
;; captures the source (only a macro can), pairing it with the command for run-chain to thread.
(define (run-chain z0 steps)
  (printf "~a~a\n" (~a "(start)" #:min-width 30) z0)
  (for/fold ([z z0]) ([step (in-list steps)])
    (match-define (cons label cmd) step)
    (define z* (cmd z))
    (printf "~a~a\n" (~a (~v label) #:min-width 30) z*)
    z*))
(define-syntax-rule (chain z0 op ...)
  (run-chain z0 (list (cons (quote op) op) ...)))

;; ============================================================================
(module+ test
  (require rackunit)
  (define cc (make-summary string-length +))
  ;; char cursor: a boundary at offset n.  L = char count left of the cut (+1 = boundary right).
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define (gap n)   (vector (at n) (at n)))
  (define (seg i j) (vector (at i) (at j)))
  (define (gap? z)  (equal? (zipper-focus z) ((make-rope cc))))
  (define (doc z)   (~a (zipper-focus (to-root z))))
  (define delete (zipper-focus ""))
  (define rope ((make-rope cc) "hello world"))
  (define g0 (gap 0))                ; a fresh cursor: a caret at the start
  (define z0 (start cc rope g0))

  ;; the lens, read face: start carries its initial cursor; installing replaces it
  (check-eq? (zipper-guide z0) g0)
  (let ([g5 (gap 5)])
    (check-eq? (zipper-guide ((zipper-guide g5) z0)) g5))

  ;; gap: installing navigates to an empty focus; a swap at a gap = insert
  (let ([z ((zipper-guide (gap 5)) z0)])
    (check-true  (gap? z))
    (check-equal? (~a (zipper-focus z)) "")
    (check-equal? (doc ((zipper-focus "XYZ") z)) "helloXYZ world"))

  ;; seg: the slice is the focus; a write navigates to where the guide points
  ;; on the NEW text (char guides re-resolve by offset, hence "HI wo")
  (let ([z ((zipper-guide (seg 0 5)) z0)])
    (check-false (gap? z))
    (check-equal? (~a (zipper-focus z)) "hello")
    (let ([z* ((zipper-focus "HI") z)])
      (check-equal? (~a (zipper-focus z*)) "HI wo")
      (check-equal? (doc z*) "HI world"))
    (check-equal? (doc (delete z)) " world"))

  ;; the lens, modify face: f sees the old pair -- change one edge, keep the other
  (let* ([z  ((zipper-guide (seg 0 5)) z0)]
         [z* ((zipper-guide (lambda (gs) (vector (vector-ref gs 0) (at 11)))) z)])
    (check-equal? (~a (zipper-focus z*)) "hello world"))

  ;; a seg in the middle; wrapping is focus's modify face around the current focus
  (let ([z ((zipper-guide (seg 6 11)) z0)])
    (check-equal? (~a (zipper-focus z)) "world")
    (check-equal? (doc ((zipper-focus (lambda (m) ((make-rope cc) "[" m "]"))) z)) "hello [world]"))

  ;; writes compose: one navigate-edit-home pipeline
  (check-equal? (~a (zipper-focus ((compose to-root (zipper-focus "HI") (zipper-guide (seg 0 5))) z0)))
                "HI world")

  ;; --- on-edges: each edge cut through its own function, results combined ---
  (let ([z ((zipper-guide (seg 6 11)) z0)])                          ; focus "world"
    (check-equal? ((on-edges list list list) z) '((6 5) (11 0)))   ; b | m+a . b+m | a
    (check-equal? ((on-edges + - -) z) (+ (- 6 5) (- 11 0))))      ; spread-combine shape
  (let ([z ((zipper-guide (gap 5)) z0)])                             ; a gap: both edges agree
    (check-equal? ((on-edges list list list) z) '((5 6) (5 6))))

  ;; --- printing: a zipper displays as its marked document ---
  (check-equal? (~a ((zipper-guide (gap 5)) z0)) "hello‸ world")     ; gap = caret
  (check-equal? (~a ((zipper-guide (seg 0 5)) z0)) "⟦hello⟧ world")  ; seg = bracketed focus
  (check-equal? (~a z0) "‸hello world")                       ; z0's own cursor: caret at the start
  (check-equal? (~a ((zipper-focus "HI") ((zipper-guide (seg 0 5)) z0)))    ; navigated cursor shows
                "⟦HI wo⟧rld")
  (let ([z ((zipper-guide (gap 6)) (start cc ((make-rope cc) "ab\ncd\nef") (gap 0)))])
    (check-equal? (~a z) "ab\ncd\n‸ef"))                      ; marks sit at the cut, multi-line

  ;; --- crossing guard: a cursor whose end precedes its start is rejected at
  ;; navigation by the `uncrossed` stage (after `ascend`, before `descend`).
  (check-exn #rx"crossed cursor" (lambda () ((zipper-guide (seg 5 2)) z0)))   ; start past end
  (check-not-exn (lambda () ((zipper-guide (gap 5)) z0)))     ; a gap (start = end) is not crossed
  (check-not-exn (lambda () ((zipper-guide (seg 2 5)) z0))))  ; an ordered seg is not crossed
