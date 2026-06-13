#lang racket

;; Sexp navigation + editing on the summarised rope, on SIGNED SPINE indexes.
;;
;; An index is a spine: the per-level position list, innermost-first, read straight
;; off the signed frontier summary (sexp-summary.rkt).  Slots are 0-BASED at every
;; level.  EACH COMPONENT picks the side it reads at its level: >= -1/2 against
;; the 0-based slot read (sub1'd opens ++ [forms]) of the before summary
;; (left-based, from the text to the left), <= -1 against (closes ++ [-(forms+1)])
;; of the after summary (right-based, from the text to the right; -1 = after the
;; last form).  The two ANCHORS of a position differ in the HEAD only: the path
;; down to the cut's frame is a left-based name either way, and the head is
;; anchored left or right within that frame -- `anchors` returns exactly this
;; pair, and re-deriving the other head at a cursor IS the anchor flip
;; (head-only, by one modulus).
;;
;; `fine@` reads the all-left and all-right spines at a cut, with the ½ refinement
;; on the HEAD only: at a form start the head is the raw integer; mid-atom it is
;; pushed half-way into the atom (the one structurally invisible interior --
;; frames' interiors are spine-visible as depth, atoms' are not); other cuts lean
;; -½ before the next start.  Under completion counting an open frame's interior
;; is a prefix extension of the frame's own start slot, so `spine-cmp` is NAIVELY
;; lexicographic: pad with -inf (a bare spine bottoms out before everything
;; deeper), read each level off the spine the index's component selects, and take
;; the first non-zero componentwise verdict.  Targets land on form starts AND on
;; frames' END SLOTS -- slot N of an N-child frame: the end region (only
;; whitespace between the cut and the close; raw back head -1, unconfusable with
;; mid-atom, whose straddled atom pushes the close entry to <= -2) reads integer
;; on both sides.  A guide is one comparison.
;;
;; The layer is three reads and a comparison: everything else is zipper-core.

(require racket/match
         srfi/41                ; variadic lazy streams: stream-map/constant/->list
         "rope-core.rkt"        ; make-rope multisect frame
         "sexp-summary.rkt"     ; sexp-smr + the frontier readers
         "zipper-core.rkt")     ; start guide focus to-root on-edges

(provide fine@                  ; (fine@ L R) -> (values front-spine back-spine)
         spine-cmp              ; (spine-cmp front back index) -> -1 | 0 | 1
         slot-guide             ; (slot-guide index) -> guide; components pick their side
         modulus base-left base-right flip  ; re-basing: head-only, by one modulus
         sexp-guides cursor     ; cursor conveniences over zipper-core
         edge-contexts anchors edge-modulus
         re-anchor cover        ; the anchor flip as a cursor operation
         (all-from-out "rope-core.rkt")
         (all-from-out "sexp-summary.rkt")
         (all-from-out "zipper-core.rkt"))

;; ---------- the cut reads ----------
;; both full spines at a cut, innermost-first, ½ baked into the heads; front
;; slots 0-based (the stored +1 drops at the read), back as stored.
(define (fine@ L R)
  (define mid?   (and (sexp-ends-atom? L) (sexp-starts-atom? R)))
  (define start? (and (sexp-starts-form? R) (not mid?)))
  (match-define (cons fh fr) (append (map sub1 (sexp-opens L)) (list (sexp-forms L))))
  (match-define (cons bh br) (append (sexp-closes R) (list (- (add1 (sexp-forms R))))))
  (define end? (= bh -1))    ; nothing but whitespace before the close: the end slot
  (values (cons (+ fh (cond [(or start? end?) 0] [else -1/2])) fr)
          (cons (+ bh (cond [(or start? end?) 0] [mid? 1/2] [else -1/2])) br)))

;; ---------- the comparison ----------
;; spine -> outermost-first component stream, -inf forever after (a bare spine --
;; the frame's own start slot -- sits before everything deeper inside it).
(define (spine->stream s)
  (stream-append (list->stream (reverse s)) (stream-constant -inf.0)))

(define (component-cmp a b) (cond [(= a b) 0] [(< a b) 1] [else -1]))

;; the family tag: back components sit at <= -1, front at >= -1/2 (the -1/2 is
;; a leaned head in a frame with no child counted yet -- whitespace right after
;; an open paren).  The half-step gap keeps the families disjoint.
(define (back-component? c) (< c -1/2))

;; each component of the index picks the spine it reads against -- per-level
;; anchoring, so all-left, all-right, and mixed indexes resolve uniformly.
(define (pick-cmp f b c) (component-cmp (if (back-component? c) b f) c))

;; the verdict stream is read one past the longer spine: both sides are -inf
;; padding from there on, so an all-zero prefix means the cut IS the target.
(define (spine-cmp front back ix)         ; -> +1 boundary right of cut / 0 / -1
  (define n (add1 (max (length front) (length ix))))
  (or (findf (negate zero?)
             (stream->list n (stream-map pick-cmp
                                         (spine->stream front)
                                         (spine->stream back)
                                         (spine->stream ix))))
      0))

;; ---------- the guide ----------
;; one comparison; each component of the index picks its own side, split by
;; `back-component?` at the half-step gap (front >= -1/2, back <= -1, leans
;; included).
(define ((slot-guide ix) L R)
  (define-values (front back) (fine@ L R))
  (spine-cmp front back ix))

;; ---------- cursor conveniences ----------
(define (sexp-guides s [e s]) (vector (slot-guide s) (slot-guide e)))
(define (cursor rope s [e s]) ((guide (sexp-guides s e)) (start sexp-smr rope)))

;; ---------- anchors ----------
;; each edge of the focus folds the focus to the other side; a gap (empty focus)
;; collapses both to before | after.
(define (edge-contexts z i)               ; i: 0 = start edge, 1 = end edge
  ((on-edges (lambda (e0 e1) (apply values (if (zero? i) e0 e1))) list list) z))

;; the two anchor indexes of edge i: the same left-based path, the head anchored
;; left (off the before side) or right (off the after side).  Same position now;
;; under edits within the frame each head follows its own side.  Re-navigating
;; with the other one IS the flip.
(define (anchors z i)
  (define-values (L R) (edge-contexts z i))
  (define-values (f b) (fine@ L R))
  (values f (cons (car b) (cdr f))))

;; ---------- re-basing ----------
;; the modulus of a cut: front head - back head = N+1 at the cut's own level
;; (the uniformity bar; encoding A's modulus over N forms).  Only the head
;; re-bases -- the path components are a left-based name shared by both
;; anchorings -- so the flip data is one number, exactly what one index alone
;; cannot know.
(define (modulus L R)
  (define-values (f b) (fine@ L R))
  (- (car f) (car b)))

;; re-base an index's head, given the modulus of its own cut; `back-component?`
;; dispatches on the head.  The shift is head-only, so the ½ heads carry for free.
(define ((base-left m) ix)               ; -> left-based head
  (if (back-component? (car ix)) (cons (+ (car ix) m) (cdr ix)) ix))
(define ((base-right m) ix)              ; -> right-based head
  (if (back-component? (car ix)) ix (cons (- (car ix) m) (cdr ix))))
(define ((flip m) ix)                    ; the other anchoring; an involution
  (((if (back-component? (car ix)) base-left base-right) m) ix))

(define (edge-modulus z i)               ; the modulus at edge i, staged like anchors
  (define-values (L R) (edge-contexts z i))
  (modulus L R))

;; ---------- re-anchoring ----------
;; install edge i's guide re-derived from the chosen side's anchor ('front |
;; 'back), through the guide accessor's modify face: the anchor flip as a
;; cursor operation.  Same position now; the family decides how the edge
;; follows future edits.
(define (re-anchor z i side)
  (define-values (front back) (anchors z i))
  (define g (slot-guide (if (eq? side 'front) front back)))
  ((guide (lambda (gs)
            (if (zero? i)
                (vector g (vector-ref gs 1))
                (vector (vector-ref gs 0) g))))
   z))

;; cover: flip the SECOND guide onto its right anchor (the start already reads
;; the left).  An edit between the edges then touches neither anchor's side,
;; so the cursor keeps covering whatever replaces the focus.
(define (cover z) (re-anchor z 1 'back))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; --- helpers: read spines and indexes straight off string cuts ---
  (define (sides str i) (values (sexp-smr (substring str 0 i)) (sexp-smr (substring str i))))
  (define (front-of str i) (let-values ([(L R) (sides str i)])
                             (let-values ([(f b) (fine@ L R)]) f)))
  (define (back-of  str i) (let-values ([(L R) (sides str i)])
                             (let-values ([(f b) (fine@ L R)]) b)))

  ;; --- guide sign sweeps: every interior cut x every target, all-left,
  ;; all-right, AND the mixed anchor (right head over the left path) ---
  (define (sweep str targets)
    (for ([at targets])
      (define-values (f b) (let-values ([(L R) (sides str at)]) (fine@ L R)))
      (for ([ix (list f b (cons (car b) (cdr f)))])
        (for ([i (in-range 1 (string-length str))])
          (define-values (L R) (sides str i))
          (check-equal? ((slot-guide ix) L R)
                        (cond [(< i at) 1] [(= i at) 0] [else -1])
                        (format "~s target ~a ix ~s cut ~a" str at ix i))))))
  (sweep "(aa bb cc)"    '(4 7 9))        ; ^bb ^cc, and the end slot before ")"
  (sweep "(aa (p q) cc)" '(4 7 8 10))     ; ^(p q) ^q, (p q)'s end slot, ^cc
  (sweep "( aa)"         '(2 4))          ; ^aa behind leading ws (-1/2 region) + end slot

  ;; --- the spines themselves, for the record ---
  (check-equal? (front-of "(aa bb cc)" 4) '(1 0))
  (check-equal? (back-of  "(aa bb cc)" 4) '(-3 -2))
  (check-equal? (front-of "(aa (p q) cc)" 7) '(1 1 0))
  (check-equal? (back-of  "(aa (p q) cc)" 7) '(-2 -3 -2))
  (check-equal? (front-of "( aa)" 2) '(0 0))           ; slots are 0-based
  (check-equal? (front-of "(aa bb cc)" 9) '(3 0))      ; the end slot: slot N
  (check-equal? (back-of  "(aa bb cc)" 9) '(-1 -2))    ; -1 = after last, in storage
  (check-equal? (front-of "(aa (p q) cc)" 8) '(2 1 0)) ; (p q)'s end slot

  ;; --- navigation + editing, flat ---
  (define (cuts rope ix)                  ; where a single index cuts, as strings
    (let-values ([(l r) ((multisect (vector (slot-guide ix))) rope)])
      (cons (~a l) (~a r))))
  (define (doc z) (~a (focus (to-root z))))
  (define rope ((make-rope sexp-smr) "(aa bb cc)"))
  (define ^bb (front-of "(aa bb cc)" 4))
  (define ^cc (front-of "(aa bb cc)" 7))

  (let ([z (cursor rope ^bb)])                       ; gap: replace at an empty focus = insert
    (check-equal? (~a (focus z)) "")
    (check-equal? (doc ((focus "xx ") z)) "(aa xx bb cc)"))

  (let ([z (cursor rope ^bb ^cc)])                   ; seg [^bb ^cc): half-open
    (check-equal? (~a (focus z)) "bb ")
    (check-equal? (doc ((focus "XX ") z)) "(aa XX cc)")
    (check-equal? (doc ((focus "") z)) "(aa cc)")) ; the empty replace = delete

  ;; back index navigates to the same place
  (check-equal? (doc ((focus "xx ") (cursor rope (back-of "(aa bb cc)" 4))))
                "(aa xx bb cc)")

  ;; --- the end slot: slot N of an N-child frame is a real target; both
  ;; anchorings name it (front N | right -1) and appending lands tight after
  ;; the last child ---
  (check-equal? (doc ((focus " dd") (cursor rope '(3 0))))  "(aa bb cc dd)")
  (check-equal? (doc ((focus " dd") (cursor rope '(-1 0)))) "(aa bb cc dd)")
  (let ([z (cursor ((make-rope sexp-smr) "(aa )") '(1 0))]) ; ws after the last child:
    (check-equal? (~a z) "(aa‸ )"))                         ; the plateau lands at its left edge

  ;; --- navigation + editing, nested ---
  (define rope2 ((make-rope sexp-smr) "(aa (p q) cc)"))
  (check-equal? (doc ((focus "xx ") (cursor rope2 (front-of "(aa (p q) cc)" 7))))
                "(aa (p xx q) cc)")

  ;; --- anchors: read both at the cursor, and they diverge under editing ---
  (let*-values ([(z) (cursor rope ^bb)]
                [(front back) (anchors z 0)])
    (check-equal? front '(1 0))
    (check-equal? back  '(-3 0))                       ; right head over the left path
    (define rope1 (focus (to-root ((focus "xx ") z))))
    (check-equal? (cuts rope1 front) (cons "(aa " "xx bb cc)"))  ; left-anchored: stays by aa
    (check-equal? (cuts rope1 back)  (cons "(aa xx " "bb cc)"))) ; right-anchored: stays by bb

  ;; --- re-basing: every cut (incl. mid-atom ½s, whitespace, and the -1/2 head
  ;; after an open paren) -- the modulus is integral, head-basing reproduces the
  ;; anchor pair, flip is an involution ---
  (for ([str '("(aa bb cc)" "(aa (p q) cc)" "((a) (b (c)) d)" "aa bb cc"
               "( aa)" "(aa )")])
    (for ([i (in-range 1 (string-length str))])
      (define-values (L R) (sides str i))
      (define-values (f b) (fine@ L R))
      (define m (modulus L R))
      (define mixed (cons (car b) (cdr f)))            ; the right-headed anchor
      (check-true (exact-integer? m) (format "~s cut ~a modulus ~s" str i m))
      (check-equal? ((base-right m) f) mixed (format "~s cut ~a -> right head" str i))
      (check-equal? ((base-left m) mixed) f (format "~s cut ~a -> left head" str i))
      (check-equal? ((flip m) ((flip m) f)) f (format "~s cut ~a involution" str i))))

  ;; --- the modulus itself: N+1 at the cut's own level ---
  (check-equal? (let-values ([(L R) (sides "(aa bb cc)" 4)]) (modulus L R))
                4)                        ; 3 forms in the frame +1
  (check-equal? (let-values ([(L R) (sides "(aa (p q) cc)" 7)]) (modulus L R))
                3)                        ; 2 forms in (p q) +1

  ;; --- a flipped index is a real index: same gap now, the other anchor after ---
  (let* ([z     (cursor rope ^bb)]
         [back* ((flip (edge-modulus z 0)) ^bb)])
    (check-equal? back* '(-3 0))                       ; right head, path untouched
    (check-equal? (doc ((focus "xx ") (cursor rope back*))) "(aa xx bb cc)")
    (define rope1 (focus (to-root ((focus "xx ") z))))
    (check-equal? (cuts rope1 ^bb)   (cons "(aa " "xx bb cc)"))
    (check-equal? (cuts rope1 back*) (cons "(aa xx " "bb cc)")))

  ;; --- the guide lens: an anchor flip installs and re-navigates to the SAME
  ;; gap (L2); editing afterwards behaves as the flipped family ---
  (let* ([z  (cursor rope ^bb)]
         [zb ((guide (sexp-guides ((flip (edge-modulus z 0)) ^bb))) z)])
    (check-equal? (~a (focus zb)) "")                          ; same gap at flip-time
    (check-equal? (doc ((focus "xx ") zb)) "(aa xx bb cc)"))

  ;; --- re-anchoring the END edge via the lens's modify face makes the cursor
  ;; edit-stable: it keeps covering the focus across replace and delete ---
  (let* ([z  (cursor rope ^bb ^cc)]                            ; both front: end drifts under edits
         [e* ((flip (edge-modulus z 1)) ^cc)]                  ; re-anchor the end's head on its right side
         [z  ((guide (lambda (gs) (vector (vector-ref gs 0) (slot-guide e*)))) z)])
    (check-equal? (~a (focus z)) "bb ")                        ; same seg at flip-time
    (let ([z* ((focus "x1 x2 ") z)])
      (check-equal? (~a (focus z*)) "x1 x2 ")                  ; replace re-navigated: still covering
      (check-equal? (doc z*) "(aa x1 x2 cc)")
      (let ([zg ((focus "") z*)])                            ; delete collapses to the gap
        (check-equal? (~a (focus zg)) "")
        (check-equal? (doc ((focus "yy ") zg)) "(aa yy cc)")))) ; and editing chains on

  ;; --- cover: flip the second guide; the right anchor stays fixed while the
  ;; stuff inside is edited ---
  (let* ([z  (cover (cursor rope2 (front-of "(aa (p q) cc)" 7)))] ; covered gap at ^q
         [z1 ((focus "x ") z)]
         [z2 ((focus "x y ") z1)]
         [z3 ((focus "") z2)])
    (check-equal? (~a (focus z1)) "x ")
    (check-equal? (doc z1) "(aa (p x q) cc)")
    (check-equal? (~a (focus z2)) "x y ")
    (check-equal? (doc z2) "(aa (p x y q) cc)")    ; the interior grew: q held its ground
    (check-equal? (~a (focus z3)) "")
    (check-equal? (doc z3) "(aa (p q) cc)"))       ; and shrank back to the gap

  (let* ([z (cover (cursor rope ^bb ^cc))])        ; a seg, covered by the same mechanism
    (check-equal? (~a (focus z)) "bb ")
    (check-equal? (doc ((focus "b1 (b2 b3) ") z)) "(aa b1 (b2 b3) cc)")))
