#lang racket

;; Sexp navigation + editing on the summarised rope, on SIGNED SPINE indexes.
;;
;; An index is a spine: the per-level position list, innermost-first, read straight
;; off the signed frontier summary (sexp-summary.rkt) -- (opens ++ [forms]) of the
;; before summary for a FRONT index (positive components, depends only on the text
;; to its left), (closes ++ [-(forms+1)]) of the after summary for a BACK index
;; (negative, depends only on the text to its right).  The sign of the head picks
;; the side; front and back are the two ANCHORS of the same position, and re-deriving
;; the other side's spine at a cursor IS the anchor flip.
;;
;; `fine@` reads both spines at a cut, with the ½ refinement on the HEAD only:
;; at a form start the head is the raw integer; mid-atom it is pushed half-way into
;; the atom (the one structurally invisible interior -- frames' interiors are spine-
;; visible as depth, atoms' are not); other cuts lean -½ before the next start.
;; Under completion counting an open frame's interior is a prefix extension of the
;; frame's own start slot, so `spine-cmp` is NAIVELY lexicographic: pad with -inf
;; (a bare spine bottoms out before everything deeper) and take the first non-zero
;; componentwise verdict.  Targets land exactly on form starts (the only integer
;; heads); a guide is one comparison.
;;
;; The layer is three reads and a comparison: everything else is zipper-core.

(require racket/match
         srfi/41                ; variadic lazy streams: stream-map/filter/constant
         "rope-core.rkt"        ; make-rope multisect frame
         "sexp-summary.rkt"     ; sexp-smr + the frontier readers
         "zipper-core.rkt")     ; start guide focus to-root on-edges

(provide fine@                  ; (fine@ L R) -> (values front-spine back-spine)
         spine-cmp              ; (spine-cmp cut target) -> -1 | 0 | 1
         slot-guide             ; (slot-guide index) -> guide, sign-dispatched
         moduli base-left base-right flip   ; re-basing: the flip as data
         sexp-guides cursor     ; cursor conveniences over zipper-core
         edge-contexts anchors edge-moduli
         re-anchor cover        ; the anchor flip as a cursor operation
         (all-from-out "rope-core.rkt")
         (all-from-out "sexp-summary.rkt")
         (all-from-out "zipper-core.rkt"))

;; ---------- the cut reads ----------
;; both full spines at a cut, innermost-first, ½ baked into the heads.
(define (fine@ L R)
  (define mid?   (and (sexp-ends-atom? L) (sexp-starts-atom? R)))
  (define start? (and (sexp-starts-form? R) (not mid?)))
  (match-define (cons fh fr) (append (sexp-opens  L) (list (sexp-forms L))))
  (match-define (cons bh br) (append (sexp-closes R) (list (- (add1 (sexp-forms R))))))
  (values (cons (+ fh (if start? 0 -1/2)) fr)
          (cons (+ bh (cond [start? 0] [mid? 1/2] [else -1/2])) br)))

;; ---------- the comparison ----------
;; spine -> outermost-first component stream, -inf forever after (a bare spine --
;; the frame's own start slot -- sits before everything deeper inside it).
(define (spine->stream s)
  (stream-append (list->stream (reverse s)) (stream-constant -inf.0)))

(define (component-cmp a b) (cond [(= a b) 0] [(< a b) 1] [else -1]))

;; equal spines answer immediately; otherwise the first non-zero verdict exists at
;; a finite position, so the lazy search terminates.
(define (spine-cmp cut target)            ; -> +1 boundary right of cut / 0 / -1
  (if (equal? cut target)
      0
      (stream-car (stream-filter (negate zero?)
                                 (stream-map component-cmp
                                             (spine->stream cut)
                                             (spine->stream target))))))

;; ---------- the guide ----------
;; one comparison; the index's sign picks which side of the cut is read.  back
;; components are always <= -1, so `negative?` on the head dispatches.
(define ((slot-guide ix) L R)
  (define-values (front back) (fine@ L R))
  (spine-cmp (if (negative? (car ix)) back front) ix))

;; ---------- cursor conveniences ----------
(define (sexp-guides s [e s]) (vector (slot-guide s) (slot-guide e)))
(define (cursor rope s [e s]) ((guide (sexp-guides s e)) (start sexp-smr rope)))

;; ---------- anchors ----------
;; each edge of the focus folds the focus to the other side; a gap (empty focus)
;; collapses both to before | after.
(define (edge-contexts z i)               ; i: 0 = start edge, 1 = end edge
  ((on-edges (lambda (e0 e1) (apply values (if (zero? i) e0 e1))) list list) z))

;; the two anchor indexes of edge i: front (left-anchored, off the before side) and
;; back (right-anchored, off the after side).  Same position now; under edits each
;; follows its own side.  Re-navigating with the other one IS the flip.
(define (anchors z i)
  (define-values (L R) (edge-contexts z i))
  (fine@ L R))

;; ---------- re-basing ----------
;; the per-level moduli of a cut, innermost-first: front - back = N+2 at every
;; level and every cut (the uniformity bar), so the difference is an integer
;; list constant along the cut's frame path -- exactly the data a flip needs,
;; and exactly what one index alone cannot know.
(define (moduli L R)
  (define-values (f b) (fine@ L R))
  (map - f b))

;; re-base an index into a family, given the moduli of its own cut; the head's
;; sign dispatches.  The shift is componentwise, so the ½ heads carry for free.
(define ((base-left ms) ix)              ; -> front family
  (if (negative? (car ix)) (map + ix ms) ix))
(define ((base-right ms) ix)             ; -> back family
  (if (negative? (car ix)) ix (map - ix ms)))
(define ((flip ms) ix)                   ; the other family; an involution
  (((if (negative? (car ix)) base-left base-right) ms) ix))

(define (edge-moduli z i)                ; the moduli at edge i, staged like anchors
  (define-values (L R) (edge-contexts z i))
  (moduli L R))

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

  ;; --- guide sign sweeps: every interior cut x every target, both sides ---
  (define (sweep str targets)
    (for ([at targets])
      (for ([ix (list (front-of str at) (back-of str at))])
        (for ([i (in-range 1 (string-length str))])
          (define-values (L R) (sides str i))
          (check-equal? ((slot-guide ix) L R)
                        (cond [(< i at) 1] [(= i at) 0] [else -1])
                        (format "~s target ~a ix ~s cut ~a" str at ix i))))))
  (sweep "(aa bb cc)"    '(4 7))          ; ^bb ^cc
  (sweep "(aa (p q) cc)" '(4 7 10))       ; ^(p q) ^q ^cc -- nesting, incl. the )^ cuts

  ;; --- the spines themselves, for the record ---
  (check-equal? (front-of "(aa bb cc)" 4) '(2 0))
  (check-equal? (back-of  "(aa bb cc)" 4) '(-3 -2))
  (check-equal? (front-of "(aa (p q) cc)" 7) '(2 2 0))
  (check-equal? (back-of  "(aa (p q) cc)" 7) '(-2 -3 -2))

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

  ;; --- navigation + editing, nested ---
  (define rope2 ((make-rope sexp-smr) "(aa (p q) cc)"))
  (check-equal? (doc ((focus "xx ") (cursor rope2 (front-of "(aa (p q) cc)" 7))))
                "(aa (p xx q) cc)")

  ;; --- anchors: read both at the cursor, and they diverge under editing ---
  (let*-values ([(z) (cursor rope ^bb)]
                [(front back) (anchors z 0)])
    (check-equal? front '(2 0))
    (check-equal? back  '(-3 -2))
    (define rope1 (focus (to-root ((focus "xx ") z))))
    (check-equal? (cuts rope1 front) (cons "(aa " "xx bb cc)"))  ; left-anchored: stays by aa
    (check-equal? (cuts rope1 back)  (cons "(aa xx " "bb cc)"))) ; right-anchored: stays by bb

  ;; --- re-basing: every cut (incl. mid-atom ½s and whitespace) -- moduli are
  ;; integral, basing reproduces the fine@ reads, flip is an involution ---
  (for ([str '("(aa bb cc)" "(aa (p q) cc)" "((a) (b (c)) d)" "aa bb cc")])
    (for ([i (in-range 1 (string-length str))])
      (define-values (L R) (sides str i))
      (define-values (f b) (fine@ L R))
      (define ms (moduli L R))
      (check-true (andmap exact-integer? ms) (format "~s cut ~a moduli ~s" str i ms))
      (check-equal? ((base-right ms) f) b (format "~s cut ~a -> back" str i))
      (check-equal? ((base-left ms) b) f (format "~s cut ~a -> front" str i))
      (check-equal? ((flip ms) ((flip ms) f)) f (format "~s cut ~a involution" str i))))

  ;; --- the moduli themselves: N+2 per level ---
  (check-equal? (let-values ([(L R) (sides "(aa bb cc)" 4)]) (moduli L R))
                '(5 2))                   ; 3 forms in the frame +2 . 0 top-level +2
  (check-equal? (let-values ([(L R) (sides "(aa (p q) cc)" 7)]) (moduli L R))
                '(4 5 2))

  ;; --- a flipped index is a real index: same gap now, the other anchor after ---
  (let* ([z     (cursor rope ^bb)]
         [back* ((flip (edge-moduli z 0)) ^bb)])
    (check-equal? back* (back-of "(aa bb cc)" 4))
    (check-equal? (doc ((focus "xx ") (cursor rope back*))) "(aa xx bb cc)")
    (define rope1 (focus (to-root ((focus "xx ") z))))
    (check-equal? (cuts rope1 ^bb)   (cons "(aa " "xx bb cc)"))
    (check-equal? (cuts rope1 back*) (cons "(aa xx " "bb cc)")))

  ;; --- the guide lens: an anchor flip installs and re-navigates to the SAME
  ;; gap (L2); editing afterwards behaves as the flipped family ---
  (let* ([z  (cursor rope ^bb)]
         [zb ((guide (sexp-guides ((flip (edge-moduli z 0)) ^bb))) z)])
    (check-equal? (~a (focus zb)) "")                          ; same gap at flip-time
    (check-equal? (doc ((focus "xx ") zb)) "(aa xx bb cc)"))

  ;; --- re-anchoring the END edge via the lens's modify face makes the cursor
  ;; edit-stable: it keeps covering the focus across replace and delete ---
  (let* ([z  (cursor rope ^bb ^cc)]                            ; both front: end drifts under edits
         [e* ((flip (edge-moduli z 1)) ^cc)]                   ; re-anchor the end on its right side
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
