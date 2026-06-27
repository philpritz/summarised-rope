#lang racket

;; Sexp navigation + editing on the summarised rope, as a layer over zipper-core.
;; An index is a SIGNED SPINE -- the per-level slot list, innermost-first, each
;; component picking the side it reads (front >= -1/2, back <= -1, the pair
;; `sand-spines` reads at a cut). The layer is one thing: a comparison over those
;; spines (`spine-cmp`); everything else is zipper-core. The surface:
;;   spine-cmp        front back index -> -1 | 0 | 1   -- the comparison (one guide's verdict)
;;   slot-guide       index -> guide   -- a guide that also reads back as its index
;;   modulus + base-left/base-right/flip  -- re-basing one index's head between its two anchors
;;   cursor sexp-guides anchors re-anchor cover  -- place / re-anchor a cursor
;;   edge-guide edge-index                       -- lenses onto one cursor edge
;;   at move edge each both                      -- edit verbs: zipper -> zipper commands
;; Index model, the two anchors, the lexicographic comparison, and the lens style:
;; scribble/sexp-edit.scrbl.

(require racket/match
         "rope-core.rkt"        ; make-rope multisect frame
         "summaries/sexp-summary.rkt"  ; sexp-smr, sand-spines
         "summaries/summaries.rkt"     ; bundle
         "zipper-core.rkt"      ; start zipper-guide zipper-edge zipper-focus to-root on-edges
         "helper-algebras.rkt") ; on; lexicographic; make-lens/list-of/lref/ldiag (lens vocab)

(provide spine-cmp              ; (spine-cmp front back index) -> -1 | 0 | 1
         slot-guide guide-index ; (slot-guide index) -> guide (carries its index)
         modulus base-left base-right flip  ; re-basing: head-only, by one modulus
         sexp-guides cursor     ; cursor conveniences over zipper-core
         edge-contexts anchors edge-modulus
         re-anchor cover        ; the anchor flip as a cursor operation
         edge-guide edge-index  ; lenses onto one cursor edge (its guide / its index)
         at move edge each both slot  ; edit verbs: zipper -> zipper commands (ride `idxs`)
         (all-from-out "rope-core.rkt")
         (all-from-out "summaries/sexp-summary.rkt")
         (all-from-out "summaries/summaries.rkt")
         (all-from-out "zipper-core.rkt"))

;; ---------- the comparison ----------
(define (component-cmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))

;; the family tag: back at <= -1, front at >= -1/2; the half-step gap keeps them
;; disjoint, so a leaned head (-1/2, ws after an open paren) still classes front.
(define (back-component? c) (< c -1/2))

;; an index component vs a cut element -- its (front . back) pair; the component
;; picks its own side, so all-left/all-right/mixed resolve uniformly. Index first,
;; so +1 = target right of the cut.
(define (cut-cmp c fb) (component-cmp c (if (back-component? c) (cdr fb) (car fb))))

;; zip the co-indexed spines into one cut, compare an index against it
;; lexicographically, outermost-first (why naively lexicographic: scribble).
(define (spine-cmp front back ix)         ; -> +1 boundary right of cut / 0 / -1
  ((lexicographic cut-cmp) (reverse ix) (reverse (map cons front back))))

;; ---------- the guide ----------
;; A guide is BOTH callable as the comparator and readable as its index, so the
;; edit verbs reach the indices through `idxs` without re-deriving from the zipper.
(struct guide (index proc) #:property prop:procedure (struct-field-index proc))
(define (slot-guide ix)
  (guide ix (lambda (L R)
              (let-values ([(front back) ((on sand-spines sexp-smr) L R)])
                (spine-cmp front back ix)))))

;; ---------- cursor conveniences ----------
(define (sexp-guides s [e s]) (list (slot-guide s) (slot-guide e)))   ; the guide list (gs ge)
(define (cursor rope s [e s])                        ; place the cursor, then navigate to it
  (let ([p (sexp-guides s e)])
    ((setter zipper-guide p) (start sexp-smr rope (first p) (second p)))))

;; ---------- anchors ----------
;; each edge folds the focus to the other side (a gap collapses both to before | after).
(define (edge-contexts z i)               ; i: 0 = start edge, 1 = end edge
  ((on-edges (lambda (e0 e1) (apply values (if (zero? i) e0 e1))) list list) z))

;; the two anchor indexes of edge i: same left-based path, head anchored left or
;; right -- the right-headed one being (car b) over the left path. Why two: scribble.
(define (anchors z i)
  (define-values (L R) (edge-contexts z i))
  (define-values (f b) ((on sand-spines sexp-smr) L R))
  (values f (cons (car b) (cdr f))))

;; ---------- re-basing ----------
;; the cut's re-basing constant: front head - back head = N+1 at its level. The
;; whole of the flip data, since only the head re-bases. Why one number: scribble.
(define (modulus L R)
  (define-values (f b) ((on sand-spines sexp-smr) L R))
  (- (car f) (car b)))

;; re-base an index's head, `back-component?` dispatching; head-only, so the ½
;; heads and the path carry for free.
(define ((base-left m) ix)               ; -> left-based head
  (if (back-component? (car ix)) (cons (+ (car ix) m) (cdr ix)) ix))
(define ((base-right m) ix)              ; -> right-based head
  (if (back-component? (car ix)) ix (cons (- (car ix) m) (cdr ix))))
(define ((flip m) ix)                    ; the other anchoring; an involution
  (((if (back-component? (car ix)) base-left base-right) m) ix))

(define (edge-modulus z i)               ; the modulus at edge i, staged like anchors
  (define-values (L R) (edge-contexts z i))
  (modulus L R))

;; ---------- edge lenses ----------
;; `zipper-edge` (zipper-core) lenses onto one cursor edge's guide; one hop more,
;; through `index-of`, lands on its index. A write through the composite re-navigates once.
(define index-of (make-lens (lambda (g) (values slot-guide (guide-index g)))))  ; lens: a guide <-> its index
(define (edge-guide i) (zipper-edge i))                     ; -> the guide at edge i
(define (edge-index i) (compose (zipper-edge i) index-of))  ; -> its index

;; ---------- re-anchoring ----------
;; install edge i's guide from the chosen side's anchor -- the anchor flip as a
;; cursor operation. Same position now; the family decides how it follows edits.
(define (re-anchor z i side)
  (define-values (front back) (anchors z i))
  (define g (slot-guide (if (eq? side 'front) front back)))
  ((setter (edge-guide i) g) z))

;; cover: flip the end onto its right anchor (the start already reads the left), so
;; an edit between the edges touches neither anchor's side and the cursor keeps covering.
(define (cover z) (re-anchor z 1 'back))

;; ---------- command vocabulary ----------
;; The verbs ride `idxs` (zipper <-> the index list), then a selector lens at the tail picks the
;; reach: `(ldiag i)` collapses to position i, `(lref i)` singles one edge, no selector keeps the
;; list. Each is one setter/updater that re-navigates once. Point-free style, the reach per verb:
;; scribble.
(define idxs (compose zipper-guide (list-of index-of)))  ; zipper <-> (list ix0 ix1)

(define ((slot h) ix) (cons (h (car ix)) (cdr ix)))      ; map the innermost slot; frame (cdr) untouched

(define (at   ix)    (setter  (compose idxs (ldiag 0)) ix))                                    ; absolute gap at ix
(define (move f)     (updater (compose idxs (ldiag 0)) f))                                     ; gap, f on the basis
(define (edge i f)   (updater (compose idxs (lref i)) f))                                      ; one edge (0 start, 1 end)
(define (each fl fr) (updater idxs (lambda (xs) (map (lambda (g x) (g x)) (list fl fr) xs))))  ; a fn per edge
(define (both f)     (updater idxs (lambda (xs) (map f xs))))                                  ; same fn over both

;; ============================================================================
;; Tree generators -- in their own submodule, so they import without dragging in
;; the test suite (a `module+ gen` is instantiated only when explicitly required).
;; gen:shape draws bare nesting (bounded by max-kids/depth); gen:populate fills it
;; with atoms; tree->text renders a populated tree to sexp source.
(module+ gen
  (require rackcheck)
  (provide gen:shape gen:atom gen:populate tree->text)
  (define (gen:shape max-kids depth)
    (if (zero? depth) (gen:const '())
        (gen:list (gen:shape max-kids (sub1 depth)) #:max-length max-kids)))
  (define gen:atom
    (gen:let ([c gen:char-letter] [cs (gen:string gen:char-letter #:max-length 3)])
      (string-append (string c) cs)))
  (define (gen:populate shape)
    (if (null? shape) gen:atom (apply gen:tuple (map gen:populate shape))))
  (define (tree->text t)
    (if (string? t) t
        (string-append "(" (string-join (map tree->text t) " ") ")"))))

;; ============================================================================
(module+ test
  (require rackunit rackcheck "helper-algebras.rkt"
           (submod ".." gen))

  ;; --- helpers: read spines and indexes straight off string cuts ---
  (define (sides str i) (values (sexp-smr (substring str 0 i)) (sexp-smr (substring str i))))
  (define (front-of str i) (let-values ([(L R) (sides str i)])
                             (let-values ([(f b) (sand-spines L R)]) f)))
  (define (back-of  str i) (let-values ([(L R) (sides str i)])
                             (let-values ([(f b) (sand-spines L R)]) b)))

  ;; --- guide sign sweeps: every interior cut x every target, all-left,
  ;; all-right, AND the mixed anchor (right head over the left path) ---
  (define (sweep str targets)
    (for ([at targets])
      (define-values (f b) (let-values ([(L R) (sides str at)]) (sand-spines L R)))
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
    (let-values ([(l r) ((multisect sexp-smr (slot-guide ix)) rope)])
      (cons (~a l) (~a r))))
  (define (doc z) (~a ((viewer zipper-focus) (to-root z))))
  (define rope ((make-rope sexp-smr) "(aa bb cc)"))
  (define ^bb (front-of "(aa bb cc)" 4))
  (define ^cc (front-of "(aa bb cc)" 7))

  (let ([z (cursor rope ^bb)])                       ; gap: replace at an empty focus = insert
    (check-equal? (~a ((viewer zipper-focus) z)) "")
    (check-equal? (doc ((setter zipper-focus "xx ") z)) "(aa xx bb cc)"))

  ;; --- the same navigation through a BUNDLE rope: sexp guides read the sexp
  ;; component out of each bundle value via (on sand-spines sexp-smr) ---
  (let* ([cc    (make-summary string-length +)]
         [b     (bundle sexp-smr cc)]
         [brope ((make-rope b) "(aa bb cc)")]
         [gs    (sexp-guides ^bb)]
         [z     ((setter zipper-guide gs) (start b brope (first gs) (second gs)))])
    (check-equal? (~a ((viewer zipper-focus) z)) "")
    (check-equal? (doc ((setter zipper-focus "xx ") z)) "(aa xx bb cc)"))

  (let ([z (cursor rope ^bb ^cc)])                   ; seg [^bb ^cc): half-open
    (check-equal? (~a ((viewer zipper-focus) z)) "bb ")
    (check-equal? (doc ((setter zipper-focus "XX ") z)) "(aa XX cc)")
    (check-equal? (doc ((setter zipper-focus "") z)) "(aa cc)")) ; the empty replace = delete

  ;; back index navigates to the same place
  (check-equal? (doc ((setter zipper-focus "xx ") (cursor rope (back-of "(aa bb cc)" 4))))
                "(aa xx bb cc)")

  ;; --- the end slot: slot N of an N-child frame is a real target; both
  ;; anchorings name it (front N | right -1) and appending lands tight after
  ;; the last child ---
  (check-equal? (doc ((setter zipper-focus " dd") (cursor rope '(3 0))))  "(aa bb cc dd)")
  (check-equal? (doc ((setter zipper-focus " dd") (cursor rope '(-1 0)))) "(aa bb cc dd)")
  (let ([z (cursor ((make-rope sexp-smr) "(aa )") '(1 0))]) ; ws after the last child:
    (check-equal? (~a z) "(aa ‸)"))                         ; lands tight before the close (ws binds left)

  ;; --- navigation + editing, nested ---
  (define rope2 ((make-rope sexp-smr) "(aa (p q) cc)"))
  (check-equal? (doc ((setter zipper-focus "xx ") (cursor rope2 (front-of "(aa (p q) cc)" 7))))
                "(aa (p xx q) cc)")

  ;; --- anchors: read both at the cursor, and they diverge under editing ---
  (let*-values ([(z) (cursor rope ^bb)]
                [(front back) (anchors z 0)])
    (check-equal? front '(1 0))
    (check-equal? back  '(-3 0))                       ; right head over the left path
    (define rope1 ((viewer zipper-focus) (to-root ((setter zipper-focus "xx ") z))))
    (check-equal? (cuts rope1 front) (cons "(aa " "xx bb cc)"))  ; left-anchored: stays by aa
    (check-equal? (cuts rope1 back)  (cons "(aa xx " "bb cc)"))) ; right-anchored: stays by bb

  ;; --- re-basing: every cut (incl. mid-atom ½s, whitespace, and the -1/2 head
  ;; after an open paren) -- the modulus is integral, head-basing reproduces the
  ;; anchor pair, flip is an involution ---
  (for ([str '("(aa bb cc)" "(aa (p q) cc)" "((a) (b (c)) d)" "aa bb cc"
               "( aa)" "(aa )")])
    (for ([i (in-range 1 (string-length str))])
      (define-values (L R) (sides str i))
      (define-values (f b) (sand-spines L R))
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
    (check-equal? (doc ((setter zipper-focus "xx ") (cursor rope back*))) "(aa xx bb cc)")
    (define rope1 ((viewer zipper-focus) (to-root ((setter zipper-focus "xx ") z))))
    (check-equal? (cuts rope1 ^bb)   (cons "(aa " "xx bb cc)"))
    (check-equal? (cuts rope1 back*) (cons "(aa xx " "bb cc)")))

  ;; --- the guide lens: an anchor flip installs and re-navigates to the SAME
  ;; gap (L2); editing afterwards behaves as the flipped family ---
  (let* ([z  (cursor rope ^bb)]
         [zb ((setter zipper-guide (sexp-guides ((flip (edge-modulus z 0)) ^bb))) z)])
    (check-equal? (~a ((viewer zipper-focus) zb)) "")                          ; same gap at flip-time
    (check-equal? (doc ((setter zipper-focus "xx ") zb)) "(aa xx bb cc)"))

  ;; --- re-anchoring the END edge via the lens's modify face makes the cursor
  ;; edit-stable: it keeps covering the focus across replace and delete ---
  (let* ([z  (cursor rope ^bb ^cc)]                            ; both front: end drifts under edits
         [e* ((flip (edge-modulus z 1)) ^cc)]                  ; re-anchor the end's head on its right side
         [z  ((setter (edge-guide 1) (slot-guide e*)) z)])     ; install the flipped end guide
    (check-equal? (~a ((viewer zipper-focus) z)) "bb ")                        ; same seg at flip-time
    (let ([z* ((setter zipper-focus "x1 x2 ") z)])
      (check-equal? (~a ((viewer zipper-focus) z*)) "x1 x2 ")                  ; replace re-navigated: still covering
      (check-equal? (doc z*) "(aa x1 x2 cc)")
      (let ([zg ((setter zipper-focus "") z*)])                            ; delete collapses to the gap
        (check-equal? (~a ((viewer zipper-focus) zg)) "")
        (check-equal? (doc ((setter zipper-focus "yy ") zg)) "(aa yy cc)")))) ; and editing chains on

  ;; --- cover: flip the second guide; the right anchor stays fixed while the
  ;; stuff inside is edited ---
  (let* ([z  (cover (cursor rope2 (front-of "(aa (p q) cc)" 7)))] ; covered gap at ^q
         [z1 ((setter zipper-focus "x ") z)]
         [z2 ((setter zipper-focus "x y ") z1)]
         [z3 ((setter zipper-focus "") z2)])
    (check-equal? (~a ((viewer zipper-focus) z1)) "x ")
    (check-equal? (doc z1) "(aa (p x q) cc)")
    (check-equal? (~a ((viewer zipper-focus) z2)) "x y ")
    (check-equal? (doc z2) "(aa (p x y q) cc)")    ; the interior grew: q held its ground
    (check-equal? (~a ((viewer zipper-focus) z3)) "")
    (check-equal? (doc z3) "(aa (p q) cc)"))       ; and shrank back to the gap

  (let* ([z (cover (cursor rope ^bb ^cc))])        ; a seg, covered by the same mechanism
    (check-equal? (~a ((viewer zipper-focus) z)) "bb ")
    (check-equal? (doc ((setter zipper-focus "b1 (b2 b3) ") z)) "(aa b1 (b2 b3) cc)"))

  ;; --- the edit verbs: each is a zipper -> zipper command (rides `idxs`), reading the index
  ;; straight off the guide.  (indexes in "(aa bb cc)": ^bb = '(1 0), ^cc = '(2 0), end slot = '(3 0))
  (let ([z (cursor rope '(1 0))])                  ; a gap before bb
    (check-equal? (doc ((setter zipper-focus "xx ") ((at '(2 0)) z)))         ; absolute
                  "(aa bb xx cc)")
    (check-equal? (doc ((setter zipper-focus "xx ") ((move (slot add1)) z)))  ; advance one slot
                  "(aa bb xx cc)")
    (check-equal? (~a ((viewer zipper-focus) ((each values (slot add1)) z)))  ; open gap -> seg
                  "bb ")
    (check-equal? (~a ((viewer zipper-focus) ((edge 1 (slot add1)) z))) "bb ")) ; one edge: end +1 = same seg
  (let ([z (cursor rope '(1 0) '(2 0))])           ; a seg [bb, cc) = "bb "
    (check-equal? (~a ((viewer zipper-focus) ((both (slot add1)) z))) "cc"))    ; shift both edges

  ;; ========================================================================
  ;; Document isos -- test scaffolding for the index battery (rationale: scribble).
  ;; Three genuine isos over the document's states (helper-algebras' `iso`):
  ;;   A  shape  <-> spines   (structure; fold / unfold)
  ;;   C  tree   <-> pieces   (content;   tokenize / parse)
  ;;   B  pieces <-> text     (text;      concat / lex)
  ;; tree rep: atom = string, frame = (list child ...); a bare shape uses () for
  ;; atoms.  The generator draws a bare shape, then populates atoms into it.

  ;; -- helpers --
  (define (map-group-by pairs)             ; group by car, collect cdrs, key order
    (for/list ([g (in-list (group-by car (sort pairs < #:key car)))]) (map cdr g)))
  (define (unfold-tree coalg seed)         ; labelless rose-tree anamorphism
    (map (lambda (s) (unfold-tree coalg s)) (coalg seed)))
  (define (blank? p) (regexp-match? #px"^\\s*$" p))

  ;; -- A: shape <-> spines (fold / unfold) --
  (define (shape->spines t [ix '(0)])
    (if (null? t)
        (list ix)
        (cons ix (append* (for/list ([c (in-list t)] [j (in-naturals)])
                            (shape->spines c (cons j ix)))))))
  (define (spines->shape sps)
    (unfold-tree (lambda (ps) (map-group-by (filter pair? ps)))
                 (map (lambda (s) (cdr (reverse s))) sps)))
  (define shape<->spines (iso shape->spines spines->shape))

  ;; -- C: tree <-> pieces (tokenize / parse) --
  (define (tree->pieces t)
    (if (string? t)
        (list t)
        (append (list "(")
                (append* (add-between (map tree->pieces t) (list " ")))
                (list ")"))))
  (define (pieces->tree pieces)
    (define (parse ts)
      (match ts
        [(cons "(" rest)
         (let loop ([ts rest] [kids '()])
           (match ts
             [(cons ")" rest) (values (reverse kids) rest)]
             [_ (define-values (k rest*) (parse ts)) (loop rest* (cons k kids))]))]
        [(cons atom rest) (values atom rest)]))
    (define-values (t _) (parse (filter (lambda (p) (not (blank? p))) pieces)))
    t)
  (define tree<->pieces (iso tree->pieces pieces->tree))

  ;; -- B: pieces <-> text (concat / lex) --
  (define (pieces->text pieces) (apply string-append pieces))
  (define (text->pieces s) (regexp-match* #px"[()]|[^()\\s]+|\\s+" s))
  (define pieces<->text (iso pieces->text text->pieces))

  ;; -- generator: gen:shape / gen:atom / gen:populate live in the `gen`
  ;;    submodule (required above); gen:tree composes them at the test's params --
  (define gen:tree (gen:bind (gen:shape 4 3) gen:populate))

  ;; -- the isos round-trip: worked examples, a curated corpus, and randomly --
  (check-equal? (shape->spines '(() (() ()) ())) '((0) (0 0) (1 0) (0 1 0) (1 1 0) (2 0)))
  (check-equal? (tree->pieces '("f" ("g" "x") "y"))
                '("(" "f" " " "(" "g" " " "x" ")" " " "y" ")"))
  (check-equal? (text->pieces "(f (g x) y)")
                '("(" "f" " " "(" "g" " " "x" ")" " " "y" ")"))

  (define shape-corpus (list '() '(()) '(() ()) '(() (() ()) ()) '((()) ())))
  (define tree-corpus
    (list "x" '("a" "b") '("f" ("g" "x") "y") '(("a")) '()
          '("define" ("f" "x") ("+" "x" "1"))))
  (check-equal? (check-iso-laws shape<->spines shape-corpus) '())
  (check-equal? (check-iso-laws tree<->pieces tree-corpus) '())
  (check-equal? (check-iso-laws pieces<->text (map tree->pieces tree-corpus)) '())

  (check-property (make-config) (property ([s (gen:shape 4 3)])
                                  (check-true (iso-law? shape<->spines s))))
  (check-property (make-config) (property ([t gen:tree])
                                  (check-true (iso-law? tree<->pieces t))))
  (check-property (make-config) (property ([t gen:tree])
                                  (check-true (iso-law? pieces<->text (tree->pieces t))))))
