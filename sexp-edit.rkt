#lang racket

;; Sexp navigation + editing on the summarised rope, on SIGNED SPINE indexes.
;;
;; An index is a spine: the per-level position list, innermost-first.  Slots are
;; 0-BASED at every level.  EACH COMPONENT picks the side it reads at its level:
;; >= -1/2 against the all-left `front` spine, <= -1 against the all-right `back`
;; spine -- the pair `sand-spines` reads at a cut (summaries/summaries.rkt; -1 = after the
;; last form).  The two ANCHORS of a position differ in the HEAD only: the path
;; down to the cut's frame is a left-based name either way, and the head is
;; anchored left or right within that frame -- `anchors` returns exactly this
;; pair, and re-deriving the other head at a cursor IS the anchor flip
;; (head-only, by one modulus).
;;
;; `spine-cmp` compares an index against a cut's (front, back) spines.  Under
;; completion counting an open frame's interior is a prefix extension of the
;; frame's own start slot, so the comparison is lexicographic (helper-algebras'
;; `lexicographic`, outermost-first): read each level off the spine the index's
;; component selects (front for >= -1/2, back for <= -1) and take the first
;; non-zero componentwise verdict; a shorter spine sorts before a deeper one (a
;; bare spine sits before everything deeper inside it -- the early exit that
;; replaced the -inf padding).  Targets land on form starts AND on frames' END
;; SLOTS -- slot N of an N-child frame.  A guide is one comparison.
;;
;; The layer is a comparison over the spines `sand-spines` reads (summaries/summaries.rkt):
;; everything else is zipper-core.

(require racket/match
         "rope-core.rkt"        ; make-rope multisect frame
         "summaries/sexp-summary.rkt"  ; sexp-smr, sand-spines
         "summaries/summaries.rkt"     ; bundle
         "zipper-core.rkt"      ; start zipper-guide zipper-focus to-root on-edges
         "helper-algebras.rkt") ; on (bundle projection); lexicographic (spine-cmp); make-lens/vref/vdiag (lens vocab)

(provide spine-cmp              ; (spine-cmp front back index) -> -1 | 0 | 1
         slot-guide guide-index ; (slot-guide index) -> guide (carries its index)
         modulus base-left base-right flip  ; re-basing: head-only, by one modulus
         sexp-guides cursor     ; cursor conveniences over zipper-core
         edge-contexts anchors edge-modulus
         re-anchor cover        ; the anchor flip as a cursor operation
         edge-guide edge-index  ; lenses onto one cursor edge (its guide / its index)
         at move each both slot  ; edit verbs: gs -> gs, for (updater zipper-guide ...)
         (all-from-out "rope-core.rkt")
         (all-from-out "summaries/sexp-summary.rkt")
         (all-from-out "summaries/summaries.rkt")
         (all-from-out "zipper-core.rkt"))

;; ---------- the comparison ----------
;; the ordinary 3-way order: -1 if a<b, +1 if a>b, 0 equal.
(define (component-cmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))

;; the family tag: back components sit at <= -1, front at >= -1/2 (the -1/2 is
;; a leaned head in a frame with no child counted yet -- whitespace right after
;; an open paren).  The half-step gap keeps the families disjoint.
(define (back-component? c) (< c -1/2))

;; an index component vs a cut element -- its (front . back) pair -- to a sign;
;; the component picks its own anchoring (front >= -1/2, back <= -1), so all-left,
;; all-right, and mixed indexes resolve uniformly.  Index first, so +1 = target
;; right of the cut.
(define (cut-cmp c fb) (component-cmp c (if (back-component? c) (cdr fb) (car fb))))

;; zip the two co-indexed spines into one cut, then compare an index against it
;; lexicographically, outermost-first; a shorter spine sorts before a deeper one
;; (a bare spine sits before everything deeper inside it).
(define (spine-cmp front back ix)         ; -> +1 boundary right of cut / 0 / -1
  ((lexicographic cut-cmp) (reverse ix) (reverse (map cons front back))))

;; ---------- the guide ----------
;; A guide carries its index: callable as the comparator (one comparison; each
;; component of the index picks its own side, split by `back-component?` at the
;; half-step gap -- front >= -1/2, back <= -1, leans included), AND readable as
;; the index, so the edit verbs operate on the cursor vector directly without
;; re-deriving from the zipper.
(struct guide (index proc) #:property prop:procedure (struct-field-index proc))
(define (slot-guide ix)
  (guide ix (lambda (L R)
              (let-values ([(front back) ((on sand-spines sexp-smr) L R)])
                (spine-cmp front back ix)))))

;; ---------- cursor conveniences ----------
(define (sexp-guides s [e s]) (vector (slot-guide s) (slot-guide e)))
(define (cursor rope s [e s])                        ; place the cursor, then navigate to it
  (let ([gs (sexp-guides s e)]) ((setter zipper-guide gs) (start sexp-smr rope gs))))

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
  (define-values (f b) ((on sand-spines sexp-smr) L R))
  (values f (cons (car b) (cdr f))))

;; ---------- re-basing ----------
;; the modulus of a cut: front head - back head = N+1 at the cut's own level
;; (the uniformity bar; encoding A's modulus over N forms).  Only the head
;; re-bases -- the path components are a left-based name shared by both
;; anchorings -- so the flip data is one number, exactly what one index alone
;; cannot know.
(define (modulus L R)
  (define-values (f b) ((on sand-spines sexp-smr) L R))
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

;; ---------- edge lenses ----------
;; A vector-slot lens (`vref`) composed onto `zipper-guide` is a lens straight onto one cursor
;; edge; one hop more (a guide <-> its index) lands on its index.  Composition is plain `compose`
;; (the van Laarhoven encoding), and a write through the composite re-navigates once -- the put
;; bottoms out in zipper-guide's put.
(define index-of (make-lens (lambda (g) (values (guide-index g) slot-guide))))  ; lens: a guide <-> its index
(define (edge-guide i) (compose zipper-guide (vref i)))           ; -> the guide at edge i
(define (edge-index i) (compose zipper-guide (vref i) index-of))  ; -> its index

;; ---------- re-anchoring ----------
;; install edge i's guide re-derived from the chosen side's anchor ('front |
;; 'back), by setting that edge's lens: the anchor flip as a cursor operation.
;; Same position now; the family decides how the edge follows future edits.
(define (re-anchor z i side)
  (define-values (front back) (anchors z i))
  (define g (slot-guide (if (eq? side 'front) front back)))
  ((setter (edge-guide i) g) z))

;; cover: flip the SECOND guide onto its right anchor (the start already reads
;; the left).  An edit between the edges then touches neither anchor's side,
;; so the cursor keeps covering whatever replaces the focus.
(define (cover z) (re-anchor z 1 'back))

;; ---------- command vocabulary ----------
;; The cursor verbs are vector-lens expressions over the guide vector gs = #(g0 g1).
;; `index-of` is the one bridge (a guide <-> its index, above); `vref` / `vdiag` are the
;; vector lenses (helper-algebras).  Each verb is a gs -> gs command (fed to (updater
;; zipper-guide ...)), e.g. (updater zipper-guide (move f) z):
;;   gap verbs collapse to a gap -- vdiag views the start guide and fills both edges;
;;   seg verbs keep the edges distinct -- per-edge (vref i) for `each`, the tuple
;;   (vref 0 1) for `both`.
;; `slot` is the index-level helper: map the innermost slot, the frame (cdr) untouched,
;; so each edge stays in its own sexp.  (`chain`, the trace pipe, now lives in
;; zipper-core's `internal` submodule.)
(define ((slot h) ix) (cons (h (car ix)) (cdr ix)))

(define (at   ix)    (setter  (compose vdiag index-of) ix))                 ; absolute gap at ix
(define (move f)     (updater (compose vdiag index-of) f))                  ; gap at (f start)
(define (each fl fr) (compose (updater (compose (vref 0) index-of) fl)      ; each edge its own fn
                              (updater (compose (vref 1) index-of) fr)))
(define (both f)     (updater (vref 0 1) (curry map (updater index-of f)))) ; the tuple: f over both edges

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
    (let-values ([(l r) ((multisect sexp-smr (vector (slot-guide ix))) rope)])
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
         [z     ((setter zipper-guide gs) (start b brope gs))])
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
         [z  ((updater zipper-guide (lambda (gs) (vector (vector-ref gs 0) (slot-guide e*)))) z)])
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

  ;; --- the edit verbs: each is a vector -> vector (or vector value) fed through
  ;; (updater / setter zipper-guide ...), reading the index straight off the guide.
  ;; (indexes in "(aa bb cc)": ^bb = '(1 0), ^cc = '(2 0), end slot = '(3 0))
  (let ([z (cursor rope '(1 0))])                  ; a gap before bb
    (check-equal? (doc ((setter zipper-focus "xx ") ((updater zipper-guide (at '(2 0))) z)))         ; absolute
                  "(aa bb xx cc)")
    (check-equal? (doc ((setter zipper-focus "xx ") ((updater zipper-guide (move (slot add1))) z)))  ; advance one slot
                  "(aa bb xx cc)")
    (check-equal? (~a ((viewer zipper-focus) ((updater zipper-guide (each values (slot add1))) z)))  ; open gap -> seg
                  "bb "))
  (let ([z (cursor rope '(1 0) '(2 0))])           ; a seg [bb, cc) = "bb "
    (check-equal? (~a ((viewer zipper-focus) ((updater zipper-guide (both (slot add1))) z))) "cc"))    ; shift both edges

  ;; ========================================================================
  ;; Document isos -- the tree-first scaffolding for the index battery.
  ;; Three genuine isos over the document's states (helper-algebras' `iso`):
  ;;   A  shape  <-> spines   (structure; fold / unfold)
  ;;   C  tree   <-> pieces   (content;   tokenize / parse)
  ;;   B  pieces <-> text     (text;      concat / lex)
  ;; tree rep: atom = string, frame = (list child ...); a bare shape uses () for
  ;; atoms.  The generator draws a bare shape, then populates atoms into it.
  ;; (The guide-driven bridge -- spines locating the cuts in the text -- is the
  ;; next step; these three isos are the free scaffolding it gets checked against.)

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
