#lang racket

;; Sexp navigation + editing on the summarised rope, as a layer over zipper-core.
;; An index is a SIGNED SPINE -- the per-level slot list, innermost-first, each
;; component picking the side it reads (front >= -1/2, back <= -1, the pair
;; `sand-spines` reads at a cut). The layer is one thing: a comparison over those
;; spines (`spine-cmp`); everything else is zipper-core. The surface:
;;   spine-cmp        front back index -> -1 | 0 | 1   -- the comparison (one guide's verdict)
;;   slot-guide       index -> guide   -- a guide that also reads back as its index
;;   cut-index        L R -> index     -- read an index straight off a cut (front anchor)
;;   cursor sexp-guides                          -- place a cursor
;;   anchors reanchor cover                      -- edge i's snapped anchor (i picks side+rounding); re-anchor; cover
;;   reguide front-guide back-guide               -- (reguide gl gr): set each edge's guide from its cut (L R -> guide)
;;   edge-guide edge-index                       -- STAGES onto one cursor edge (its guide / its index)
;;   at move edge each both                      -- edit verbs: zipper -> zipper commands (parked)
;; The guide layer rides zipper-core's STAGED (g*) optic (zipper-guide/g i): edge i's
;; guide focal, its cut on the render bus; the anchor ops are consumers of it (enter +
;; a (g put) handler, or stage-view / stage-update). Index model, the two anchors, the
;; lexicographic comparison: scribble/sexp-edit.scrbl (pre-dates the staged optic).

(require racket/match
         "../rope-core.rkt"        ; make-rope multisect frame
         "../summaries/sexp-summary.rkt"  ; sexp-smr, sand-spines
         "../summaries/summaries.rkt"     ; bundle
         "../zipper-core.rkt"      ; start; zipper-guide/g zipper-focus/g edge-view to-root; the stage vocab
         "../toolbox/main.rkt"     ; on; lexicographic; pure
         (submod "../toolbox/algebra.rkt" experimental)) ; the curried `lambda` -- (lambda ((L R) g) ...)

(provide spine-cmp              ; (spine-cmp front back index) -> -1 | 0 | 1
         slot-guide guide-index ; (slot-guide index) -> guide (carries its index)
         cut-index              ; (cut-index L R) -> front index, read off a cut's sides (no guide)
         sexp-guides cursor     ; cursor conveniences over zipper-core
         edge-contexts anchors  ; edge i's cut; edge i's snapped anchor (i picks side+rounding)
         reanchor cover         ; re-anchor edge i to its snapped anchor; cover = both edges
         reguide                ; (reguide gl gr): set each edge's guide from its cut
         front-guide back-guide ; L R -> guide at an anchor (reguide's makers)
         edge-guide edge-index  ; opts onto one cursor edge (its guide / its index)
         slot                         ; innermost-slot wrapper (verbs at/move/edge/each/both commented out -- values exploration)
         (all-from-out "../rope-core.rkt")
         (all-from-out "../summaries/sexp-summary.rkt")
         (all-from-out "../summaries/summaries.rkt")
         (all-from-out "../zipper-core.rkt"))

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

;; cut-index: read a sexp index straight off a cut's sides L R -- the front anchor, no guide. Fold
;; the edge-view with it: ((compose cut-index (edge-view i)) z). (sand-spines also yields the
;; back anchor, which `anchors` pairs with the front; reading the front only, this drops the front/back choice.)
(define (cut-index L R)
  (define-values (front back) ((on sand-spines sexp-smr) L R))
  front)

;; ---------- cursor conveniences ----------
(define (sexp-guides s [e s]) (list (slot-guide s) (slot-guide e)))   ; the guide list (gs ge)
(define (cursor rope s [e s])                        ; place the cursor, then navigate to it
  (let ([p (sexp-guides s e)])                       ; start seeds both guides; one edge-set re-navigates
    (((stage-set (zipper-guide/g 0)) (first p))
     (start sexp-smr rope (first p) (second p)))))

;; ============================================================================
;; Guides: opts & transforms -- everything that focuses the cursor's guides
;; or transforms their indices, gathered here.
;; ============================================================================

;; ---------- optics onto the guides ----------
;; The parameterized (zipper-guide/g i) (zipper-core) IS edge i's guide optic -- the
;; guide focal, its cut (L R) on the render bus. index-of reads a guide's index; the
;; edge index optic is the guide optic then index-of. A write through a composite
;; re-navigates once.
(define index-of (pure (lambda (g) (values (lambda (c) ((c) (guide-index g))) slot-guide)))) ; guide <-> index
(define edge-guide zipper-guide/g)                            ; -> the guide at edge i (its cut on the bus)
(define (edge-index i) (compose-stage (zipper-guide/g i) index-of))  ; -> its index

;; ---------- anchors & re-anchoring ----------
;; edge-contexts: edge i's cut as its two side-summaries (a gap collapses both to
;; before | after) -- zipper-core's edge-view, argument order kept.
(define (edge-contexts z i) ((edge-view i) z))    ; i: 0 = start edge, 1 = end edge

;; snap i -> edge i's anchor index, snapped to a clean slot; the side AND rounding read off i:
;;   i=0 (start): front head FLOORED   -> the form's start
;;   i=1 (end):   back  head CEILING'd -> past the form
;; clean integer heads are fixed points; a mid-atom/ws ½ head snaps outward, so a mid-atom
;; cursor brackets the atom. Why floor/ceiling, why per-i: scribble.
(define ((snap i) L R)
  (define-values (f b) ((on sand-spines sexp-smr) L R))
  (if (zero? i)
      (cons (floor   (car f)) (cdr f))     ; start
      (cons (ceiling (car b)) (cdr f))))   ; end

;; anchors: read edge i's cut off (zipper-guide/g i)'s bus, snap it -- a pure read.
(define (anchors z i)
  ((compose (lambda (g _put) (g (lambda ((L R) _guide) ((snap i) L R))))
            (enter z))
   (zipper-guide/g i)))

;; reanchor: install edge i's guide from its snapped anchor (read off the cut, re-navigates).
;; The (g put) handler reads the cut (renders) and writes the snapped guide (put), one pass.
(define ((reanchor i) z)
  ((compose (lambda (g put) (g (lambda ((L R) _guide) (put (slot-guide ((snap i) L R))))))
            (enter z))
   (zipper-guide/g i)))

;; reguide: install a guide at each cursor edge, computed from that edge's cut (its L R).
;;   gl, gr : L R -> guide   -- the start edge's maker, the end edge's maker.
;; The covering pair is (reguide front-guide back-guide): the start stays front-anchored, the end
;; flips to its back anchor. front-guide is slot-guide on the front anchor (cut-index); back-guide
;; takes the back-headed anchor off the same spines.
(define front-guide (compose slot-guide cut-index))   ; ((compose ..) L R) = (slot-guide (cut-index L R))
(define (back-guide L R)
  (define-values (f b) ((on sand-spines sexp-smr) L R))
  (slot-guide (cons (car b) (cdr f))))

;; re-edge: one edge -- read its cut (renders), make the guide with mk, install it there.
(define ((re-edge i mk) z)
  ((compose (lambda (g put) (g (lambda ((L R) _guide) (put (mk L R)))))
            (enter z))
   (zipper-guide/g i)))

;; reguide: re-guide the start edge, then the end edge.
(define (reguide gl gr) (compose (re-edge 1 gr) (re-edge 0 gl)))

;; cover: the start to its front anchor, the end to its back anchor, so an edit between the
;; edges touches neither side and the cursor keeps covering (a mid-atom cursor brackets its atom).
(define cover (compose (reanchor 1) (reanchor 0)))

;; ---------- command vocabulary ----------
;; The verbs ride `idxs` (the index-list opt, above), then a selector opt at the tail picks the
;; reach: `(ldiag i)` collapses to position i, `(lref i)` singles one edge, no selector keeps the
;; list. Each is one opt-set/opt-update that re-navigates once. Point-free style, the reach per
;; verb: scribble.
(define ((slot h) ix) (cons (h (car ix)) (cdr ix)))      ; map the innermost slot; frame (cdr) untouched

;; The verbs are parked (commented) -- each rides `reindex`, one stage-update over an
;; edge's index optic; the reach is which edges you reindex (compose of one or two), so
;; the old idxs list + ldiag/lref selectors are gone.
#;(define (reindex i f) (stage-update (edge-index i) f))                                ; edge i's index by f
#;(define (at   ix)    (compose (reindex 1 (lambda (_) ix)) (reindex 0 (lambda (_) ix)))) ; absolute gap at ix
#;(define ((move f) z) ((at (f ((stage-get (edge-index 0)) z))) z))                     ; gap, f on edge 0's basis
#;(define (edge i f)   (reindex i f))                                                   ; one edge (0 start, 1 end)
#;(define (each fl fr) (compose (reindex 1 fr) (reindex 0 fl)))                         ; a fn per edge
#;(define (both f)     (compose (reindex 1 f) (reindex 0 f)))                           ; same fn over both

;; ---------- a worked edit sequence (the format to follow when one is asked for) ----------
;; Each verb spelled INLINE as a reindex (stage-update over an edge's index optic) composed
;; per edge, the sugared verb named in the trailing comment, the marked document each step
;; produces on the right. `chain` (from zipper-core's `internal` submodule) threads and prints.
;;
;;   (chain (cursor rope '(1 0))                                          ; (aa ‸bb cc)
;;          (compose (reindex 1 (slot add1)) (reindex 0 (slot add1)))     ; move (slot add1)  -> (aa bb ‸cc)
;;          (compose (reindex 1 (lambda (_) '(1 0))) (reindex 0 (lambda (_) '(1 0)))) ; at '(1 0) -> (aa ‸bb cc)
;;          (reindex 1 (slot add1))                                       ; edge 1 (slot add1) -> (aa ⟦bb ⟧cc)
;;          (reguide front-guide back-guide)                              ; cover              -> (aa ⟦bb ⟧cc)
;;          ((stage-set zipper-focus/g) "XX ")                            ; replace the seg    -> (aa ⟦XX ⟧cc)
;;          ((stage-set zipper-focus/g) ""))                              ; delete             -> (aa ‸cc)

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
  (require rackunit rackcheck "../toolbox/main.rkt"
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
  (define (doc z) (~a ((opt-get zipper-focus) (to-root z))))
  (define rope ((make-rope sexp-smr) "(aa bb cc)"))
  (define ^bb (front-of "(aa bb cc)" 4))
  (define ^cc (front-of "(aa bb cc)" 7))

  (let ([z (cursor rope ^bb)])                       ; gap: replace at an empty focus = insert
    (check-equal? (~a ((opt-get zipper-focus) z)) "")
    (check-equal? (doc (((opt-set zipper-focus) "xx ") z)) "(aa xx bb cc)"))

  ;; --- the same navigation through a BUNDLE rope: sexp guides read the sexp
  ;; component out of each bundle value via (on sand-spines sexp-smr) ---
  (let* ([cc    (make-summary string-length +)]
         [b     (bundle sexp-smr cc)]
         [brope ((make-rope b) "(aa bb cc)")]
         [gs    (sexp-guides ^bb)]
         [z     (((opt-set zipper-guide) gs) (start b brope (first gs) (second gs)))])
    (check-equal? (~a ((opt-get zipper-focus) z)) "")
    (check-equal? (doc (((opt-set zipper-focus) "xx ") z)) "(aa xx bb cc)"))

  (let ([z (cursor rope ^bb ^cc)])                   ; seg [^bb ^cc): half-open
    (check-equal? (~a ((opt-get zipper-focus) z)) "bb ")
    (check-equal? (doc (((opt-set zipper-focus) "XX ") z)) "(aa XX cc)")
    (check-equal? (doc (((opt-set zipper-focus) "") z)) "(aa cc)")) ; the empty replace = delete

  ;; back index navigates to the same place
  (check-equal? (doc (((opt-set zipper-focus) "xx ") (cursor rope (back-of "(aa bb cc)" 4))))
                "(aa xx bb cc)")

  ;; --- the end slot: slot N of an N-child frame is a real target; both
  ;; anchorings name it (front N | right -1) and appending lands tight after
  ;; the last child ---
  (check-equal? (doc (((opt-set zipper-focus) " dd") (cursor rope '(3 0))))  "(aa bb cc dd)")
  (check-equal? (doc (((opt-set zipper-focus) " dd") (cursor rope '(-1 0)))) "(aa bb cc dd)")
  (let ([z (cursor ((make-rope sexp-smr) "(aa )") '(1 0))]) ; ws after the last child:
    (check-equal? (~a z) "(aa ‸)"))                         ; lands tight before the close (ws binds left)

  ;; --- navigation + editing, nested ---
  (define rope2 ((make-rope sexp-smr) "(aa (p q) cc)"))
  (check-equal? (doc (((opt-set zipper-focus) "xx ") (cursor rope2 (front-of "(aa (p q) cc)" 7))))
                "(aa (p xx q) cc)")

  ;; --- anchors: edge 0 reads its front anchor, edge 1 its back; they name the same gap now
  ;; but diverge under editing (front stays left, back follows right) ---
  (let* ([z     (cursor rope ^bb)]
         [front (anchors z 0)]
         [back  (anchors z 1)])
    (check-equal? front '(1 0))
    (check-equal? back  '(-3 0))                       ; right head over the left path
    (define rope1 ((opt-get zipper-focus) (to-root (((opt-set zipper-focus) "xx ") z))))
    (check-equal? (cuts rope1 front) (cons "(aa " "xx bb cc)"))  ; left-anchored: stays by aa
    (check-equal? (cuts rope1 back)  (cons "(aa xx " "bb cc)"))) ; right-anchored: stays by bb

  ;; --- cut-index: the front index straight off an edge's sides, folding the
  ;; edge-view, no guide -- agrees with edge 0's anchor ---
  (let ([z (cursor rope ^bb)])
    (check-equal? ((compose cut-index (edge-view 0)) z) ^bb)
    (check-equal? ((compose cut-index (edge-view 0)) z) (anchors z 0)))

  ;; --- anchors snap: floor/ceiling per i, so a clean cut covers as a gap while a cut
  ;; inside an atom brackets the whole atom ---
  (let ([gm (cover (cursor rope '(3/2 0)))])      ; a gap inside "bb" (mid-atom, head 3/2)
    (check-equal? (~a ((opt-get zipper-focus) gm)) "bb "))   ; cover snaps to select the atom
  (let ([gc (cover (cursor rope ^bb))])           ; a clean gap at ^bb
    (check-equal? (~a ((opt-get zipper-focus) gc)) ""))      ; stays a gap

  ;; --- cover: re-anchor the end onto its back anchor; the right anchor stays fixed while the
  ;; stuff inside is edited ---
  (let* ([z  (cover (cursor rope2 (front-of "(aa (p q) cc)" 7)))] ; covered gap at ^q
         [z1 (((opt-set zipper-focus) "x ") z)]
         [z2 (((opt-set zipper-focus) "x y ") z1)]
         [z3 (((opt-set zipper-focus) "") z2)])
    (check-equal? (~a ((opt-get zipper-focus) z1)) "x ")
    (check-equal? (doc z1) "(aa (p x q) cc)")
    (check-equal? (~a ((opt-get zipper-focus) z2)) "x y ")
    (check-equal? (doc z2) "(aa (p x y q) cc)")    ; the interior grew: q held its ground
    (check-equal? (~a ((opt-get zipper-focus) z3)) "")
    (check-equal? (doc z3) "(aa (p q) cc)"))       ; and shrank back to the gap

  (let* ([z (cover (cursor rope ^bb ^cc))])        ; a seg, covered by the same mechanism
    (check-equal? (~a ((opt-get zipper-focus) z)) "bb ")
    (check-equal? (doc (((opt-set zipper-focus) "b1 (b2 b3) ") z)) "(aa b1 (b2 b3) cc)"))

  ;; reguide is parameterised per edge: a maker pair sets each edge's guide from its cut.
  ;; (reguide front-guide back-guide) reproduces cover; front on both keeps the seg front-anchored.
  (let ([z ((reguide front-guide back-guide) (cursor rope ^bb ^cc))])
    (check-equal? (doc (((opt-set zipper-focus) "b1 (b2 b3) ") z)) "(aa b1 (b2 b3) cc)"))  ; == cover
  (let ([z ((reguide front-guide front-guide) (cursor rope ^bb ^cc))])
    (check-equal? (~a ((opt-get zipper-focus) z)) "bb "))

  ;; --- the edit verbs are commented out (values-based exploration); their tests too ---
  #;(let ([z (cursor rope '(1 0))])                  ; a gap before bb
    (check-equal? (doc (((opt-set zipper-focus) "xx ") ((at '(2 0)) z)))         ; absolute
                  "(aa bb xx cc)")
    (check-equal? (doc (((opt-set zipper-focus) "xx ") ((move (slot add1)) z)))  ; advance one slot
                  "(aa bb xx cc)")
    (check-equal? (~a ((opt-get zipper-focus) ((each values (slot add1)) z)))  ; open gap -> seg
                  "bb ")
    (check-equal? (~a ((opt-get zipper-focus) ((edge 1 (slot add1)) z))) "bb ")) ; one edge: end +1 = same seg
  #;(let ([z (cursor rope '(1 0) '(2 0))])           ; a seg [bb, cc) = "bb "
    (check-equal? (~a ((opt-get zipper-focus) ((both (slot add1)) z))) "cc"))    ; shift both edges

  ;; ========================================================================
  ;; Document isos -- test scaffolding for the index battery (rationale: scribble).
  ;; Three genuine isos over the document's states (algebra' `iso`):
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
