#lang racket

(require racket/generic                       ; define-generics -- the gen:summary-part extension point
         (only-in "helper-algebras.rkt" on))  ; (on f sel) reads each side through sel before deciding

;; Summarised rope: a persistent rope of text that caches a user-defined summary
;; at every node. Three factories make the whole surface:
;;
;;   smr        : (string | rope | summary)* -> summary  ; built by `make-summary`
;;   make-rope  : smr -> ((string | rope)* -> rope)      ; the rope factory (rebalances)
;;   multisect  : smr [guides] -> (rope -> piece values) ; the one split primitive
;;
;; A node is a `leaf` (its whole text) or a `branch` (two sub-ropes); both inherit
;; from `rope`, which caches what every node shares:
;;   summary -- the cached summary value (O(1) reads)
;;   algebra -- the summary fn it was built under, so `smr` can verify (by
;;              eq?) that a rope's cached value belongs to the summary folding it
;;   leaves  -- number of leaves, kept automatically for balancing. Structural,
;;              like height: it counts NODES, not content. Char length is a content
;;              measure (= string-length), so it is the summary's job, not a node
;;              field -- read off a boundary leaf's own text when the seam needs it.
;;   height  -- node height (leaf 0, branch 1 + max child); like `leaves`, a plain
;;              structural node field for balancing -- off the summary.
;;
;; Nodes participate in Racket's display/write protocol (prop:custom-write on
;; `rope`, inherited), so (~a r), (display r), (with-output-to-string ...) all
;; yield/emit the text -- no bespoke rope->string.
;;
;; The summary fn is the single handle threaded into construction (bound as `smr`
;; at use sites). Building from strings needs it passed; ops on an existing rope
;; recover it from the node via `rope-algebra`.
;;
;; A pure rope library: make, summarise, split (`multisect`), join (`rope-join`),
;; display. Guided navigation lives in `zipper-core.rkt`, built on these.
;;
;; The file is in two halves across an abstraction barrier (the banners below):
;; PART 1 is the dumb structural rope; PART 2 is the nuanced guided/balanced
;; descent (`bisect`, `multisect`, `make-rope`), which reaches PART 1 only through
;; a small boundary -- rope-split / rope-info / combine-info / rope-zero / rope-join.
;; One fusing `rope-join` serves both balance and guided cuts because PART 1 keeps
;; every rope well-formed -- no fusable adjacent leaf pair, i.e.
;; (rope-join (rope-split t)) = t -- so balance's joins never actually fuse.
;;
;; Design notes: discussions/2026-05-29/2-claude.md (the variadic surface),
;; discussions/2026-06-01/1-claude.md (the cleanup this file is the rewrite of),
;; discussions/2026-06-19 (this abstraction-barrier rewrite; see deprecated-5/).

;; ---------- contract vocabulary (private -- defined, not provided) ----------
;; Shared across the contracts below; a `let` can't span contract-out clauses (and
;; `provide` is no expression to wrap), so these live as module-level defines that
;; simply stay off the provide list -- private to the module, unseen by importers.
;;   smr/c   -- an smr is a bare closure, so `procedure?` is the most we can say.
;;             Deliberately NOT (unconstrained-domain-> any/c): the range would be
;;             any/c (summary values are user-defined, opaque), so the arrow checks
;;             nothing the flat `procedure?` doesn't -- it only chaperones the smr,
;;             which is then called on every measure. A flat check, no hot-path wrap.
;;   guide/c -- a guide's codomain IS checkable, so this stays higher-order: a guide
;;             that returns a bad value is caught at the split, not deep in binary
;;             search. (It does follow the guide inward, re-checking on each call --
;;             accepted: the split is where a malformed guide first bites.)
(define smr/c   procedure?)
(define guide/c (-> any/c any/c (or/c -1 0 1)))

(provide
 rope?   ; the node predicate -- exposed for downstream contracts (zipper-core)
 gen:summary-part part->summary summary-part?  ; the open extension point: a value that
                ; selects/contributes itself to a summary under a given smr (e.g. a bundle value)
 (contract-out
  ;; (make-summary string-summary combine) -> smr, the variadic summary fn
  [make-summary (-> (-> string? any/c) (-> any/c any/c any/c) smr/c)]
  ;; (make-rope smr) -> the rope builder (fuses, rebalances); parts are strings | ropes
  [make-rope    (-> smr/c (->* () #:rest (listof (or/c string? rope?)) rope?))]
  ;; (multisect smr [guides]) -> splitter: t -> n+1 pieces as values; no guides -> balance halve.
  ;; Result arity is (add1 (vector-length guides)) -- inexpressible, so the range is `any`.
  [multisect    (->* (smr/c) ((vectorof guide/c)) (-> rope? any))]
  ;; ((frame smr b a) g) -> g with the outer context baked in
  [frame        (-> smr/c any/c any/c (-> guide/c guide/c))]))
;; everything else is internal: leaf?/leaf-rope/branch-rope, rope-join, the descent
;; boundary (rope-split, rope-info, combine-info, rope-zero), the balance guide
;; (within-ratio), bisect, rope-leaves/rope-height,
;; rope-write-text (and make-rope's own pathological?/rebalance). Emptiness is
;; (equal? x ((make-rope smr))): the empty branch is unconstructable (branch guard),
;; so the only 0-leaf rope is an empty leaf.

;; ============================================================================
;; PART 1 -- the dumb rope. Structure, summary, construction, join, display, and
;; the boundary PART 2 speaks through. It maintains ONE invariant PART 2 leans on:
;;
;;     (rope-join (rope-split t)) = t            -- for every well-formed t
;;
;; equivalently: no seam carries a fusable adjacent leaf pair. `rope-join` establishes
;; it (it fuses any fusable seam it builds), and split/join preserve it. Nothing here
;; knows about guides or balance.
;; ============================================================================

;; ---------- nodes ----------
;; A node is a leaf (its whole text) or a branch (two sub-ropes). The `rope`
;; parent caches the summary value, the summary fn it was built under, leaf count,
;; and height; the inherited accessors rope-summary / rope-algebra / rope-leaves /
;; rope-height read any node, and prop:custom-write is inherited too.
(struct rope (summary algebra leaves height) #:transparent
  #:property prop:custom-write (lambda (r port mode) (rope-write-text r port)))
(struct leaf   rope (text)       #:transparent)   ; ctor: (leaf summary algebra leaves height text)
(struct branch rope (left right) #:transparent
  ;; the illegal state, made unconstructable: a branch holds two NON-empty ropes, so
  ;; the only 0-leaf rope is ever an empty leaf.
  #:guard (lambda (summary algebra leaves height left right _name)
            (when (or (zero? (rope-leaves left)) (zero? (rope-leaves right)))
              (error 'branch "empty child -- branches hold two non-empty ropes"))
            (values summary algebra leaves height left right)))

;; max-leaf: the target leaf size -- the fuse limit AND the chunk, so chunking,
;; splitting, and fusing all agree on how big a leaf wants to be. The one knob,
;; deliberately not a parameter: a per-rope size would have to travel with the
;; rope (join recovers everything else from `rope-algebra`) and answer what
;; happens at a seam between ropes that disagree.
(define max-leaf 32)

;; ---------- summary ----------
;; (make-summary string-summary combine) -> smr, the variadic summary fn.
;;   (smr)            = (string-summary "")   ; the empty/identity, like ((make-rope smr))
;;   (smr str)        = (string-summary str)
;;   (smr a b c ...)  = combine, folded left-to-right (associative, not
;;                      commutative -- order is preserved)
;; Strings are measured, a rope re-coerces its cached value (an own rope's returns
;; unchanged by identity; a bundle rope's yields the component slot), summary values
;; pass through. Identity is (smr "") -- no separate empty (string-summary is a monoid
;; homomorphism). Knows nothing of `size`.

;; the open extension point: a `summary-part` is a value that knows how to
;; contribute itself to a summary under a given smr -- applying that smr selects
;; its part (a bundle value extracts one component; see summaries.rkt's `bundle`).
(define-generics summary-part
  (part->summary summary-part smr))

(define (make-summary string-summary combine)
  (define id (string-summary ""))             ; the identity -- built ONCE, at construction

  ;; coerce a part to a summary value: "" is the cached identity; otherwise strings are
  ;; measured, a rope re-coerces its cached value (an own rope's passes back unchanged by
  ;; the identity law (smr v) = v; a bundle rope's value is a summary-part, so it selects
  ;; that component's slot), a part selects itself under smr, and a summary value passes through.
  (define (coerce x)
    (match x
      [""                                        id]
      [(? string?)                               (string-summary x)]
      [(? rope?)                                 (smr (rope-summary x))]   ; re-coerce the cached value: an own rope's passes back by identity, a bundle rope's selects its component slot
      [(? summary-part?)                         (part->summary x smr)]
      [_                                         x]))

  ;; map coerce, then fold combine from the cached identity. foldl, not foldr: the
  ;; original was a LEFT fold, and the law battery (summary-laws.rkt) leans on it --
  ;; "folded left from the unit it still matches measuring the whole" -- so a
  ;; non-associative combine must keep folding left. Racket's foldl passes the element
  ;; first, hence the flip to keep acc on the left.
  (define (smr . parts)
    (foldl (lambda (x acc) (combine acc x)) id (map coerce parts)))
  smr)

;; ---------- construction ----------
;; leaf-rope / branch-rope stamp the cached fields. leaf-rope builds the empty leaf
;; inline for "" (the only 0-leaf rope); branch-rope does NOT fuse -- it is the raw
;; 2-child constructor, called only by rope-join (which has already decided the seam
;; won't fuse), so the no-fusable-pair invariant is never violated through it.
(define ((leaf-rope smr) text)
  (if (string=? text "")
      (leaf (smr "") smr 0 0 "")        ; the empty leaf -- built inline, no cache
      (leaf (smr text) smr 1 0 text)))  ; a content leaf is always 1 leaf
(define ((branch-rope smr) l r)
  (branch (smr l r) smr
          (+ (rope-leaves l) (rope-leaves r))
          (add1 (max (rope-height l) (rope-height r)))
          l r))

;; ---------- join ----------
;; rope-join: the binary join -- the rope analog of `combine`, and rope-split's
;; inverse. Drops empties, fuses the seam when the two boundary leaves fit one leaf,
;; else branches; to reach the seam it descends a boundary edge through SMALL LEAF
;; tips only (< max-leaf chars), so it stays O(1)-ish, never O(depth). Fusing is what
;; KEEPS the no-fusable-pair invariant -- the result never carries a fusable adjacent
;; pair, so split/join round-trips, which is why one fusing join serves both cuts
;; (see the boundary note). Char count is read straight off the boundary leaves
;; (string-length is O(1)) -- the only place it is needed.
(define (rope-join l r)
  (define smr (rope-algebra l))
  (define (chars lf) (string-length (leaf-text lf)))      ; O(1); only at the seam
  (let join ([l l] [r r])
    (match* (l r)
      [(_ _) #:when (zero? (rope-leaves l)) r]            ; empties drop
      [(_ _) #:when (zero? (rope-leaves r)) l]
      [((? leaf?) (? leaf?))                              ; two leaves at the seam:
       (if (<= (+ (chars l) (chars r)) max-leaf)
           ((leaf-rope smr) (string-append (leaf-text l) (leaf-text r)))   ; fuse if they fit,
           ((branch-rope smr) l r))]                                       ; else branch
      [((branch _ _ _ _ ll lr) _) #:when (and (leaf? lr) (< (chars lr) max-leaf))
       ((branch-rope smr) ll (join lr r))]                                 ; small right LEAF tip of l
      [(_ (branch _ _ _ _ rl rr)) #:when (and (leaf? rl) (< (chars rl) max-leaf))
       ((branch-rope smr) (join l rl) rr)]                                 ; small left LEAF tip of r
      [(_ _) ((branch-rope smr) l r)])))

;; ---------- display ----------
;; The walk behind prop:custom-write on `rope`. (~a r), (display r), and
;; (with-output-to-string (lambda () (display r))) all route through here.
(define (rope-write-text r port)
  (cond
    [(leaf? r)   (write-string (leaf-text r) port)]
    [(branch? r) (rope-write-text (branch-left r) port)
                 (rope-write-text (branch-right r) port)]))

;; ---------- the boundary ----------
;; All PART 2 may touch (with `rope-join` above). It never names a struct field below
;; this line; the algebra arrives as a PARAMETER -- `smr` into multisect, the derived
;; side-folder `cmb` (= (combine-info smr)) into bisect/frame. A "side" is the rope's structural
;; measure, the list (summary weight height): summary FIRST, weight (leaf count) SECOND,
;; height THIRD. Summary and weight are exact monoid measures end-to-end; height is exact
;; off a node (rope-info) but only NOMINAL under `combine-info` (folded by max, so the identity
;; side stays an identity) -- fine, because a folded height is never read, only the cached
;; node value is (pathological?/rebalance). PART 2 reads sides with first/second/third.
;;
;; THE INVARIANT, where it is used:  (rope-join (rope-split t)) = t.
;; A well-formed rope has no fusable adjacent pair, so the seam rope-split exposes
;; never re-fuses to a different shape. That is why one fusing rope-join serves both
;; cuts: the joins BALANCE makes on the rise are the tree's own seams (non-fusable,
;; so nothing fuses, leaf counts preserved); the fragments a GUIDED midpoint-split
;; makes ARE fusable, and rope-join puts them back into clean leaves.
;; Precondition: ropes reach PART 2 only via rope-join (make-rope, guided pieces,
;; zipper rebuilds) -- never hand-built past it.

(define (rope-split t)                            ; the dumb halve: l ++ r = t
  (match t
    [(branch _ _ _ _ l r) (values l r)]           ; a branch -> its children (the seam)
    [(? leaf?)                                     ; a leaf -> its char midpoint
     (define s (rope-algebra t)) (define str (leaf-text t))
     (define mid (quotient (string-length str) 2))
     (values ((leaf-rope s) (substring str 0 mid))
             ((leaf-rope s) (substring str mid)))]))

(define (rope-info t)                             ; a node's side: (summary weight height)
  (list (rope-summary t) (rope-leaves t) (rope-height t)))

(define (rope-zero t)                             ; the identity side, for seeding a descent
  (list ((rope-algebra t) "") 0 0))               ; empty summary, 0 leaves, height 0

(define ((combine-info smr) a b)                         ; smr -> cmb: fold two adjacent sides, a left of b
  (list (smr (first a) (first b))                 ; summary    by smr
        (+   (second a) (second b))               ; leaf count by +
        (max (third a)  (third b))))              ; height     by max (nominal; see the boundary note)

;; ============================================================================
;; PART 2 -- guided & balanced descent, and the public factory. Speaks only the
;; boundary above: rope-split / rope-info / combine-info / rope-zero / rope-join.
;; ============================================================================

;; ---------- balance guide ----------
;; A bisect weight guide reads the two boundary WEIGHTS (leaf counts) and returns a
;; sign: 0 = balanced enough (stop), +1 = right-heavy, -1 = left-heavy. `within-ratio`
;; calls a side good enough when the heavier is within a ratio of the lighter (plus 1
;; of slack, to ignore single-leaf granularity). bisect's default lazy heal is a loose
;; (within-ratio 3); make-rope's rebalance a tighter (within-ratio 2) -- inlined at both.
(define ((within-ratio a) l r)             ; l, r are WEIGHTS (leaf counts)
  (cond [(<= (max l r) (+ (* a (min l r)) 1)) 0]   ; good enough
        [(> r l)  1]                               ; right-heavy
        [else    -1]))                             ; left-heavy, still not perfect

;; ---------- bisect: the one descent ----------
;; Cut t into two halves, l ++ r = t, descending the cut edge and reading `decide`
;; (side side -> -1/0/1: +1 cut RIGHT of this seam, -1 LEFT, 0 at it). A decider reads
;; one slot of a side via `on`: (on (within-ratio 3) second) balances on weight,
;; (on g first) cuts on a summary guide. `before`/`after` thread the context sides of t so decide
;; sees totals across t; they start empty -- a standalone t has no outer context. A
;; caller cutting t-in-context bakes that in with `frame` (multisect does). Reassembly
;; on the rise is the single fusing `rope-join`: by the invariant the seams balance
;; rejoins are the tree's own (non-fusable, so counts hold), and the fragments a guided
;; midpoint-split makes ARE fusable and get put back.
;;
;; The leaf is not special: `rope-split` halves a leaf at its midpoint, so a guided cut
;; inside a leaf is the same descent recursing into leaf halves (binary search falls
;; out), and balance stops at a leaf because its weight is flat. The one base case is a
;; leaf too small to halve -- `rope-split` then yields an empty half, and the cut lands
;; at the leaf's near or far edge by decide's sign.
(define ((bisect cmb) t [decide (on (within-ratio 3) second)])   ; cmb = (combine-info smr): the side-folder
  (let descend ([before (rope-zero t)] [t t] [after (rope-zero t)])
    (define-values (l r) (rope-split t))
    (define il (rope-info l)) (define ir (rope-info r))
    (define L (cmb before il))
    (define R (cmb ir after))
    (cond
      [(or (zero? (second il)) (zero? (second ir)))   ; a half empty -> a leaf too small to halve
       (if (positive? (decide L R)) (values r l) (values l r))]   ; cut at the far / near edge
      [else
       (match (decide L R)
         [ 0 (values l r)]
         [ 1 (let-values ([(rl rr) (descend L r after)]) (values (rope-join l rl) rr))]
         [-1 (let-values ([(ll lr) (descend before l R)]) (values ll (rope-join lr r)))])])))

;; ---------- frame ----------
;; Bake outer context into a guide, agnostic to the combine. `combine` is ordinarily
;; `smr` (summary-level -- consumers like zipper-core bake a head's context this way)
;; but here, for multisect, the side-folder `cmb` (= (combine-info smr), side-level). ((frame
;; combine b a) g) wraps g to read totals: (g (combine b l) (combine r a)).
(define ((frame combine b a) g)
  (lambda (l r) (g (combine b l) (combine r a))))

;; ---------- multisect ----------
;; (multisect smr [guides]): smr + guides (a vector of boundary guides) -> splitter.
;; ((multisect smr guides) t) cuts t at each guide's boundary left to right and returns
;; the n+1 pieces as values (p0 ++ ... ++ pn = t). It builds the side-folder cmb = (combine-info
;; smr) once, then each cut runs bisect over the remaining tail with the guide framed by
;; `bacc` -- the side of everything cut off so far -- so it judges against the whole;
;; `bacc` grows by one `cmb` per cut. NO guides -- (multisect smr) or #() -- is the
;; balance halve (`(bisect cmb)`).
(define ((multisect smr [guides #()]) t)
  (define cmb (combine-info smr))                              ; smr -> the side-folder, once
  (if (zero? (vector-length guides))
      ((bisect cmb) t)                                  ; the balance halve
      (for/fold ([rest t] [bacc (rope-zero t)] [pieces '()]
                 #:result (apply values (reverse (cons rest pieces))))
                ([g (in-vector guides)])
        (let-values ([(l r) ((bisect cmb) rest ((frame cmb bacc (rope-zero t)) (on g first)))])
          (values r (cmb bacc (rope-info l)) (cons l pieces))))))

;; ---------- make-rope ----------
;; The public factory, mirroring make-summary: build the empty + the helpers once,
;; return the variadic builder. It assembles parts -- strings chunked into max-leaf
;; leaves, ropes passed through -- by folding the fusing `rope-join` from the empty,
;; then heals balance (a fresh load folds to a right-leaning spine, so if it came out
;; pathologically tall, rebalance it). The whole rebuild tier (pathological?/rebalance)
;; lives here: it is make-rope's private balance policy, the only thing it adds over a
;; dumb fold. It reads height and leaf count through the boundary side (rope-info) now
;; that a side carries height, so it names no struct field (the leaf test is height = 0).
;; The empty is internal too, built once like make-summary's `id` (no global cache).
(define (make-rope smr)
  (define mt    ((leaf-rope smr) ""))          ; the empty -- once, like make-summary's id
  (define cmb   (combine-info smr))                   ; the side-folder, once -- beside leaf-rope/branch-rope
  (define halve (bisect cmb))                  ; this rope's balance-splitter
  (define (chunk s)                            ; a string -> max-leaf-sized pieces
    (for/list ([start (in-range 0 (string-length s) max-leaf)])
      (substring s start (min (string-length s) (+ start max-leaf)))))
  (define (coerce x)                           ; a part -> a rope (the summary coerce's twin)
    (match x
      [""          mt]                         ; empty string -> the empty rope
      [(? string?) (foldr rope-join mt (map (leaf-rope smr) (chunk x)))]
      [_           x]))                        ; a rope (by contract) passes through
  ;; pathological?: height too tall for weight -- the rebuild trigger. C=3 sits just
  ;; above the ~2.41 a ratio-3 rope guarantees (1/log2(4/3)); K=2 is small-rope slack.
  (define (pathological? t)
    (match-define (list _ leaves height) (rope-info t))   ; off the boundary side, not struct fields
    (> height (+ (* 3 (log (add1 leaves) 2)) 2)))
  ;; rebalance: rebuild to a tighter balance by recursively bisecting with the stricter
  ;; (within-ratio 2). Leaves are reused (never split); only branches are rebuilt --
  ;; O(leaves * log). rope-join keeps the shape: on a well-formed rope the halves' seam
  ;; is the tree's own, so it branches rather than fuses.
  (define (rebalance t)
    (if (zero? (third (rope-info t)))                                   ; height 0 = a leaf
        t
        (let-values ([(l r) (halve t (on (within-ratio 2) second))])   ; the stricter rebuild ratio
          (rope-join (rebalance l) (rebalance r)))))
  (define (build . parts)
    (define t (foldr rope-join mt (map coerce parts)))   ; map coerce, then fold -- mirrors smr
    (if (pathological? t) (rebalance t) t))
  build)

;; ============================================================================
(module+ test
  (require rackunit)

  ;; A trivial summary: character count.
  (define sum (make-summary string-length +))

  ;; an explicit right-leaning spine of n max-leaf-sized leaves -- WELL-FORMED (no
  ;; fusable adjacent pair, so it respects the invariant) yet genuinely unbalanced,
  ;; so bisect/rebalance get a real spine to chew on. Content is all #\x.
  (define (spine n)
    (let loop ([i n])
      (if (= i 1)
          ((leaf-rope sum) (make-string max-leaf #\x))
          ((branch-rope sum) ((leaf-rope sum) (make-string max-leaf #\x)) (loop (sub1 i))))))

  ;; a loose-rope balance-halve for the tests below: build the side-folder from `sum`.
  (define (halve t) ((bisect (combine-info sum)) t))

  ;; --- build & read ---
  (define r ((make-rope sum) "abcdef"))
  (check-equal? (~a r) "abcdef")
  (check-equal? (sum r) 6)                          ; rope coerced -> cached summary

  ;; a build past max-leaf genuinely chunks, still round-trips and summarises
  (define r2 ((make-rope sum) "the quick brown fox jumps over the lazy dog"))
  (check-false  (leaf? r2))                         ; 43 chars > max-leaf -> a real branch
  (check-equal? (~a r2) "the quick brown fox jumps over the lazy dog")
  (check-equal? (sum r2) 43)

  ;; --- interleaving strings / ropes / summaries ---
  (check-equal? (sum "ab" r "x") (+ 2 6 1))
  (check-equal? (sum 5 r)        (+ 5 6))           ; a summary value passes through
  (check-equal? (sum "")         0)                 ; identity = (smr "")
  (check-equal? (sum)            0)                 ; (smr) = (string-summary "") -- empty, like ((make-rope smr))

  ;; assembling mixed parts into a rope
  (define joined ((make-rope sum) "(" r ")"))
  (check-equal? (~a joined) "(abcdef)")
  (check-equal? (sum joined) 8)

  ;; --- foreign rope: re-coerces its cached value (no same-algebra guard) ---
  (define sum2 (make-summary string-length +))            ; a different instance, same shape
  (check-equal? (sum2 r) 6)                          ; r built under `sum`; its cached value re-folds

  ;; --- bisect round-trips text ---
  (define-values (l rr) (halve r))
  (check-equal? (string-append (~a l) (~a rr)) "abcdef")

  ;; --- bisect is total: the empty rope splits into two empties ---
  (let-values ([(a b) (halve ((make-rope sum)))])
    (check-true (equal? a ((make-rope sum))))
    (check-true (equal? b ((make-rope sum)))))

  ;; --- suppose an empty IS produced (bisecting an atom): rope-join reabsorbs it,
  ;;     never branching it -- the empty-drop runs before any branch ---
  (let-values ([(lh rh) (halve ((make-rope sum) "x"))])   ; an atom -> one half is empty
    (check-equal? (~a (rope-join lh rh)) "x"))
  (let ([e ((make-rope sum))] [ab ((make-rope sum) "ab")])
    (check-true   (equal? (rope-join e e) e))             ; empties only -> empty
    (check-equal? (~a (rope-join e (rope-join ab e))) "ab"))  ; empties around content -> dropped

  ;; --- bisect rough-balances a spine toward weight-even (within ratio 3) halves ---
  (let-values ([(sl sr) (halve (spine 8))])
    (check-equal? (string-append (~a sl) (~a sr)) (make-string (* 8 max-leaf) #\x))  ; content
    (check-true (<= (max (rope-leaves sl) (rope-leaves sr))            ; within ratio (by leaves)
                    (+ (* 3 (min (rope-leaves sl) (rope-leaves sr))) 1))))

  ;; --- a guided cut lands at the exact char and fuses back to clean leaves ---
  (let* ([phrase ((make-rope sum) "the quick brown fox jumps over the lazy dog")]
         [at17 (lambda (L R) (cond [(< L 17) 1] [(> L 17) -1] [else 0]))])  ; cut at char 17
    (let-values ([(lft rgt) ((multisect sum (vector at17)) phrase)])
      (check-equal? (~a lft) "the quick brown f")    ; 17 chars
      (check-equal? (~a rgt) "ox jumps over the lazy dog")
      (check-equal? (string-append (~a lft) (~a rgt))
                    "the quick brown fox jumps over the lazy dog")))

  ;; --- rope rebalances a load that folds to a pathological spine. rebalance is now
  ;;     make-rope-internal, so its effect is checked here, on make-rope's output: a
  ;;     2048-char load folds to a 64-leaf spine, and comes out within the height bound ---
  (define big ((make-rope sum) (make-string 2048 #\x)))
  (check-equal? (~a big) (make-string 2048 #\x))                   ; content intact
  (check-true (<= (rope-height big)                                ; came out balanced, not a spine
                  (+ (* 3 (log (add1 (rope-leaves big)) 2)) 2)))

  ;; --- rope-join fuses small adjacent leaves into one ---
  (let ([j (rope-join ((make-rope sum) "ab") ((make-rope sum) "cd"))])
    (check-true   (leaf? j))                                          ; one leaf, not a branch
    (check-equal? (~a j) "abcd"))

  ;; --- seam-fuse: a small remainder split across a big sub-rope recombines (not scatters) ---
  (let* ([lf   (leaf-rope sum)]
         [br   (branch-rope sum)]
         [mid  ((make-rope sum) (make-string 3000 #\z))]                   ; a real multi-leaf rope
         [frag (br (lf "c") (br mid (lf "d")))]                       ; "c" stranded at mid's left
         [whole (rope-join (lf "ab") frag)])
    (define (leftmost t) (if (leaf? t) t (leftmost (branch-left t))))
    (check-equal? (~a whole) (string-append "abc" (make-string 3000 #\z) "d"))  ; content
    (check-equal? (leaf-text (leftmost whole)) "abc"))                          ; "ab"+"c" fused

  ;; --- a big build lands on max-leaf leaves: no oversized leaf, no scatter ---
  (let ([t ((make-rope sum) (make-string 3000 #\a))])
    (define (leaf-count t) (if (leaf? t) 1 (+ (leaf-count (branch-left t)) (leaf-count (branch-right t)))))
    (check-equal? (~a t) (make-string 3000 #\a))
    (check-equal? (leaf-count t) (ceiling (/ 3000 max-leaf)))
    (check-equal? (rope-leaves t) (leaf-count t)))   ; the cached field matches the walk

  ;; --- leaf count is tracked on every node, off the summary ---
  (check-equal? (rope-leaves r)  1)            ; "abcdef": 6 chars, one leaf
  (check-equal? (rope-leaves r2) 2))           ; 43 chars -> ceil(43/32) = 2 leaves
