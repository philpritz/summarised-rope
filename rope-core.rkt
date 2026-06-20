#lang racket

(require racket/generic
         (only-in "helper-algebras.rkt" on))

;; Summarised rope: a persistent rope of text that caches a user-defined summary
;; at every node. Three factories make the whole surface:
;;
;;   smr        : (string | rope | summary)* -> summary  ; built by `make-summary`
;;   make-rope  : smr -> ((string | rope)* -> rope)      ; the rope factory (rebalances)
;;   multisect  : smr [guides] -> (rope -> piece values) ; the one split primitive
;;
;; Nodes participate in Racket's display/write protocol (prop:custom-write on
;; `rope`, inherited), so (~a r), (display r), (with-output-to-string ...) all
;; yield/emit the text -- no bespoke rope->string.
;;
;; A pure rope library: make, summarise, split (`multisect`), join (`rope-join`),
;; display. Guided navigation lives in `zipper-core.rkt`, built on these.
;;
;; The file is in two halves (an abstraction barrier; banners below):
;;  - PART 1 -- the dumb structural rope.
;;  - PART 2 -- the guided/balanced descent (bisect, multisect, make-rope); where balancing lives.
;;
;; Design notes: discussions/2026-05-29/2-claude.md (the variadic surface),
;; discussions/2026-06-01/1-claude.md (the cleanup this file is the rewrite of),
;; discussions/2026-06-19 (this abstraction-barrier rewrite; see deprecated/deprecated-5/).

(define smr/c   procedure?)
(define guide/c (-> any/c any/c (or/c -1 0 1)))

(provide
 rope?
 gen:summary-part part->summary summary-part?
 (contract-out
  [make-summary (-> (-> string? any/c) (-> any/c any/c any/c) smr/c)]
  [make-rope    (-> smr/c (->* () #:rest (listof (or/c string? rope?)) rope?))]
  [multisect    (->* (smr/c) ((vectorof guide/c)) (-> rope? any))]
  [frame        (-> smr/c any/c any/c (-> guide/c guide/c))]))

(module+ internal
  (provide rope-leaves rope-height
           leaf? branch? leaf-text
           branch-left branch-right))

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
;; `algebra` caches the summary fn a node was built under, so `smr` can recover it.
(struct rope (summary algebra leaves height) #:transparent
  #:property prop:custom-write (lambda (r port mode) (rope-write-text r port)))
(struct leaf   rope (text)       #:transparent)   ; ctor: (leaf summary algebra leaves height text)
(struct branch rope (left right) #:transparent
  ;; no empty child -> emptiness is just (equal? x ((make-rope smr)))
  #:guard (lambda (summary algebra leaves height left right _name)
            (when (or (zero? (rope-leaves left)) (zero? (rope-leaves right)))
              (error 'branch "empty child -- branches hold two non-empty ropes"))
            (values summary algebra leaves height left right)))

;; max-leaf: the target leaf size -- fuse limit and chunk size both.
(define max-leaf 32)

;; ---------- summary (public; see scribble/rope-core.scrbl) ----------
;; the open extension point: a `summary-part` contributes itself to a summary under a
;; given smr -- applying smr selects its part (e.g. a bundle value; see summaries.rkt).
(define-generics summary-part
  (part->summary summary-part smr))

(define (make-summary string-summary combine)   ; string-summary is a monoid homomorphism
  (define id (string-summary ""))               ; the identity, built once
  (define (coerce x)
    (match x
      [""                                        id]
      [(? string?)                               (string-summary x)]
      [(? rope?)                                 (smr (rope-summary x))]   ; re-coerce: own rope passes back by identity, bundle rope selects its component slot
      [(? summary-part?)                         (part->summary x smr)]
      [_                                         x]))
  ;; foldl not foldr: the law battery (summary-laws.rkt) leans on a left fold.
  (define (smr . parts)
    (foldl (lambda (x acc) (combine acc x)) id (map coerce parts)))
  smr)

;; ---------- construction ----------
;; cache smr itself in each node -- the hook for make-summary to check a rope is built
;; under the same summary (no such guard yet; see scribble).
(define ((leaf-rope smr) text)
  (if (string=? text "")
      (leaf (smr "") smr 0 0 "")
      (leaf (smr text) smr 1 0 text)))
(define ((branch-rope smr) l r)
  (branch (smr l r) smr
          (+ (rope-leaves l) (rope-leaves r))
          (add1 (max (rope-height l) (rope-height r)))
          l r))

;; ---------- join ----------
;; rope-join: rope-split's inverse. Fusing keeps the no-fusable-pair invariant, so one
;; fusing join serves both balance and guided cuts (see PART 1 / the boundary).
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
;; The surface PART 2 speaks through (with `rope-join` above); below here PART 2 names no struct field.
;;  - the algebra arrives as a PARAMETER: `smr` to multisect, `cmb` (= (combine-info smr)) to bisect/frame.
;;  - a "side" is (summary weight height): summary and weight are exact monoid measures, height is
;;    NOMINAL under combine-info (max-folded, never read).
;;  - invariant (rope-join (rope-split t)) = t (see PART 1): one fusing join then serves both cuts.
;;    Holds because ropes reach PART 2 only via rope-join (make-rope, guided pieces, zipper rebuilds),
;;    never hand-built.

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
;; within-ratio: a weight guide (Guides, scribble/rope-core.scrbl); +1 is slack. 3 heal, 2 rebalance.
(define ((within-ratio a) l r)
  (cond [(<= (max l r) (+ (* a (min l r)) 1)) 0]
        [(> r l)  1]
        [else    -1]))

;; ---------- bisect: the one descent ----------
;; Cut t into two halves (l ++ r = t), descending by `decide` -- a guide on sides (see Guides).
;; `before`/`after` thread t's outer context: empty standalone, baked by `frame` (as multisect does).
;; The leaf is not special -- a guided cut inside a leaf recurses into its halves (binary search).
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
;; (public; see scribble/rope-core.scrbl) -- `combine` is variously smr and, in this file, cmb.
(define ((frame combine b a) g)
  (lambda (l r) (g (combine b l) (combine r a))))

;; ---------- multisect ----------
;; (public; see scribble/rope-core.scrbl) Each guided cut runs bisect over the remaining tail with
;; the guide framed by `bacc` -- the side of all cut so far -- so it judges against the whole doc.
(define ((multisect smr [guides #()]) t)
  (define cmb (combine-info smr))                              ; smr -> the side-folder, once
  (if (zero? (vector-length guides))
      ((bisect cmb) t)                                  ; no guides -> a rough balancing bisect
      (for/fold ([rest t] [bacc (rope-zero t)] [pieces '()]
                 #:result (apply values (reverse (cons rest pieces))))
                ([g (in-vector guides)])
        (let-values ([(l r) ((bisect cmb) rest ((frame cmb bacc (rope-zero t)) (on g first)))])
          (values r (cmb bacc (rope-info l)) (cons l pieces))))))

;; ---------- make-rope ----------
;; (public; see scribble/rope-core.scrbl) The factory mirrors make-summary (empty + helpers once).
;; Its one addition over a dumb fold is the rebuild tier: a fresh load folds to a right-leaning
;; spine, so a pathologically tall result is rebalanced (pathological?/rebalance below).
(define (make-rope smr)
  (define mt    ((leaf-rope smr) ""))
  (define cmb   (combine-info smr))
  (define halve (bisect cmb))
  (define (chunk s)
    (for/list ([start (in-range 0 (string-length s) max-leaf)])
      (substring s start (min (string-length s) (+ start max-leaf)))))
  (define (coerce x)
    (match x
      [""          mt]
      [(? string?) (foldr rope-join mt (map (leaf-rope smr) (chunk x)))]
      [_           x]))
  ;; pathological?: height too tall for weight. C=3 just above the ~2.41 a ratio-3 rope
  ;; guarantees (1/log2(4/3)); K=2 is small-rope slack.
  (define (pathological? t)
    (match-define (list _ leaves height) (rope-info t))
    (> height (+ (* 3 (log (add1 leaves) 2)) 2)))
  ;; rebalance: rebuild tighter via (within-ratio 2); leaves reused, only branches rebuilt.
  ;; rope-join branches (not fuses) on a well-formed rope, so it keeps the shape.
  (define (rebalance t)
    (if (zero? (third (rope-info t)))
        t
        (let-values ([(l r) (halve t (on (within-ratio 2) second))])
          (rope-join (rebalance l) (rebalance r)))))
  (define (build . parts)
    (define t (foldr rope-join mt (map coerce parts)))
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
