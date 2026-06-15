#lang racket

(require racket/generic)   ; define-generics -- the gen:summary-part extension point

;; Summarised rope: a persistent rope of text that caches a user-defined summary
;; at every node. Three factories make the whole surface:
;;
;;   smr        : (string | rope | summary)* -> summary  ; built by `make-summary`
;;   make-rope  : smr -> ((string | rope)* -> rope)      ; the rope factory (rebalances)
;;   multisect  : guides -> (rope -> piece values)       ; the one split primitive
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
;; A pure rope library: make, summarise, split (`multisect`), join (`make-rope`),
;; display. Guided navigation lives in `zipper-core.rkt`, built on these.
;;
;; Design notes: discussions/2026-05-29/2-claude.md (the variadic surface),
;; discussions/2026-06-01/1-claude.md (the cleanup this file is the rewrite of).

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
  ;; (multisect [guides]) -> splitter: t -> n+1 pieces as values; none -> balance halve.
  ;; Result arity is (add1 (vector-length guides)) -- inexpressible, so the range is `any`.
  [multisect    (->* () ((vectorof guide/c)) (-> rope? any))]
  ;; ((frame smr b a) g) -> g with the outer context baked in
  [frame        (-> smr/c any/c any/c (-> guide/c guide/c))]))
;; everything else is internal: leaf?/leaf-rope/branch-rope/empty-rope, concat-rope,
;; bisect, bisect-guided, split-leaf, split-leaf-at, rope-leaves/rope-height, within-ratio,
;; rebalance, pathological?, chunk-string, rope-write-text. Emptiness is
;; (equal? x ((make-rope smr))): the empty branch is unconstructable (branch guard), so
;; the only 0-leaf rope is the canonical empty leaf.

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
  ;; the only 0-leaf rope is ever the canonical empty leaf.
  #:guard (lambda (summary algebra leaves height left right _name)
            (when (or (zero? (rope-leaves left)) (zero? (rope-leaves right)))
              (error 'branch "empty child -- branches hold two non-empty ropes"))
            (values summary algebra leaves height left right)))

;; max-leaf: the target leaf size -- the fuse limit AND the chunk, so chunking,
;; splitting, and fusing all agree on how big a leaf wants to be. The one knob,
;; deliberately not a parameter: a per-rope size would have to travel with the
;; rope (concat recovers everything else from `rope-algebra`) and answer what
;; happens at a seam between ropes that disagree.
(define max-leaf 32)

;; ---------- summary ----------
;; (make-summary string-summary combine) -> smr, the variadic summary fn.
;;   (smr)            = (string-summary "")   ; the empty/identity, like ((make-rope smr))
;;   (smr str)        = (string-summary str)
;;   (smr a b c ...)  = combine, folded left-to-right (associative, not
;;                      commutative -- order is preserved)
;; Strings are measured, ropes contribute their cached summary (same-algebra
;; guarded), summary values pass through. Identity is (smr "") -- no separate
;; empty (string-summary is a monoid homomorphism). Knows nothing of `size`.

;; the open extension point: a `summary-part` is a value that knows how to
;; contribute itself to a summary under a given smr -- applying that smr selects
;; its part (a bundle value extracts one component; see summaries.rkt's `bundle`).
(define-generics summary-part
  (part->summary summary-part smr))

(define (make-summary string-summary combine)
  (define (smr . parts)
    (for/fold ([acc (string-summary "")])
              ([x (in-list parts)])
      (combine
       acc
       (cond
         [(string? x) (string-summary x)]
         [(rope? x)
          (if (eq? (rope-algebra x) smr)
              (rope-summary x)
              (error 'smr
                     "rope was summarised under a different summary; reconstruction unsupported"))]
         [(summary-part? x) (part->summary x smr)]   ; a part selects itself under smr
         [else x]))))                           ; already a summary value
  smr)

;; ---------- construction ----------
;; leaf-rope / branch-rope stamp the cached summary, leaf count, and height. A
;; content leaf is 1 leaf; the empty leaf is 0, so (zero? rope-leaves) still uniquely
;; marks the empty. empty-rope is the canonical empty leaf -- the only representable
;; empty, since concat drops empties before branching, so a branch is never empty.
(define ((leaf-rope smr) text)
  (leaf (smr text) smr (if (string=? text "") 0 1) 0 text))
(define ((branch-rope smr) l r)
  (branch (smr l r) smr
          (+ (rope-leaves l) (rope-leaves r))
          (add1 (max (rope-height l) (rope-height r)))
          l r))
(define (empty-rope smr) (leaf (smr "") smr 0 0 ""))

;; ---------- split ----------
;; Split a leaf (size >= 2) at its char midpoint into two leaves. A half's
;; summary can't be derived from the whole (combine has no inverse), so it is
;; re-measured -- which substrings anyway, so the halves are plain copies.
(define (split-leaf lf)
  (define smr  (rope-algebra lf))
  (define text (leaf-text lf))
  (define mid  (quotient (string-length text) 2))
  (values ((leaf-rope smr) (substring text 0 mid))
          ((leaf-rope smr) (substring text mid))))

;; A bisect weight guide reads the two boundary WEIGHTS (leaf counts) and returns a
;; sign, like the navigation guides elsewhere: 0 = balanced enough (stop), +1 =
;; right-heavy (borrow r->l), -1 = left-heavy (borrow l->r). `within-ratio` makes the
;; guide that calls a split good enough when the heavier side is within a ratio of the
;; lighter (plus 1 of slack, to ignore single-leaf granularity); the lazy heal uses a
;; loose ratio, `rebalance` a tighter one. The sign IS the weight direction -- which is
;; what bisect's overshoot guards rely on.
(define ((within-ratio a) l r)             ; l, r are WEIGHTS (leaf counts)
  (cond [(<= (max l r) (+ (* a (min l r)) 1)) 0]   ; good enough
        [(> r l)  1]                               ; right-heavy
        [else    -1]))                             ; left-heavy
(define heal-guide    (within-ratio 3))   ; bisect's default -- the lazy heal
(define rebuild-guide (within-ratio 2))   ; rebalance -- stricter, still not perfect

;; Bisect a non-atomic node into two halves the weight guide `g` calls balanced. A
;; leaf splits at its char midpoint; a branch rough-borrows across its boundary --
;; rotating the boundary child to the lighter side, [L [A B]] -> [[L A] B] (and the
;; mirror) -- until `g` reads 0 or a whole-sub-rope move would overshoot. `g` reads
;; the two leaf counts and returns a sign: 0 stop, +1 borrow r->l, -1 borrow l->r;
;; the sign is the heavy side, so the overshoot guard (the chunk to move is smaller
;; than the imbalance) keeps the borrow from flipping the rope further off-balance. The
;; concat invariant l ++ r = t holds throughout, so only the boundary branches are
;; rebuilt; every other sub-rope and every leaf is reused. All guided descent (in the
;; zipper) builds on this, healing as it goes. Total: a leaf under 2 chars splits into
;; an empty rope plus the rest, so the empty rope bisects to two empties.
(define (bisect t [g heal-guide])          ; g : weight weight -> {-1,0,1}
  (match t
    [(? leaf?) (split-leaf t)]
    [(branch _ smr _ _ l0 r0)
     (define br (branch-rope smr))
     (define (w x) (rope-leaves x))
     (let loop ([l l0] [r r0])
       (match (g (w l) (w r))                                  ; the guide reads the imbalance
         [0  (values l r)]                                     ; balanced enough -> done
         [1  (if (and (branch? r) (< (w (branch-left r))  (- (w r) (w l))))   ; borrow r->l, no overshoot
                 (loop (br l (branch-left r)) (branch-right r))
                 (values l r))]                                ; coarse boundary -- accept
         [-1 (if (and (branch? l) (< (w (branch-right l)) (- (w l) (w r))))   ; borrow l->r, no overshoot
                 (loop (branch-left l) (br (branch-right l) r))
                 (values l r))]))]))

;; A guide g : (L R) -> {-1,0,1} reads the FULL totals around a cut: +1 when the
;; boundary g names is RIGHT of the cut, -1 LEFT, 0 at it.  That stays true on a
;; sub-rope by framing: ((frame smr b a) g) is g with the outer context baked in --
;; the framed guide reads within-rope totals, g itself still sees full totals.
(define ((frame smr b a) g)
  (lambda (l r) (g (smr b l) (smr r a))))

;; Guided split: cut t at the boundary g names -- descend the boundary edge reading g
;; at each seam, threading the within-node accumulation, and split the straddling
;; leaf at the exact char.  l ++ r = t throughout.
(define (bisect-guided t g)
  (let ([smr (rope-algebra t)])
    (let descend ([b (smr "")] [t t] [a (smr "")])
      (match t
        [(? leaf?) (split-leaf-at smr b t a g)]
        [(branch _ _ _ _ l r)
         (match (g (smr b l) (smr r a))                            ; read the guide at the seam
           [ 1 (let-values ([(rl rr) (descend (smr b l) r a)])     ; boundary right of seam -> cut in r
                 (values ((concat-rope smr) l rl) rr))]
           [-1 (let-values ([(ll lr) (descend b l (smr r a))])     ; boundary left  of seam -> cut in l
                 (values ll ((concat-rope smr) lr r)))]
           [ 0 (values l r)])]))))                                 ; boundary exactly at the seam

;; split a leaf at the char where g flips: smallest i with (g L R) <= 0 (binary search;
;; g is monotone non-increasing in i, since growing the cut moves the boundary from
;; right to left).
(define (split-leaf-at smr b lf a g)
  (let* ([s    (leaf-text lf)]
         [sign (lambda (i) (g (smr b (substring s 0 i)) (smr (substring s i) a)))]
         [cut  (let search ([lo 0] [hi (string-length s)])
                 (if (>= lo hi)
                     lo
                     (let ([mid (quotient (+ lo hi) 2)])
                       (if (positive? (sign mid)) (search (add1 mid) hi) (search lo mid)))))])
    (values ((leaf-rope smr) (substring s 0 cut))
            ((leaf-rope smr) (substring s cut)))))

;; (multisect [guides]): guides (a vector of boundary guides) -> splitter.
;; ((multisect guides) t) cuts t at each guide's boundary left to right and returns
;; the n+1 pieces as values (p0 ++ ... ++ pn = t).  Guides read totals over t;
;; `frame` them if t sits in context.  NO guides -- (multisect) or #() -- is the
;; balance halve (`bisect`): two good-enough?, rough-borrowed halves -- an atom
;; halves to itself and an empty, on whichever side.
(define ((multisect [guides #()]) t)
  (let ([smr (rope-algebra t)])
    (if (zero? (vector-length guides))
        (bisect t)                                        ; the balance halve
        (for/fold ([rest t] [bAcc (smr "")] [pieces '()]
                   #:result (apply values (reverse (cons rest pieces))))
                  ([g (in-vector guides)])
          (let-values ([(l r) (bisect-guided rest ((frame smr bAcc (smr "")) g))])
            (values r (smr bAcc l) (cons l pieces)))))))

;; rebalance: rebuild t to a tighter balance by recursively bisecting with the
;; stricter `rebuild-guide`. Leaves are reused (never bisected); only branches are
;; rebuilt -- O(leaves * log) -- so it resets accumulated shape debt. The pathology
;; tier: a fresh load or a node that drifted too tall is run through it once.
(define (rebalance t)
  (if (leaf? t)
      t
      (let-values ([(l r) (bisect t rebuild-guide)])
        ((branch-rope (rope-algebra t)) (rebalance l) (rebalance r)))))

;; pathological?: height too tall for weight -- the scapegoat trigger. C=3 sits just
;; above the ~2.41 a ratio-3 rope guarantees (1/log2(4/3)), leaving the lazy heal
;; slack before a rebuild is forced; K=2 is constant slack for small ropes.
(define (pathological? t)
  (> (rope-height t) (+ (* 3 (log (add1 (rope-leaves t)) 2)) 2)))

;; ---------- join ----------
;; concat-rope: variadic join, folding the binary `join`. `join` drops empties,
;; fuses the seam when the two boundary leaves fit one leaf, else branches. It's the
;; inverse of bisect's split-leaf: descent splits a leaf, the rise's joins fuse it
;; back. To reach the seam it descends a boundary edge only through SMALL LEAF tips
;; (< max-leaf chars -- the bounded fragment splitting leaves) and stops at any real
;; sub-rope, so it stays O(1)-ish, never O(depth). Char count is read straight off
;; those boundary leaves (string-length is O(1)) -- the only place it is needed, now
;; that nodes cache leaf count, not char size. Balance-dumb: shape is bisect's job.
(define ((concat-rope smr) . ropes)
  (letrec ([mt   (empty-rope smr)]
           [br   (branch-rope smr)]
           [fuse (lambda (l r) ((leaf-rope smr) (string-append (leaf-text l) (leaf-text r))))]
           [chars (lambda (lf) (string-length (leaf-text lf)))]    ; O(1); only at the seam
           [join (match-lambda**
                  [((== mt) r) r]                                   ; empties drop
                  [(l (== mt)) l]
                  [((? leaf? l) (? leaf? r))                        ; two leaves at the seam:
                   (if (<= (+ (chars l) (chars r)) max-leaf)
                       (fuse l r)                                   ;   fuse if they fit,
                       (br l r))]                                   ;   else branch
                  [((branch _ _ _ _ ll lr) r)                       ; small right LEAF tip of l
                   #:when (and (leaf? lr) (< (chars lr) max-leaf))
                   (br ll (join lr r))]
                  [(l (branch _ _ _ _ rl rr))                       ; small left LEAF tip of r
                   #:when (and (leaf? rl) (< (chars rl) max-leaf))
                   (br (join l rl) rr)]
                  [(l r) (br l r)])])
    (foldr join mt ropes)))

(define (chunk-string text n)
  (for/list ([start (in-range 0 (string-length text) n)])
    (substring text start (min (string-length text) (+ start n)))))

;; ---------- rope (factory) ----------
;; ((make-rope smr) . parts) assembles strings (chunked into max-leaf leaves) and
;; ropes (passed through) by a dumb concat fold, then `rebalance`s the result if it
;; came out pathologically tall (a fresh load folds to a right-leaning spine).
(define ((make-rope smr) . parts)
  (define (->rope x)
    (if (string? x)
        (apply (concat-rope smr) (map (leaf-rope smr) (chunk-string x max-leaf)))
        x))
  (define t (apply (concat-rope smr) (map ->rope parts)))
  (if (pathological? t) (rebalance t) t))

;; ---------- display ----------
;; The walk behind prop:custom-write on `rope`. (~a r), (display r), and
;; (with-output-to-string (lambda () (display r))) all route through here.
(define (rope-write-text r port)
  (cond
    [(leaf? r)   (write-string (leaf-text r) port)]
    [(branch? r) (rope-write-text (branch-left r) port)
                 (rope-write-text (branch-right r) port)]))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; A trivial summary: character count.
  (define sum (make-summary string-length +))

  ;; an explicit right-leaning spine of one-char leaves -- bypasses concat's fuse, so
  ;; bisect/rebalance get a genuinely unbalanced rope to chew on.
  (define (spine str)
    (let loop ([cs (string->list str)])
      (if (null? (cdr cs))
          ((leaf-rope sum) (string (car cs)))
          ((branch-rope sum) ((leaf-rope sum) (string (car cs))) (loop (cdr cs))))))

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

  ;; --- same-summary guard ---
  (define sum2 (make-summary string-length +))            ; a different summary instance
  (check-exn exn:fail? (lambda () (sum2 r)))         ; r was built under `sum`

  ;; --- bisect round-trips text ---
  (define-values (l rr) (bisect r))
  (check-equal? (string-append (~a l) (~a rr)) "abcdef")

  ;; --- bisect is total: the empty rope splits into two empties ---
  (let-values ([(a b) (bisect ((make-rope sum)))])
    (check-true (equal? a ((make-rope sum))))
    (check-true (equal? b ((make-rope sum)))))

  ;; --- suppose an empty IS produced (bisecting an atom): concat reabsorbs it, never
  ;;     branching it -- the empty-drop runs before any branch in `join` ---
  (let-values ([(lh rh) (bisect ((make-rope sum) "x"))])   ; an atom -> one half is empty
    (check-equal? (~a ((concat-rope sum) lh rh)) "x"))
  (let ([e ((make-rope sum))] [ab ((make-rope sum) "ab")])
    (check-true   (equal? ((concat-rope sum) e e) e))     ; empties only -> empty
    (check-equal? (~a ((concat-rope sum) e ab e)) "ab"))  ; empties around content -> dropped

  ;; --- bisect rough-borrows a spine toward weight-even (within ratio 3) halves ---
  (let-values ([(sl sr) (bisect (spine "abcdefgh"))])
    (check-equal? (string-append (~a sl) (~a sr)) "abcdefgh")          ; content preserved
    (check-true (<= (max (rope-leaves sl) (rope-leaves sr))            ; within ratio (by leaves)
                    (+ (* 3 (min (rope-leaves sl) (rope-leaves sr))) 1))))

  ;; --- rope rebalances a load that folds to a pathological spine ---
  (define big ((make-rope sum) (make-string 2048 #\x)))   ; 64 max-leaf chunks -> a tall spine
  (check-equal? (~a big) (make-string 2048 #\x))                   ; content intact
  (check-false (pathological? big))                               ; came out balanced, not a spine

  ;; --- rebalance turns a pathological spine into a non-pathological rope ---
  (let ([sp (spine "abcdefghijklmnop")])             ; 16-leaf right spine
    (check-true   (pathological? sp))                 ; tall for its weight
    (define b (rebalance sp))
    (check-false  (pathological? b))                  ; now balanced
    (check-equal?  (~a b) "abcdefghijklmnop"))        ; content preserved

  ;; --- concat fuses small adjacent leaves into one ---
  (let ([j ((concat-rope sum) ((make-rope sum) "ab") ((make-rope sum) "cd"))])
    (check-true   (leaf? j))                                          ; one leaf, not a branch
    (check-equal? (~a j) "abcd"))

  ;; --- seam-fuse: a small remainder split across a big sub-rope recombines (not scatters) ---
  (let* ([lf   (leaf-rope sum)]
         [br   (branch-rope sum)]
         [mid  ((make-rope sum) (make-string 3000 #\z))]                   ; a real multi-leaf rope
         [frag (br (lf "c") (br mid (lf "d")))]                       ; "c" stranded at mid's left
         [whole ((concat-rope sum) (lf "ab") frag)])
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
