#lang racket

;; ============================================================================
;; occur-summary.rkt  --  a word / substring-search SUMMARY for the
;; summarised rope.
;;
;;   ***  API UNSETTLED  ***  the object shape, field representation, names,
;;   and query surface are all still very raw.
;;
;; The summary value at a node is a word-AGNOSTIC function
;;     node -> (Word -> wmatch)
;; so ONE build answers any word; the word is supplied at query time.  For a
;; word W over a chunk the object is
;;
;;     (wmatch count right prefix len)
;;       count  -- full occurrences of W lying entirely inside the chunk
;;       right  -- partial-match prefix-lengths still live at the RIGHT end:
;;                 the ints of matches-in-progress a right neighbour may finish
;;       prefix -- the chunk's leading min(len,|W|) chars -- the LEFT end, as TEXT
;;       len    -- chunk length (used to re-base seam-spanning positions)
;;
;; Why the two ends are asymmetric (right = int list, left = string): combining
;; A.B counts seam-spanning matches by feeding A's live right-partials into B's
;; leading characters.  "Does B's start continue A's partial of length k" asks
;; whether B begins with the INTERIOR substring W[k..], which needs the actual
;; characters -- a bare prefix-length will not do.  So the right end can be ints
;; but the left end must carry text.  A fully symmetric int-list-both-ends form
;; is a candidate redesign; that it is not obvious which shape wins is exactly
;; why the API is flagged unsettled.
;;
;; combine is a pure pointwise monoid:  (L (+) R)(W) = seq (L W) (R W)  -- it runs
;; no matching, only defers.  The caching lives in ONE global capped trie
;; (toolbox/trie.rkt):  word -> cell -> wmatch, 64 warm words with 10000 cells
;; each.  A cell keys itself (procedure identity), so because the rope is
;; persistent, an unchanged subtree after an edit is the SAME cell object and
;; its cached wmatch is reused -- a re-query costs only the changed root-to-edit
;; path.  The queries enter through the STORED root cell (rope-summary), never
;; through the smr's coercion -- (occur-smr rope) builds a fresh wrapper
;; cell per call, and a fresh closure is a fresh KEY: junk in the shared map.
;; SIZING RULE (measured, not guessed): the per-word cap must exceed the total
;; live cells of the ropes actively re-queried for that word (~ chars/16 per
;; rope), else the ropes cycle the sub-map and evict each other wholesale --
;; LRU ping-pong, every re-query a full recompute.  Within one over-cap rope
;; the root still hits (a query fills post-order, root last), but an edit's
;; re-query loses its unchanged-subtree reuse.  10000 holds one ~150KB document
;; or dozens of small ones per word.  Evicting a cold word drops its whole
;; cell sub-map at once.  The trade accepted: cached cells are RETAINED until
;; they age out (bounded at 64x10000), where the old per-cell hashes died with
;; their nodes -- the price of having eviction at all.  The interface stays
;; referentially transparent.
;; ============================================================================

(require "../rope-core.rkt"
         (submod "../rope-core.rkt" internal)   ; leaf?/leaf-text/branch-left/-right/rope-summary
         "../toolbox/trie.rkt")

(provide (struct-out wmatch)
         occur-smr      ; the summary algebra: (make-summary leaf combine)
         occur-prop            ; root W   -> wmatch  (the summary object; the "prop" query)
         occur-count           ; root W   -> exact   (full occurrences of W)
         occur-select)         ; root W k -> exact   (absolute offset of the k-th match)

;; ---- the monoid on the data object -------------------------------------------

(struct wmatch (count right prefix len) #:transparent)

(define (has-prefix? s pre)
  (and (>= (string-length s) (string-length pre))
       (string=? (substring s 0 (string-length pre)) pre)))

;; scan a chunk for W from a fresh start: return (values match-positions right-live)
(define (scan-leaf W s)
  (define m (string-length W))
  (define (advance live c)
    (remove-duplicates
     (for/list ([p (in-list (cons 0 live))]           ; cons 0 = a fresh attempt at every char
                #:when (and (< p m) (char=? (string-ref W p) c)))
       (add1 p))))
  (define len (string-length s))
  (let loop ([i 0] [live '()] [acc '()])
    (if (= i len) (values (reverse acc) live)
        (let ([stepped (advance live (string-ref s i))])
          (if (memv m stepped)
              (loop (add1 i) (remove m stepped) (cons (- (+ i 1) m) acc))
              (loop (add1 i) stepped acc))))))

;; positions of matches that span the A|B seam: an A-right-partial of length k
;; completed by B's leading chars.  Returned as offsets measured from A's start.
(define (spanning W rpA lenA prefixB)
  (define m (string-length W))
  (for/list ([k (in-list (sort rpA >))]
             #:when (has-prefix? prefixB (substring W k m)))
    (- lenA k)))

(define (leaf-wm W s)
  (define-values (pos rp) (scan-leaf W s))
  (wmatch (length pos) rp
          (substring s 0 (min (string-length s) (string-length W)))
          (string-length s)))

(define (combine-wm W A B)
  (define m (string-length W)) (define la (wmatch-len A)) (define lb (wmatch-len B))
  (define span (spanning W (wmatch-right A) la (wmatch-prefix B)))
  ;; A right-partial of length k survives past a SHORT B that fully continues it
  (define survivors
    (if (< lb m)
        (for/list ([k (in-list (wmatch-right A))]
                   #:when (and (< (+ k lb) m)
                               (string=? (wmatch-prefix B) (substring W k (+ k lb)))))
          (+ k lb))
        '()))
  (wmatch (+ (wmatch-count A) (length span) (wmatch-count B))
          (remove-duplicates (append (wmatch-right B) survivors))
          (if (>= la m)
              (wmatch-prefix A)
              (substring (string-append (wmatch-prefix A) (wmatch-prefix B)) 0 (min (+ la lb) m)))
          (+ la lb)))

;; ---- the summary value: word -> wmatch, cached in ONE global capped trie ----
;; word -> cell -> wmatch.  A cell keys ITSELF (procedure identity -- equal? on
;; procedures is reference equality, no structural compare).

(define missing (string->uninterned-symbol "missing"))
(define-values (cache-put! cache-ref cache-clear! cache-walk)
  (make-trie #:caps '(64 10000)))

(define (cached cell W compute)
  (define hit (cache-ref missing W cell))
  (if (eq? hit missing)
      (let ([v (compute)]) (cache-put! v W cell) v)
      hit))

(define (leaf-cell s)
  (define (cell W) (cached cell W (lambda () (leaf-wm W s))))
  cell)

(define (combine-cell A B)
  (define (cell W) (cached cell W (lambda () (combine-wm W (A W) (B W)))))
  cell)

(define occur-smr (make-summary leaf-cell combine-cell))

;; ---- queries -----------------------------------------------------------------

(define (occur-prop root W) ((rope-summary root) W))   ; the STORED cell -- stable key, no wrapper
(define (occur-count root W) (wmatch-count (occur-prop root W)))

;; the k-th match's absolute offset, by an order-statistics descent on `count`
(define (occur-select root W k)
  (let descend ([node root] [k k] [base 0])
    (cond
      [(leaf? node)
       (define-values (pos _) (scan-leaf W (leaf-text node)))
       (+ base (list-ref pos k))]
      [else
       (define L (branch-left node)) (define R (branch-right node))
       (define wl ((rope-summary L) W))
       (define cL (wmatch-count wl)) (define lenL (wmatch-len wl))
       (cond
         [(< k cL) (descend L k base)]                            ; k-th match is inside the left
         [else
          (define seam (sort (spanning W (wmatch-right wl) lenL
                                       (wmatch-prefix ((rope-summary R) W))) <))
          (if (< k (+ cL (length seam)))
              (+ base (list-ref seam (- k cL)))                   ; ... straddles the seam
              (descend R (- k cL (length seam)) (+ base lenL)))])])))  ; ... in the right

;; ========== PARKED, PRIVATE: navigation -- guides over the occur summary =======
;; NOT provided, deliberately: nothing outside this file can reach any of it
;; (module privacy). It stays compiled and under test (module+ test sees
;; unexported bindings). To expose later: add provides, or re-wrap as a
;; (module+ experimental ...) submodule.
;;
;;   *** TODO(phil): NOT MY DESIGN -- this section was designed and landed by
;;   *** Claude (2026-07-10, from scratchpad sketches at session end). I have
;;   *** not internalized it: go through it to UNDERSTAND it, not just review
;;   *** it. Names, surface, and semantics all unvetted; the session's
;;   *** discussion is the only rationale record.
;;
;; Navigate with the TRANSIENT algebra: (multisect occur-nav-smr (occ-start W k)).
;; occur-nav-smr is the same monoid with cells that do NOT cache -- multisect's
;; framing builds throwaway wrapper cells per descent step, and caching those
;; pollutes the shared map with junk keys (measured: with the document near the
;; per-word cap, junk eviction cascades into an ~88x navigation slowdown).
;; The stored cells underneath still hit the cache; only wrappers compute through.
;; REMAINING LEAK (small): bisect halves leaves via rope-split, which builds the
;; half-leaves with the rope's OWN caching algebra -- ~2 junk cells per halving
;; level per navigation (~32 measured). Removing it needs a rope-core hook
;; (rope-split taking an smr); TODO alongside the re-review.
;;
;; occ-sand is the seam-read, sand-spines' analogue: at a cut, stitch L's live
;; right-partials to R's leading text -- the chars-since-start of every match
;; straddling the cut (several only when W overlaps itself), deepest first.
;; It exists because "a match has started here" is not a one-sided fact (L =
;; "a nee" may or may not contain a start, depending on R), so it cannot be a
;; stored field; both sides in hand, it is a cheap local read.
;;
;; Guides (comparators for multisect; L R arrive as framed cells):
;;   (occ-end W k)    cut at the END of the k-th occurrence. One-sided: completed
;;                    counts tick at ends, so this is just the count. Never 0 --
;;                    the cut is the -1/+1 boundary, bisect lands on it exactly.
;;   (occ-start W k)  cut at the START of the k-th occurrence. Needs occ-sand:
;;                    mid-match cuts must answer "started already", which only
;;                    the two-sided read knows. ~2x occ-end's cost.
;;   (at-offset W p)  cut at char offset p (wmatch-len as the ruler; any W).
;;   (occ-starts* W)  guide* splitting at EVERY start -- one multisect* walk,
;;                    variable piece count, match-free subtrees pruned. A match
;;                    at position 0 needs (and gets) no cut.
(require (submod "../rope-core.rkt" experimental))   ; make-guide*, for occ-starts*

(define (leaf-cell/transient s)      (lambda (W) (leaf-wm W s)))
(define (combine-cell/transient A B) (lambda (W) (combine-wm W (A W) (B W))))
(define occur-nav-smr (make-summary leaf-cell/transient combine-cell/transient))

(define (occ-sand W L R)
  (define m (string-length W))
  (define pre (wmatch-prefix (R W)))
  (for/list ([p (in-list (sort (wmatch-right (L W)) >))]
             #:when (string-prefix? pre (substring W p m)))
    p))

(define ((occ-end W k) L R)
  (if (<= (wmatch-count (L W)) k) 1 -1))

(define ((occ-start W k) L R)
  (define started (+ (wmatch-count (L W)) (length (occ-sand W L R))))
  (cond [(> started k) -1]                                           ; k-th start lies left
        [(and (= started k) (string=? (wmatch-prefix (R W)) W)) 0]   ; starts exactly here
        [else 1]))                                                   ; lies right

(define ((at-offset W p) L R)
  (define lenL (wmatch-len (L W)))
  (cond [(> lenL p) -1] [(= lenL p) 0] [else 1]))

;; guide*: answers (values left? mid? right?) from S(cut) = starts strictly
;; before the cut, evaluated at the three seams in view; left/right are the
;; S-differences over each child's span, minus the edge-exact start (owned by
;; mid? of whichever call has that seam).
(define (occ-starts* W)
  (define (S L R) (+ (wmatch-count (L W)) (length (occ-sand W L R))))
  (define (starts-here? R) (string=? (wmatch-prefix (R W)) W))
  (make-guide* occur-nav-smr
    (lambda (bs fsl fsr as)
      (define R2 (occur-nav-smr fsr as))          ; right of the middle seam
      (define R0 (occur-nav-smr fsl R2))          ; right of the left edge
      (define L1 (occur-nav-smr bs fsl))          ; left of the middle seam
      (define L2 (occur-nav-smr L1 fsr))          ; left of the right edge
      (define S0 (S bs R0)) (define S1 (S L1 R2)) (define S2 (S L2 as))
      (define at0 (starts-here? R0)) (define at1 (starts-here? R2))
      (values (positive? (- S1 S0 (if at0 1 0)))
              at1
              (positive? (- S2 S1 (if at1 1 0)))))))

;; ============================================================================
(module+ test
  (require rackunit)
  (define build (make-rope occur-smr))

  (define (brute text W)
    (for/list ([i (in-range 0 (add1 (- (string-length text) (string-length W))))]
               #:when (string=? (substring text i (+ i (string-length W))) W))
      i))

  ;; one word-agnostic build answers many words; count + select agree with brute
  (define text "a needle. a needled fox. needle box needle")
  (define doc (build text))
  (for ([W '("needle" "needled" "eedl" "ee" "box" "zzz" "e")])
    (check-equal? (occur-count doc W) (length (brute text W)) (format "count ~s" W))
    (for ([k (in-range (occur-count doc W))])
      (check-equal? (occur-select doc W k) (list-ref (brute text W) k) (format "select ~s ~a" W k))))

  ;; seam-spanning across chunks the build keeps as branches
  (define seam (build (make-string 40 #\.) "nee" "dle" (make-string 40 #\.)))
  (check-equal? (occur-count seam "needle") 1)
  (check-equal? (occur-select seam "needle" 0) 40)

  ;; the object at a chunk that ends mid-word carries the right-end partial
  (define m (occur-prop (build "aa need") "needle"))
  (check-equal? (wmatch-count m) 0)
  (check-equal? (wmatch-right m) '(4))          ; 4 chars ("need") pending at the right edge

  ;; the cache is warm after a query and enumerable: the word leads its sub-map
  (define warm '())
  (cache-walk (lambda (path v) (set! warm (cons (car path) warm))))
  (check-not-false (member "needle" warm) "needle cached")

  ;; combine is associative on the object (a lawful monoid)
  (define A (leaf-wm "aba" "ab")) (define B (leaf-wm "aba" "aab")) (define C (leaf-wm "aba" "a"))
  (check-equal? (combine-wm "aba" (combine-wm "aba" A B) C)
                (combine-wm "aba" A (combine-wm "aba" B C)))

  ;; empty chunk is the identity
  (define e (leaf-wm "aa" ""))
  (define x (leaf-wm "aa" "aaa"))
  (check-equal? (combine-wm "aa" e x) x)
  (check-equal? (combine-wm "aa" x e) x)

  ;; --- parked, private: navigation (module+ test sees unexported bindings) ---
  ;; multisect* comes via the main module's require of rope-core's experimental

  ;; occ-sand mid-match: 3 chars in, one straddler
  (check-equal? (occ-sand "needle" (occur-smr "a nee") (occur-smr "dle x")) '(3))
  (check-equal? (occ-sand "needle" (occur-smr "a ") (occur-smr "needle")) '())  ; clean cut

  ;; guide landings agree with select (the straddler-across-chunks included)
  (define ndoc (build "a needle here. " (make-string 20 #\.) " nee" "dle across. "
                      "last needle"))
  (define (cut-at g) (let-values ([(l r) ((multisect occur-nav-smr g) ndoc)])
                       (string-length (~a l))))
  (for ([k (in-range (occur-count ndoc "needle"))])
    (check-equal? (cut-at (occ-start "needle" k)) (occur-select ndoc "needle" k)
                  (format "start ~a" k))
    (check-equal? (cut-at (occ-end "needle" k)) (+ (occur-select ndoc "needle" k) 6)
                  (format "end ~a" k)))
  (check-equal? (cut-at (at-offset "needle" 17)) 17)

  ;; overlapping W: both straddlers counted, both starts reachable
  (define adoc (build "xx" "aaaa" "yy"))
  (for ([k (in-range 2)])
    (let-values ([(l r) ((multisect occur-nav-smr (occ-start "aaa" k)) adoc)])
      (check-equal? (string-length (~a l)) (+ 2 k))))

  ;; occ-starts*: one walk, variable pieces; position-0 and match-free cases
  (define (pieces doc W) (map ~a ((multisect* (occ-starts* W)) doc)))
  (check-equal? (pieces (build "see the needle; a needless needle remains") "needle")
                '("see the " "needle; a " "needless " "needle remains"))
  (check-equal? (pieces (build "needle at the very start") "needle")
                '("needle at the very start"))
  (check-equal? (pieces adoc "aaa") '("xx" "a" "aaayy"))
  (check-equal? (pieces (build "nothing here") "needle") '("nothing here"))

  ;; the transient algebra caches no WRAPPERS; what remains is rope-split's
  ;; leaf-halving junk (~2 cells/level/navigation, see the submodule header) --
  ;; bounded, unlike the O(depth^2) wrapper chains the nav algebra removes
  (define (entries-under W)
    (let ([n (box 0)])
      (cache-walk (lambda (path v) (when (equal? (car path) W) (set-box! n (add1 (unbox n))))))
      (unbox n)))
  (void (occur-count ndoc "needle"))               ; warm
  (define before-nav (entries-under "needle"))
  (for ([k (in-range (occur-count ndoc "needle"))])
    (let-values ([(l r) ((multisect occur-nav-smr (occ-start "needle" k)) ndoc)]) (void))
    (void ((multisect* (occ-starts* "needle")) ndoc)))
  (define growth (- (entries-under "needle") before-nav))
  (check < growth 250 "navigation junk bounded to leaf-halving"))  ; ~192 measured; wrapper chains would be 1000+)
