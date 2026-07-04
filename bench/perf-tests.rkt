#lang racket

;; Performance / stress tests for the summarised rope's editing layer.  Where
;; bench/bench.rkt micro-benchmarks single ops across a size axis, this file drives
;; the rope under sustained RANDOM EDIT SEQUENCES and scaling sweeps and reports
;; per-op cost.  Like bench.rkt it is a tool, off the CLAUDE.md orientation import
;; list; it carries no `module+ test`, so it never runs in the correctness suite.
;; Run it directly (DrRacket's Run, or `racket bench/perf-tests.rkt`) to execute all
;; three suites, or require it and call one:
;;
;;   edit-band-perf   an OU-driven mixed edit sequence (move / insert / delete) held
;;                    inside a hard size band; every step checked against three
;;                    oracles -- content (rope renders to a plain-string model), band
;;                    (size stays in range), balance (not pathological) -- then a
;;                    timed long run for per-op cost.
;;   scaling-perf     per-op edit cost vs document size across orders of magnitude
;;                    (100 .. 1e6 chars), with SMALL absolute edits so it isolates
;;                    navigation / reassembly cost, not insert-string building;
;;                    per-op time should grow ~log.
;;   sexp-form-perf   random WHOLE-FORM sexp editing (insert / delete / replace a
;;                    form), addressed by spine reads (sand-spines + sexp-guides),
;;                    checked against a form-list model and a reparse-count oracle.
;;
;; Balance is read off the rope's transparent nodes by struct->vector reflection --
;; POSITIONAL (leaves = field 3, height = field 4); if the rope struct is ever
;; reordered this breaks silently.  A `diag` submodule with named accessors would be
;; the robust home if these are ever promoted past scratch.

(require "../text-edit/sexp-edit.rkt"               ; re-exports rope-core / zipper-core / summaries
         (submod "../text-edit/sexp-edit.rkt" gen)  ; gen:shape gen:populate tree->text (sexp-form suite)
         rackcheck                        ; gen:resize gen:bind sample (sexp-form suite)
         racket/math)                     ; pi, exact-round

(provide edit-band-perf scaling-perf sexp-form-perf)

;; ---------- shared helpers ----------
;; balance measure, read externally off the transparent rope (POSITIONAL -- see header)
(define (rope-leaves* r) (vector-ref (struct->vector r) 3))
(define (rope-height* r) (vector-ref (struct->vector r) 4))
(define (tallness r) (exact->inexact (/ (rope-height* r) (log (add1 (rope-leaves* r)) 2))))
;; rope-core's OWN pathological? formula: height > 3*log2(leaves+1)+2.  The +2 slack
;; matters at small leaf counts -- a bare tallness<3 proxy is too strict there.
(define (pathological?* r) (> (rope-height* r) (+ (* 3 (log (add1 (rope-leaves* r)) 2)) 2)))

(define (median xs)
  (define s (sort xs <)) (define k (length s))
  (if (odd? k) (list-ref s (quotient k 2))
      (/ (+ (list-ref s (sub1 (quotient k 2))) (list-ref s (quotient k 2))) 2.0)))

;; char summary + relative (ratio) guides: a boundary at fraction p of the whole.
(define cc (make-summary string-length +))
(define ((at-frac p) L R)                  ; boundary at p of the whole (L+R = total chars)
  (define k (exact-round (* p (+ L R))))
  (cond [(< L k) 1] [(> L k) -1] [else 0]))
(define (gap-at p)   (vector (at-frac p) (at-frac p)))
(define (seg-at a b) (vector (at-frac a) (at-frac b)))
(define (idx n p) (exact-round (* (max 0.0 (min 1.0 p)) n)))

;; ============================================================================
;; Suite 1: edit-band -- an OU-driven mixed edit sequence in a hard size band,
;; checked against content / band / balance oracles, then timed.
(define (edit-band-perf)
  (random-seed 7)

  ;; ---- noise ----
  (define (randn)                                    ; standard normal, Box-Muller
    (* (sqrt (* -2.0 (log (- 1.0 (random))))) (cos (* 2.0 pi (random)))))

  ;; ---- OU in a latent coord, squashed into a HARD size band ----
  (define (logistic y) (/ 1.0 (+ 1.0 (exp (- y)))))          ; R -> (0,1)
  (define (logit p)    (log (/ p (- 1.0 p))))                ; (0,1) -> R
  (define ((squash lo hi) y) (+ lo (* (- hi lo) (logistic y))))
  (define ((ou-y muY var stiff) y)
    (define s (sqrt (* var stiff (- 2 stiff))))
    (+ y (* stiff (- muY y)) (* s (randn))))

  ;; ---- band config ----
  (define LO 100.0) (define HI 4000.0) (define N* 800.0)
  (define muY (logit (/ (- N* LO) (- HI LO))))
  (define sq  (squash LO HI))
  (define stepY (ou-y muY 0.8 0.3))

  ;; ---- one op: ratio movement, or OU-driven insert/delete ----
  (define (do-op z model y kind p q)
    (define n (string-length model))
    (case kind
      [(move-gap) (values (((opt-set zipper-guide) (gap-at p)) z) model y
                          (format "move-gap @~a" (~r p #:precision '(= 2))))]
      [(move-seg) (define a (min p q)) (define b (max p q))
                  (values (((opt-set zipper-guide) (seg-at a b)) z) model y
                          (format "move-seg [~a ~a]" (~r a #:precision '(= 2)) (~r b #:precision '(= 2))))]
      [else (define y* (stepY y))
            (define dn (- (exact-round (sq y*)) n))
            (cond
              [(>= dn 0) (define i (idx n p)) (define s (make-string dn #\x))
                         (values (((opt-set zipper-focus) s) (((opt-set zipper-guide) (gap-at p)) z))
                                 (string-append (substring model 0 i) s (substring model i)) y*
                                 (format "insert ~a @~a" dn (~r p #:precision '(= 2))))]
              [else (define d (min (- dn) (max 0 (sub1 n))))
                    (define i (idx (- n d) p)) (define j (+ i d))
                    (values (((opt-set zipper-focus) "") (((opt-set zipper-guide) (seg-at (/ i n) (/ j n))) z))
                            (string-append (substring model 0 i) (substring model j)) y*
                            (format "delete ~a @~a" d (~r p #:precision '(= 2))))])]))

  ;; ---- run a mixed sequence, checking all three oracles each step ----
  (define STEPS 24)
  (define start-doc (make-string (exact-round N*) #\a))
  (define z0 (start cc ((make-rope cc) start-doc) (gap-at 0.0)))
  (define kinds (vector 'move-gap 'move-seg 'edit 'edit 'edit))

  (printf "OU edit sequence: band [~a,~a] centre ~a   (~a steps, seed 7)\n"
          (exact-round LO) (exact-round HI) (exact-round N*) STEPS)
  (printf "~a ~a ~a ~a ~a ~a ~a\n"
          (~a "#" #:min-width 3) (~a "op" #:min-width 16) (~a "len" #:min-width 6)
          (~a "lvs" #:min-width 4) (~a "ht" #:min-width 4) (~a "tall" #:min-width 6) "checks")

  (define-values (bad-c bad-b bad-p max-t)
    (for/fold ([z z0] [model start-doc] [y muY] [bc 0] [bb 0] [bp 0] [mt 0.0]
               #:result (values bc bb bp mt))
              ([i STEPS])
      (define kind (vector-ref kinds (random (vector-length kinds))))
      (define-values (z* model* y* label) (do-op z model y kind (random) (random)))
      (define root ((opt-get zipper-focus) (to-root z*)))
      (define len  (string-length model*))
      (define lvs  (rope-leaves* root))
      (define ht   (rope-height* root))
      (define tall (tallness root))
      (define okc  (equal? (~a root) model*))             ; content oracle
      (define okb  (and (<= LO len) (<= len HI)))         ; band oracle
      (define okt  (not (pathological?* root)))           ; balance oracle (rope-core's formula)
      (printf "~a ~a ~a ~a ~a ~a ~a\n"
              (~a i #:min-width 3) (~a label #:min-width 16) (~a len #:min-width 6)
              (~a lvs #:min-width 4) (~a ht #:min-width 4)
              (~a (~r tall #:precision '(= 2)) #:min-width 6)
              (string-append (if okc "content " "MISMATCH ")
                             (if okb "band " "OOB ")
                             (if okt "ok" "PATHOLOGICAL")))
      (values z* model* y* (+ bc (if okc 0 1)) (+ bb (if okb 0 1)) (+ bp (if okt 0 1)) (max mt tall))))

  (printf "\n~a steps: ~a content mismatches, ~a out-of-band, ~a pathological; peak tallness ~a\n"
          STEPS bad-c bad-b bad-p (~r max-t #:precision '(= 2)))

  ;; ---- timed long run: B batches of K ops, per-batch per-op time.  No model/oracle
  ;; here -- (~a root) and the substring/append model are O(n) and would dominate the
  ;; edit cost (correctness was checked above).  We carry only the integer size n;
  ;; tallness is read at batch boundaries (untimed). ----
  (define (timed-op z n y kind p q)
    (case kind
      [(move-gap) (values (((opt-set zipper-guide) (gap-at p)) z) n y)]
      [(move-seg) (let ([a (min p q)] [b (max p q)])
                    (values (((opt-set zipper-guide) (seg-at a b)) z) n y))]
      [else (define y* (stepY y))
            (define dn (- (exact-round (sq y*)) n))
            (cond
              [(>= dn 0) (values (((opt-set zipper-focus) (make-string dn #\x)) (((opt-set zipper-guide) (gap-at p)) z))
                                 (+ n dn) y*)]
              [else (define d (min (- dn) (max 0 (sub1 n))))
                    (define i (idx (- n d) p)) (define j (+ i d))
                    (values (((opt-set zipper-focus) "") (((opt-set zipper-guide) (seg-at (/ i n) (/ j n))) z))
                            (- n d) y*)])]))

  (define BATCH 500) (define BATCHES 20)
  (printf "\ntimed long run: ~a batches x ~a ops = ~a ops (size-only state, no oracle)\n"
          BATCHES BATCH (* BATCHES BATCH))
  (printf "~a ~a ~a ~a ~a\n"
          (~a "batch" #:min-width 6) (~a "ops" #:min-width 7) (~a "size" #:min-width 6)
          (~a "us/op" #:min-width 8) (~a "tall" #:min-width 6))

  (define batch-usop
    (let ([z1 (start cc ((make-rope cc) start-doc) (gap-at 0.0))])
      (for/fold ([z z1] [n (string-length start-doc)] [y muY] [ts '()] #:result (reverse ts))
                ([b (in-range BATCHES)])
        (define t0 (current-inexact-milliseconds))
        (define-values (z* n* y*)
          (for/fold ([z z] [n n] [y y]) ([_ (in-range BATCH)])
            (timed-op z n y (vector-ref kinds (random (vector-length kinds))) (random) (random))))
        (define usop (* 1000.0 (/ (- (current-inexact-milliseconds) t0) BATCH)))
        (define tall (tallness ((opt-get zipper-focus) (to-root z*))))           ; untimed
        (printf "~a ~a ~a ~a ~a\n"
                (~a b #:min-width 6) (~a (* (add1 b) BATCH) #:min-width 7) (~a n* #:min-width 6)
                (~a (~r usop #:precision '(= 2)) #:min-width 8)
                (~a (~r tall #:precision '(= 2)) #:min-width 6))
        (values z* n* y* (cons usop ts)))))

  (printf "\nper-op over ~a ops: min ~a us  median ~a us  mean ~a us  (min = cleanest)\n"
          (* BATCHES BATCH)
          (~r (apply min batch-usop) #:precision '(= 2))
          (~r (median batch-usop) #:precision '(= 2))
          (~r (/ (apply + batch-usop) (length batch-usop)) #:precision '(= 2))))

;; ============================================================================
;; Suite 2: scaling -- per-op edit cost vs document size across orders of magnitude.
;; Same ratio-addressed moves/inserts/deletes, but edit SIZES are small & absolute
;; (1..40 chars) so we measure the rope's navigation/reassembly cost, not the
;; building of huge insert strings.  Size is held near each target by light
;; mean-reversion.  Per-op time should grow ~log.
(define (scaling-perf)
  (random-seed 11)

  ;; one op: 30% move, else a small insert/delete biased to revert toward target N*.
  (define (op z n N*)
    (cond
      [(< (random) 0.30) (values (((opt-set zipper-guide) (gap-at (random))) z) n)]      ; move
      [else
       (define s (add1 (random 40)))                                          ; small, absolute
       (define grow? (if (< n N*) (< (random) 0.7) (< (random) 0.3)))         ; revert toward N*
       (if grow?
           (values (((opt-set zipper-focus) (make-string s #\x)) (((opt-set zipper-guide) (gap-at (random))) z)) (+ n s))
           (let* ([d (min s (max 0 (sub1 n)))] [p (random)]
                  [i (idx (- n d) p)] [j (+ i d)])
             (values (((opt-set zipper-focus) "") (((opt-set zipper-guide) (seg-at (/ i n) (/ j n))) z)) (- n d))))]))

  (define (run N* K)                                  ; build N*-char rope, time K ops
    (define z0 (start cc ((make-rope cc) (make-string N* #\a)) (gap-at 0.0)))
    (define-values (zw nw)                            ; warmup (untimed)
      (for/fold ([z z0] [n N*]) ([_ (in-range (quotient K 4))]) (op z n N*)))
    (define t0 (current-inexact-milliseconds))
    (define-values (zf nf)
      (for/fold ([z zw] [n nw]) ([_ (in-range K)]) (op z n N*)))
    (define usop (* 1000.0 (/ (- (current-inexact-milliseconds) t0) K)))
    (define root ((opt-get zipper-focus) (to-root zf)))
    (values usop (rope-leaves* root) (rope-height* root) (tallness root)))

  (run 1000 200)                                      ; global JIT warmup, discarded

  (printf "per-op edit cost vs document size  (small abs edits, ~~constant size, ~a ops each)\n" 1500)
  (printf "~a ~a ~a ~a ~a\n"
          (~a "size" #:min-width 9) (~a "leaves" #:min-width 8) (~a "height" #:min-width 8)
          (~a "tall" #:min-width 6) (~a "us/op" #:min-width 9))
  (for ([N* '(100 1000 10000 100000 1000000)])
    (define-values (usop lvs ht tall) (run N* 1500))
    (printf "~a ~a ~a ~a ~a\n"
            (~a N* #:min-width 9) (~a lvs #:min-width 8) (~a ht #:min-width 8)
            (~a (~r tall #:precision '(= 2)) #:min-width 6)
            (~a (~r usop #:precision '(= 2)) #:min-width 9))))

;; ============================================================================
;; Suite 3: sexp-form -- random WHOLE-FORM sexp editing.  Each op cuts at a
;; form-boundary char count, reads the spine there (sand-spines), and navigates with
;; sexp-guides to insert / delete / replace a whole form.  Generated forms come from
;; sexp-edit's `gen` submodule.  Well-formedness trick: every form is parenthesized
;; and forms are concatenated with NO separators -- self-delimiting "(a)(b)(c)" -- so
;; every boundary is a clean paren junction and there are no separators to bookkeep.
;; A list model is the oracle.
(define (sexp-form-perf)
  (random-seed 5)

  ;; a random parenthesized form (wrap bare atoms so EVERY form has parens)
  (define (a-form)
    (define t (car (sample (gen:resize (gen:bind (gen:shape 3 2) gen:populate) 6) 1)))
    (if (string? t) (string-append "(" t ")") (tree->text t)))

  ;; model = list of form strings; document = concatenation (self-delimiting)
  (define (render m) (apply string-append m))
  (define (offsets m)                                  ; start offset of each form, plus the end
    (reverse (foldl (lambda (f acc) (cons (+ (car acc) (string-length f)) acc)) '(0) m)))

  ;; read the FRONT (left-anchored) and BACK (right-anchored) spines at char cut i --
  ;; the "cut at char count, read spine" step.  A segment uses front for its START
  ;; edge (which watches the left) and back for its END edge (watches the right); the
  ;; back anchoring is what makes the document-end boundary navigable.
  (define (front-at text i)
    (let-values ([(f b) (sand-spines (sexp-smr (substring text 0 i)) (sexp-smr (substring text i)))]) f))
  (define (back-at text i)
    (let-values ([(f b) (sand-spines (sexp-smr (substring text 0 i)) (sexp-smr (substring text i)))]) b))

  ;; ---- the three edits: each reads spine(s) at form boundaries, navigates, swaps ----
  (define (do-insert z model k)                        ; insert a new form before form k
    (define text (render model)) (define a (list-ref (offsets model) k))
    (define nf (a-form))
    (values (((opt-set zipper-focus) nf) (((opt-set zipper-guide) (sexp-guides (front-at text a))) z))
            (append (take model k) (list nf) (drop model k))))
  (define (do-delete z model k)                        ; delete form k
    (define text (render model)) (define os (offsets model))
    (define a (list-ref os k)) (define b (list-ref os (add1 k)))
    (values (((opt-set zipper-focus) "") (((opt-set zipper-guide) (sexp-guides (front-at text a) (back-at text b))) z))
            (append (take model k) (drop model (add1 k)))))
  (define (do-replace z model k)                       ; replace form k with a fresh one
    (define text (render model)) (define os (offsets model))
    (define a (list-ref os k)) (define b (list-ref os (add1 k))) (define nf (a-form))
    (values (((opt-set zipper-focus) nf) (((opt-set zipper-guide) (sexp-guides (front-at text a) (back-at text b))) z))
            (append (take model k) (list nf) (drop model (add1 k)))))

  ;; readable?: parse the whole doc, return how many top-level forms it yields (or #f)
  (define (read-count s)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (define p (open-input-string s))
      (let loop ([n 0]) (define x (read p)) (if (eof-object? x) n (loop (add1 n))))))

  ;; ---- run a random sequence, checking the oracle each step ----
  (define STEPS 20)
  (define model0 (build-list 6 (lambda (_) (a-form))))
  (define text0  (render model0))
  (define z0 (start sexp-smr ((make-rope sexp-smr) text0) (sexp-guides (front-at text0 0))))

  (printf "random whole-form sexp editing (~a steps, start ~a forms)\n" STEPS (length model0))
  (printf "~a ~a ~a ~a ~a\n" (~a "#" #:min-width 3) (~a "op" #:min-width 12)
          (~a "forms" #:min-width 6) (~a "checks" #:min-width 22) "doc")

  (define bad
    (for/fold ([z z0] [model model0] [bad 0] #:result bad) ([i STEPS])
      (define len (length model))
      (define op (cond [(< len 4) 'insert] [(> len 14) 'delete]
                       [else (case (random 3) [(0) 'insert] [(1) 'delete] [else 'replace])]))
      (define k (case op [(insert) (random (add1 len))] [else (random len)]))
      (define-values (z* model*)
        (case op [(insert) (do-insert z model k)] [(delete) (do-delete z model k)]
                 [else (do-replace z model k)]))
      (define doc (~a ((opt-get zipper-focus) (to-root z*))))
      (define okc (equal? doc (render model*)))                       ; content == model
      (define okr (equal? (read-count doc) (length model*)))          ; reparses to N forms
      (printf "~a ~a ~a ~a ~a\n"
              (~a i #:min-width 3) (~a (format "~a @~a" op k) #:min-width 12)
              (~a (length model*) #:min-width 6)
              (~a (string-append (if okc "content " "BADcontent ")
                                 (if okr "read" "BADread")) #:min-width 16)
              (let ([d doc]) (if (> (string-length d) 38) (string-append (substring d 0 38) "...") d)))
      (values z* model* (+ bad (if (and okc okr) 0 1)))))

  (printf "\n~a steps: ~a failures\n" STEPS bad)

  ;; ---- timed long run, batched.  No oracle in the timed path (content/read are O(n)
  ;; and would dominate).  We carry the form-list model (needed to address forms);
  ;; forms/leaves/tallness are read at batch boundaries (untimed).
  ;; NOTE: this addressing re-measures sexp-smr over substrings to read the spine, so
  ;; each op carries an O(n) addressing cost on top of the rope edit -- at these small
  ;; form counts it's dwarfed by the (contracted) rope ops, but it is not O(log n). ----
  (define (timed-step z model)
    (define len (length model))
    (define op (cond [(< len 4) 'insert] [(> len 14) 'delete]
                     [else (case (random 3) [(0) 'insert] [(1) 'delete] [else 'replace])]))
    (define k (case op [(insert) (random (add1 len))] [else (random len)]))
    (case op [(insert) (do-insert z model k)] [(delete) (do-delete z model k)] [else (do-replace z model k)]))

  (define BATCH 300) (define BATCHES 20)
  (printf "\ntimed long run: ~a batches x ~a ops = ~a ops (form edits, no oracle)\n"
          BATCHES BATCH (* BATCHES BATCH))
  (printf "~a ~a ~a ~a ~a\n"
          (~a "batch" #:min-width 6) (~a "forms" #:min-width 6) (~a "leaves" #:min-width 7)
          (~a "tall" #:min-width 6) (~a "us/op" #:min-width 8))

  (define usops
    (let* ([t0 (render model0)]
           [zS (start sexp-smr ((make-rope sexp-smr) t0) (sexp-guides (front-at t0 0)))])
      (for/fold ([z zS] [model model0] [ts '()] #:result (reverse ts)) ([b (in-range BATCHES)])
        (define tt0 (current-inexact-milliseconds))
        (define-values (z* model*)
          (for/fold ([z z] [model model]) ([_ (in-range BATCH)]) (timed-step z model)))
        (define usop (* 1000.0 (/ (- (current-inexact-milliseconds) tt0) BATCH)))
        (define root ((opt-get zipper-focus) (to-root z*)))
        (printf "~a ~a ~a ~a ~a\n"
                (~a b #:min-width 6) (~a (length model*) #:min-width 6)
                (~a (rope-leaves* root) #:min-width 7)
                (~a (~r (tallness root) #:precision '(= 2)) #:min-width 6)
                (~a (~r usop #:precision '(= 2)) #:min-width 8))
        (values z* model* (cons usop ts)))))

  (printf "\nper-op over ~a ops: min ~a  median ~a  mean ~a us  (min = cleanest)\n"
          (* BATCHES BATCH)
          (~r (apply min usops) #:precision '(= 2))
          (~r (median usops) #:precision '(= 2))
          (~r (/ (apply + usops) (length usops)) #:precision '(= 2))))

;; ============================================================================
;; Run all three suites when executed directly (not on require, not under raco test).
(module+ main
  (printf "========== edit-band ==========\n")  (edit-band-perf)
  (printf "\n========== scaling ==========\n")   (scaling-perf)
  (printf "\n========== sexp-form ==========\n") (sexp-form-perf))
