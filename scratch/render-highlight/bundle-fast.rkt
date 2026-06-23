#lang racket
;; Most-efficient bundle representation, A/B'd against the current hasheq bundle.
;; DROP-IN: same external API as summaries.rkt's `bundle` --
;;   - constructor (a macro capturing component names for printing),
;;   - values implement gen:summary-part, read by component-smr identity: (c v),
;;   - same custom-write, same combine/leaf results.
;; Nothing here touches summaries.rkt or rope-core; it's a scratch A/B.
;;
;; The current bundle's combine pays, per call:  a for/hasheq BUILD (~80ns) + 8
;; extraction probes (each part->summary = hash-has-key? THEN hash-ref) + the outer
;; make-summary `coerce`, which calls part->summary on each bundle-val operand only to
;; get it back (a wasted probe). We attack all three:
;;   shared schema   -- order + index + names live in ONE object every value points at.
;;   vector slots    -- combine is build-vector reading slots positionally (no build,
;;                      no extraction): the component smr sees a RAW slot, not a bundle.
;;   V2: fast binary -- the hot combine path is bvv x bvv. V2 fast-paths it with a
;;                      `bvv?` check (~1ns) instead of make-summary's coerce -> wasted
;;                      part->summary probe; cold paths (leaf, rope coercion at build,
;;                      identity) delegate to a make-summary `base`, so it's still a
;;                      faithful smr without needing rope-core internals.
;; Reads keep ONE hash probe (smr->i); that path is ~1% of a line (measured), so it stays.
;;
;; Run:  racket scratch/render-highlight/bundle-fast.rkt

(require racket/unsafe/ops
         "../../rope-core.rkt"               ; make-summary make-rope gen:summary-part
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (the V0 baseline)
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         "highlight.rkt"                     ; make-kw-smr
         "renderer.rkt")                     ; hl open-doc render line-head

;; exposed for profiling (profile-v2.rkt); the API is the macros / make-* below
(provide make-vbundle make-fbundle make-fbundle/unsafe (struct-out bvv))

;; ============================================================================
;; shared, per-bundle, built once
(struct schema (comps    ; (vectorof smr)        component order: combine + leaf walk it
                index    ; #hasheq(smr -> i)     reads
                names))  ; #hasheq(smr -> sym)   display only

(define (bvv-write v port)
  (define sch (bvv-schema v)) (define slots (bvv-slots v)) (define names (schema-names sch))
  (define entries
    (sort (for/list ([c (in-vector (schema-comps sch))] [i (in-naturals)])
            (cons (hash-ref names c (lambda () (object-name c))) (vector-ref slots i)))
          symbol<? #:key car))
  (define w (apply max 0 (map (lambda (e) (string-length (symbol->string (car e)))) entries)))
  (fprintf port "(bundle")
  (for ([e (in-list entries)]) (fprintf port "\n  [~a ~v]" (~a (car e) #:min-width w) (cdr e)))
  (fprintf port ")"))

;; the value: a slot vector + a pointer to the shared schema
(struct bvv (slots schema) #:transparent
  #:property prop:custom-write (lambda (v port mode) (bvv-write v port))
  #:methods gen:summary-part
  [(define (part->summary v smr)                 ; read: ONE probe; self-return if not a component
     (define i (hash-ref (schema-index (bvv-schema v)) smr #f))
     (if i (vector-ref (bvv-slots v) i) v))])

(define (build-schema named)
  (schema (list->vector (map car named))
          (for/hasheq ([p (in-list named)] [i (in-naturals)]) (values (car p) i))
          (for/hasheq ([p (in-list named)] #:when (cdr p)) (values (car p) (cdr p)))))

;; a make-summary-built smr over a given schema -- handles every arity/coercion
;; faithfully (leaf, rope, identity, variadic). Combine reads slots positionally.
(define (base-smr sch)
  (define comps (schema-comps sch)) (define n (vector-length comps))
  (make-summary
   (lambda (str) (bvv (build-vector n (lambda (i) ((vector-ref comps i) str))) sch))
   (lambda (a b) (bvv (build-vector n (lambda (i) ((vector-ref comps i)
                                                   (vector-ref (bvv-slots a) i)
                                                   (vector-ref (bvv-slots b) i)))) sch))))

;; ---------- V1: vector slots, built via make-summary (safe) ----------
(define (make-vbundle named) (base-smr (build-schema named)))

;; ---------- V2: V1 + fast bvv x bvv binary (skips make-summary's outer coerce) ----------
(define (make-fbundle named)
  (define sch (build-schema named))
  (define comps (schema-comps sch)) (define n (vector-length comps))
  (define base (base-smr sch))
  (define (comb a b) (bvv (build-vector n (lambda (i) ((vector-ref comps i)
                                                       (vector-ref (bvv-slots a) i)
                                                       (vector-ref (bvv-slots b) i)))) sch))
  (case-lambda
    [(a b) (if (and (bvv? a) (bvv? b)) (comb a b) (base a b))]   ; hot path: no coerce
    [args  (apply base args)]))                                  ; cold: faithful make-summary

;; ---------- V2u: V2 + unsafe vector ops (the ceiling; indices are 0..n-1, controlled) ----------
(define (make-fbundle/unsafe named)
  (define sch (build-schema named))
  (define comps (schema-comps sch)) (define n (vector-length comps))
  (define base (base-smr sch))
  (define (comb a b)
    (define sa (bvv-slots a)) (define sb (bvv-slots b))
    (bvv (build-vector n (lambda (i) ((unsafe-vector*-ref comps i)
                                      (unsafe-vector*-ref sa i) (unsafe-vector*-ref sb i)))) sch))
  (case-lambda
    [(a b) (if (and (bvv? a) (bvv? b)) (comb a b) (base a b))]
    [args  (apply base args)]))

;; ---------- API-matching macros (mirror summaries.rkt's `bundle`) ----------
(define-syntax (mk-macro stx)
  (syntax-case stx () [(_ name maker)
    #'(define-syntax (name s)
        (syntax-case s ()
          [(_ c (... ...))
           (with-syntax ([(named (... ...))
                          (map (lambda (cc) (if (identifier? cc) #`(cons #,cc '#,cc) #`(cons #,cc #f)))
                               (syntax->list #'(c (... ...))))])
             #'(maker (list named (... ...))))]))]))
(mk-macro vbundle make-vbundle)
(mk-macro fbundle make-fbundle)

;; ============================================================================
;; the four bundles over the same 4 components
(define kws    '("define" "lambda" "let" "if" "cond"))
(define kw-smr (make-kw-smr kws))
(define comps  (list char-smr kw-smr strsexp-smr linecol-smr))
(define named  (map cons comps '(char-smr kw-smr strsexp-smr linecol-smr)))
(define hbuf   (bundle char-smr kw-smr strsexp-smr linecol-smr))   ; V0 (hash)
(define v1buf  (make-vbundle named))
(define v2buf  (make-fbundle named))
(define v2ubuf (make-fbundle/unsafe named))

;; ============================================================================
(module+ test
  (require rackunit)
  (define s1 "(define (fact n) (if (zero")
  (define s2 "? n) 1 (* n (fact (sub1 n)))))")
  (define (reads-agree label a b)
    (for ([c (in-list comps)]) (check-equal? (c a) (c b) (format "~a slot ~a" label (object-name c)))))
  (for ([buf (list v1buf v2buf v2ubuf)] [nm '(v1 v2 v2u)])
    (reads-agree (format "~a leaf" nm) (buf s1) (hbuf s1))
    (reads-agree (format "~a combine" nm) (buf (buf s1) (buf s2)) (hbuf (hbuf s1) (hbuf s2)))
    (reads-agree (format "~a id-left" nm)  (buf (buf) (buf s1)) (hbuf s1))
    (reads-agree (format "~a id-right" nm) (buf (buf s1) (buf)) (hbuf s1))
    (let ([r  ((make-rope buf)  "(define x\ny)")]
          [hr ((make-rope hbuf) "(define x\ny)")])
      (reads-agree (format "~a rope" nm) r hr))))

;; ============================================================================
(module+ main
  (require rackunit (submod ".." test))
  (printf "correctness: V1 == V2 == V2u == hash (reads agree)  ok\n\n")

  (define (ns label iters thunk)
    (thunk) (collect-garbage)
    (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "  ~a ~a ns/op\n" (~a label #:min-width 30)
            (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 9 #:align 'right)))

  (define text "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))")
  (define mid  (quotient (string-length text) 2))
  (define (combine-of buf) (cons (buf (substring text 0 mid)) (buf (substring text mid))))
  (define N 1000000)

  (printf "combine (buf a b)  [a,b pre-built summaries, as combine-info hands them]:\n")
  (for ([buf (list hbuf v1buf v2buf v2ubuf)]
        [nm '("hash (V0)" "vector+make-summary (V1)" "vector fast-binary (V2)" "V2 + unsafe (ceiling)")])
    (match-define (cons a b) (combine-of buf))
    (ns nm N (lambda () (buf a b))))

  (printf "\nleaf (buf str):\n")
  (for ([buf (list hbuf v1buf v2buf v2ubuf)] [nm '("hash" "V1" "V2" "V2u")])
    (ns nm 300000 (lambda () (buf text))))

  (printf "\nread one slot (linecol-smr (buf a b)):\n")
  (for ([buf (list hbuf v1buf v2buf v2ubuf)] [nm '("hash" "V1" "V2" "V2u")])
    (match-define (cons a b) (combine-of buf))
    (define v (buf a b))
    (ns nm N (lambda () (linecol-smr v))))

  ;; ---------- does the combine win move the RENDER number? same renderer, swapped bundle ----------
  (define snip "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")
  (define big  (apply string-append (make-list 800 snip)))    ; 4000 lines
  (define hs (for/list ([buf (list hbuf v1buf v2buf v2ubuf)]) (hl kws kw-smr buf)))
  (define zs (for/list ([h (in-list hs)]) (open-doc h big)))
  (define ref (render (first hs) (first zs) 0 60))
  (for ([h (in-list (rest hs))] [z (in-list (rest zs))])
    (unless (equal? ref (render h z 0 60)) (error "render disagreement")))
  (printf "\nrender correctness: all bundles render identically  ok\n")

  (printf "\nline-head (nav+head, mid-doc):\n")
  (for ([z (in-list zs)] [nm '("hash (V0)" "V1" "V2" "V2u")])
    (ns nm 3000 (lambda () (line-head z 2000))))

  (printf "\nrender 1 line (mid-doc):\n")
  (for ([h (in-list hs)] [z (in-list zs)] [nm '("hash (V0)" "V1" "V2" "V2u")])
    (ns nm 3000 (lambda () (render h z 2000 1)))))
