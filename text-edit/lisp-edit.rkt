#lang racket

;; Lisp editing: the cursor's focus as its labeled lexical RUNS, a layer over
;; zipper-core and the lisp summary, in the STAGED (g*) protocol. The pipeline is a
;; compose-stage chain:
;;   zipper-focus/g the focus rope focal, its flanking summaries (bs as) on the bus
;;                  (zipper-core's own staged optic)
;;   split-runs     a CONFIGURED stage (lambda ((bs as) fr) ...): the head triple
;;                  PLURALIZED -- world fr -> foci frs (run pieces), renders (bss ass)
;;                  (each piece's own flanking summaries) back onto the bus
;;   labeled-run    a row POLICY (ctx = (bs as), world = (fr)) judging ONE piece:
;;                  focus fr, its `label` on the bus
;;   label-runs     (stage-list labeled-run): the policy at every piece
;;   typed-runs     the chain -- (as-stage label-runs) bridges the flat policy in
;;   label          (bs fr as) -> class: judge ONE contiguous run in context (scalar)
;; Pieces stay ROPES through the reads; a put may hand back ropes or strings --
;; (make-rope smr) coerces strings and passes ropes through (structure shared, so
;; untouched pieces are never re-leafed). Labels and flanks ride the render BUS: a
;; reading/update consumer sees them, the put never consumes them.

(require racket/match
         "../rope-core.rkt"
         (submod "../rope-core.rkt" experimental)                ; frame-guide*, multisect*
         "../summaries/lisp-summary.rkt"                         ; lisp-smr, class-sides
         (submod "../summaries/lisp-summary.rkt" experimental)   ; lisp-runs-guide*
         "../zipper-core.rkt"                                    ; the machine; zipper-focus/g, zipper-guides
         "../toolbox/main.rkt"                                   ; the stage ops, stage-list, as-stage, focal/g
         (submod "../toolbox/algebra.rkt" experimental))         ; the curried `lambda` -- (lambda ((bs as) fr) ...)

(provide label labeled-run split-runs label-runs typed-runs
         lisp-runs-guide* multisect*                          ; the splitter, usable bare
         (all-from-out "../zipper-core.rkt")                  ; the staged optics ride through
         (all-from-out "../summaries/lisp-summary.rkt")
         (all-from-out "../rope-core.rkt"))

;; ---------- label: judge one contiguous run in context ----------
;; The scalar head-triple judge: bs/as summaries, fr a rope/string/value (coerced).
;; A focus that is all glue (bare #s) adopts the class of the run it adheres to,
;; read through as; 'hash only survives when the glue runs to the document end.
(define (label bs fr as)
  (define-values (_l c) (class-sides bs (lisp-smr fr)))
  (cond [(not c) #f]                                          ; empty focus
        [(eq? c 'hash)
         (define-values (_l2 c2) (class-sides (lisp-smr bs fr) as))
         (or c2 'hash)]
        [else c]))

;; ---------- split-runs: the head triple, pluralized ----------
;; a CONFIGURED stage: config (bs as) off the bus, world fr. Foci = the piece ROPES
;; frs; renders = each piece's own flanking summaries (bss ass), a prefix and a suffix
;; scan seeded by the real flanks (the guide is framed with them too, so the CUTS are
;; context-true). The put rejoins new pieces through (make-rope smr) -- built with the
;; zipper's smr, since rope-join takes its algebra off the left operand.
(define (split-runs smr)
  (define build (make-rope smr))
  (lambda ((bs as) fr)
    (define bs* (lisp-smr bs))                           ; normalize (bundle -> slot)
    (define as* (lisp-smr as))
    (define frs ((multisect* (frame-guide* lisp-runs-guide* bs* as*)) fr))
    (define bss (for/fold ([b bs*] [acc '()] #:result (reverse acc)) ([p (in-list frs)])
                  (values (lisp-smr b p) (cons b acc))))
    (define ass (for/fold ([a as*] [acc '()] #:result acc) ([p (in-list (reverse frs))])
                  (values (lisp-smr p a) (cons a acc))))
    (values (lambda (c) ((c bss ass) frs))               ; renders (bss ass), focus frs
            (lambda (frs*) (apply build frs*)))))

;; ---------- labeled-run: ONE piece, judged in its context ----------
;; a row POLICY (curried stage) for stage-list: (staged-apply labeled-run ctx wr) with
;; ctx = (bs as), wr = (fr). Focus = the piece fr, its label on the bus; the put consumes
;; a new piece alone.
(define labeled-run
  (lambda* (ctx wr)
    (match-define (list bs as) ctx)
    (define fr (car wr))
    (values (lambda (c) ((c (label bs fr as)) fr))
            (lambda (p) p))))

;; ---------- label-runs: judge each piece in its own context ----------
;; the elementwise lift of labeled-run over the pieces (world) and their flanks (bus):
;; foci = the pieces, renders = the labels. One stage-list, no threading of its own.
(define label-runs (stage-list labeled-run))

;; ---------- the composition ----------
;; smr = the algebra the zipper's rope is built with (defaults to bare lisp-smr).
;; (as-stage label-runs) bridges the flat row policy into the compose-stage chain.
(define (typed-runs [smr lisp-smr])
  (compose-stage zipper-focus/g (split-runs smr) (as-stage label-runs)))

;; ============================================================================
(module+ test
  (require rackunit racket/format
           "../summaries/summaries.rkt")                         ; bundle, char-smr
  (define bsmr (bundle lisp-smr char-smr))
  (define ((char-at n) L R) (let ([c (char-smr L)]) (cond [(< c n) 1] [(> c n) -1] [else 0])))
  (define (cursor rope i j)
    (((stage-set zipper-guides) (list (char-at i) (char-at j)))
     (start bsmr rope (char-at i) (char-at j))))
  (define doc ((make-rope bsmr) "(a \"one two\" b ; c\n(d \"three\"))"))
  (define runs (typed-runs bsmr))

  ;; --- label: the scalar judge ---
  (check-eq? (label (lisp-smr "(a ")    "\"x y\"" (lisp-smr " b)"))   'string)
  (check-eq? (label (lisp-smr "(a \"")  "x y"     (lisp-smr "\" b)")) 'string)  ; contextual
  (check-eq? (label (lisp-smr "(a ")    "; c\n"   (lisp-smr "b)"))    'comment)
  (check-eq? (label (lisp-smr "(f ")    "#\\("    (lisp-smr " x)"))   'charlit)
  (check-eq? (label (lisp-smr "x")      "#"       (lisp-smr "|y|#"))  'block)   ; glue resolves right
  (check-eq? (label (lisp-smr "x")      "#"       (lisp-smr ""))      'hash)    ; glue to doc end
  (check-eq? (label (lisp-smr "x")      ""        (lisp-smr "y"))     #f)       ; empty focus

  ;; --- labeled-run: the one-piece policy (ctx = (bs as), world = (fr)) ---
  (let-values ([(g p) (staged-apply labeled-run (list (lisp-smr "(a ") (lisp-smr "b)")) (list "; c\n"))])
    (check-equal? (g (pure values)) "; c\n")                         ; the piece (focus)
    (check-eq?    (g pure) 'comment)                                 ; its label (on the bus)
    (check-equal? (p "x") "x"))                                      ; put = identity

  ;; --- the indexed focus: reading gives (fr bs as); the put is zipper-focus/g's own ---
  (let ([z (cursor doc 5 26)])
    (match-define (list fr bs as)
      ((compose (reading (lambda ((bs as) fr) (list fr bs as))) (enter z)) zipper-focus/g))
    (check-equal? (~a fr) "ne two\" b ; c\n(d \"thr")
    (check-true  (lisp-in-string? (lisp-smr bs)))                    ; cut sits mid-string
    (check-true  (lisp-in-string? (lisp-smr (lisp-smr bs) (lisp-smr fr)))))
  (let ([z (cursor doc 5 8)])
    (check-equal? (~a ((stage-get zipper-focus/g) (to-root (((stage-set zipper-focus/g) "X") z))))
                  "(a \"oXtwo\" b ; c\n(d \"three\"))"))

  ;; --- typed-runs, context-true: the focus starts INSIDE the string ---
  (let ([z (cursor doc 5 26)])
    (define-values (ps ls)
      ((compose (reading (lambda ((ls) ps) (values ps ls))) (enter z)) runs))  ; renders=labels, foci=pieces
    (check-equal? ls '(string code comment code string))
    (check-equal? (map ~a ps) '("ne two\"" " b " "; c\n" "(d " "\"thr")))

  ;; --- whole document: labels; a type-directed edit with a MIXED put ---
  (let ([z (start bsmr doc (char-at 0) (char-at 0))])
    (define-values (ps ls)
      ((compose (reading (lambda ((ls) ps) (values ps ls))) (enter z)) runs))
    (check-equal? ls '(code string code comment code string code))
    (check-true (andmap rope? ps))                                   ; pieces stay ropes
    (define z* ((compose (writing (lambda ((ls) ps)                  ; ropes back + one string
                                    (for/list ([p ps] [l ls]) (if (eq? l 'comment) "" p))))
                         (enter z)) runs))
    (check-equal? (~a ((stage-get zipper-focus/g) (to-root z*)))
                  "(a \"one two\" b (d \"three\"))"))

  ;; --- the guide list: reading gives (guides Ls Rs), aligned by edge ---
  (let ([z (cursor doc 5 26)])
    (define gs ((stage-get zipper-guides) z))
    (define-values (Ls Rs) ((stage-view zipper-guides) z))
    (check-equal? (length gs) 2)
    (check-equal? (map char-smr Ls) '(5 26))                         ; chars left of each edge
    (check-equal? (map char-smr Rs) '(26 5))                         ; chars right of each edge
    (check-true  (lisp-in-string? (lisp-smr (car Ls))))              ; edge 0: inside "one two"
    (check-true  (lisp-in-string? (lisp-smr (cadr Ls))))             ; edge 1: inside "three"
    (check-eq?   (lisp-state (lisp-smr (lisp-smr (car Ls)) (lisp-smr (car Rs)))) 'code)
    ;; a cut-driven guide rewrite: collapse the cursor onto its own start edge
    (let ([z* ((compose (writing (lambda ((Ls Rs) gs) (list (car gs) (car gs))))
                        (enter z)) zipper-guides)])
      (check-equal? (~a ((stage-get zipper-focus/g) z*)) "")
      (check-equal? (char-smr ((compose (lambda (L R) L) (edge-view 0)) z*)) 5))
    ;; the row lifts ride the bus: (stage-lref i focal/g) = edge i's guide WITH its cut
    (define edge0 (compose-stage zipper-guides (as-stage (stage-lref 0 focal/g))))
    (define-values (L0 R0) ((stage-view edge0) z))
    (check-equal? (char-smr L0) 5)
    (check-equal? (char-smr R0) 26)
    ;; move edge 0 alone (the put writes the focal guide back into the pair)
    (let ([z* (((stage-set edge0) (char-at 7)) z)])
      (check-equal? (~a ((stage-get zipper-focus/g) z*)) " two\" b ; c\n(d \"thr"))
    ;; the diagonal collapses the cursor onto edge 1, cut context on the bus
    (let ([z* (((stage-set (compose-stage zipper-guides (as-stage (stage-ldiag 1 focal/g)))) (char-at 12)) z)])
      (check-equal? (~a ((stage-get zipper-focus/g) z*)) "")
      (check-equal? (char-smr ((compose (lambda (L R) L) (edge-view 0)) z*)) 12)))

  ;; --- the middle stage alone: pieces with their per-piece contexts ---
  (let ([z (cursor doc 0 (char-smr doc))])
    (define-values (frs bss ass)
      ((compose (reading (lambda ((bss ass) frs) (values frs bss ass))) (enter z))
       (compose-stage zipper-focus/g (split-runs bsmr))))
    (check-equal? (length frs) 7)
    (check-eq? (lisp-state (list-ref bss 1)) 'code)                  ; before "one two"
    (check-eq? (label (list-ref bss 3) (list-ref frs 3) (list-ref ass 3)) 'comment)))
