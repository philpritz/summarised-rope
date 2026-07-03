#lang racket

;; Lisp editing: the cursor's focus as its labeled lexical RUNS, a layer over
;; zipper-core and the lisp summary. The pipeline is three composed opts:
;;   zipper-focus   the INDEXED focus (this file's default, replacing zipper-core's
;;                  in the re-export) -- view (values fr bs as): the focus rope
;;                  flanked by its two summaries; the put IS zipper-core's own, so
;;                  writes are identical and the widening is get-side only
;;   split-runs     (fr bs as) -> (values frs bss ass): the head triple PLURALIZED
;;                  -- run pieces, each with its own flanking summaries
;;   label-runs     (frs bss ass) -> (values frs labels): `label` mapped pointwise
;;   typed-runs     the composition of the three
;;   label          (bs fr as) -> class: judge ONE contiguous run in context
;; Pieces stay ROPES through the views; a put may hand back ropes or strings --
;; (make-rope smr) coerces strings and passes ropes through (structure shared, so
;; untouched pieces are never re-leafed). Labels and flanks are read-only context
;; on the values channel: transforms see them, puts never consume them.

(require racket/match
         "rope-core.rkt"
         (submod "rope-core.rkt" experimental)                ; frame-guide*, multisect*
         "summaries/lisp-summary.rkt"                         ; lisp-smr, class-sides
         (submod "summaries/lisp-summary.rkt" experimental)   ; lisp-runs-guide*
         "zipper-core.rkt"                                    ; the machine; zipper-focus widened below
         "helper-algebras.rkt")                               ; opt, arg

(provide label labeled-run split-runs label-runs typed-runs
         lisp-runs-guide* multisect*                          ; the splitter, usable bare
         (rename-out [zipper-focus* zipper-focus]             ; the indexed focus, as default
                     [zipper-guide* zipper-guide])            ; the indexed guide list, ditto
         (except-out (all-from-out "zipper-core.rkt") zipper-focus zipper-guide)
         (all-from-out "summaries/lisp-summary.rkt")
         (all-from-out "rope-core.rkt"))

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

;; ---------- the indexed focus ----------
;; zipper-focus, widened to view (values fr bs as): the focus rope stays focal, the
;; flanking summaries ride behind as read-only context. The put is zipper-focus's
;; own -- set never sees context -- so every write path is untouched.
(define zipper-focus*
  (opt (lambda (z)
         (match-define (cons bs as) ((on-edges cons (arg 0) (arg 1)) z))
         (values ((opt-get zipper-focus) z) bs as))
       (opt-set zipper-focus)
       (arg 0)))

;; ---------- the indexed guide list ----------
;; zipper-guide, widened to view (values guides Ls Rs): the guide list stays focal;
;; behind it ride two parallel lists giving each edge's cut -- Ls[i] / Rs[i] are the
;; summaries left and right of edge i (what edge-sides i reads, both edges at once,
;; aligned with the guides). The put is zipper-guide's own: new guides re-navigate,
;; the cuts are derived and unwritable. A transform hence maps cuts to guides --
;; sexp-edit's reguide as one opt-update.
(define zipper-guide*
  (opt (lambda (z)
         (match-define (cons e0 e1) ((on-edges cons cons cons) z))
         (values ((opt-get zipper-guide) z)
                 (list (car e0) (car e1))
                 (list (cdr e0) (cdr e1))))
       (opt-set zipper-guide)
       (arg 0)))

;; ---------- split-runs: the head triple, pluralized ----------
;; world (fr bs as) -> view (values frs bss ass): the piece ROPES focal, each piece's
;; own flanking summaries behind (a prefix and a suffix scan, seeded by the real
;; flanks; the guide is framed with them too, so the CUTS are context-true). The put
;; rejoins new pieces through (make-rope smr) -- built with the zipper's smr, since
;; rope-join takes its algebra off the left operand.
(define (split-runs smr)
  (define build (make-rope smr))
  (opt (lambda (fr bs as)
         (define bs* (lisp-smr bs))                           ; normalize (bundle -> slot)
         (define as* (lisp-smr as))
         (define frs ((multisect* (frame-guide* lisp-runs-guide* bs* as*)) fr))
         (values frs
                 (for/fold ([b bs*] [acc '()] #:result (reverse acc)) ([p (in-list frs)])
                   (values (lisp-smr b p) (cons b acc)))
                 (for/fold ([a as*] [acc '()] #:result acc) ([p (in-list (reverse frs))])
                   (values (lisp-smr p a) (cons a acc)))))
       (lambda (frs*) (lambda (fr bs as) (apply build frs*)))
       (arg 0)))

;; ---------- labeled-run: ONE piece, judged in its context ----------
;; world (fr bs as) -> view (values fr class): the piece focal, its label behind.
;; The put consumes a new piece (rope or string) alone; the flanks are read-only.
(define labeled-run
  (opt (lambda (fr bs as) (values fr (label bs fr as)))
       (lambda (p) (lambda (fr bs as) p))
       (lambda (fr _l) fr)))

;; ---------- label-runs: judge each piece in its own context ----------
;; The elementwise lift of labeled-run: world (frs bss ass) -> view (values frs
;; labels), the put consuming a list of new pieces (ropes or strings; split-runs'
;; build coerces). One opt-list, no threading of its own.
(define label-runs (opt-list labeled-run))

;; ---------- the composition ----------
;; smr = the algebra the zipper's rope is built with (defaults to bare lisp-smr).
(define (typed-runs [smr lisp-smr])
  (compose-opt zipper-focus* (split-runs smr) label-runs))

;; ============================================================================
(module+ test
  (require rackunit racket/format
           "summaries/summaries.rkt")                         ; bundle, char-smr
  (define bsmr (bundle lisp-smr char-smr))
  (define ((char-at n) L R) (let ([c (char-smr L)]) (cond [(< c n) 1] [(> c n) -1] [else 0])))
  (define (cursor rope i j)
    (((opt-set zipper-guide) (list (char-at i) (char-at j)))
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

  ;; --- labeled-run: the one-piece stage, scalar ---
  (check-equal? (call-with-values
                  (lambda () ((opt-get labeled-run) "; c\n" (lisp-smr "(a ") (lisp-smr "b)")))
                  list)
                (list "; c\n" 'comment))
  (check-equal? (((opt-set labeled-run) "x") "; c\n" (lisp-smr "(a ") (lisp-smr "b)")) "x")

  ;; --- the indexed focus: three values out, the ordinary put back in ---
  (let ([z (cursor doc 5 26)])
    (define-values (fr bs as) ((opt-get zipper-focus*) z))
    (check-equal? (~a fr) "ne two\" b ; c\n(d \"thr")
    (check-true  (lisp-in-string? (lisp-smr bs)))                     ; cut sits mid-string
    (check-true  (lisp-in-string? (lisp-smr (lisp-smr bs) (lisp-smr fr)))))
  (let ([z (cursor doc 5 8)])                                         ; the put: zipper-focus's own
    (check-equal? (~a ((opt-get zipper-focus) (to-root (((opt-set zipper-focus*) "X") z))))
                  "(a \"oXtwo\" b ; c\n(d \"three\"))"))

  ;; --- typed-runs, context-true: the focus starts INSIDE the string ---
  (let ([z (cursor doc 5 26)])
    (define-values (ps ls) ((opt-get runs) z))
    (check-equal? ls '(string code comment code string))
    (check-equal? (map ~a ps) '("ne two\"" " b " "; c\n" "(d " "\"thr")))

  ;; --- whole document: labels; a type-directed edit with a MIXED put ---
  ;; un-navigated start: the focus is the whole rope, and the gap-at-0 guides stay
  ;; valid when the edit SHRINKS the document (a guide past the new end cannot
  ;; re-navigate -- the scratch demos dodged that by luck)
  (let ([z (start bsmr doc (char-at 0) (char-at 0))])
    (define-values (ps ls) ((opt-get runs) z))
    (check-equal? ls '(code string code comment code string code))
    (check-true (andmap rope? ps))                                    ; pieces stay ropes
    (define z* ((opt-update runs
                  (lambda (ps ls)                                     ; ropes back + one string
                    (for/list ([p ps] [l ls]) (if (eq? l 'comment) "" p))))
                z))
    (check-equal? (~a ((opt-get zipper-focus) (to-root z*)))
                  "(a \"one two\" b (d \"three\"))"))

  ;; --- the indexed guide list: (values guides Ls Rs), aligned by edge ---
  (let ([z (cursor doc 5 26)])
    (define-values (gs Ls Rs) ((opt-get zipper-guide*) z))
    (check-equal? (length gs) 2)
    (check-equal? (map char-smr Ls) '(5 26))                          ; chars left of each edge
    (check-equal? (map char-smr Rs) '(26 5))                          ; chars right of each edge
    (check-true  (lisp-in-string? (lisp-smr (car Ls))))               ; edge 0: inside "one two"
    (check-true  (lisp-in-string? (lisp-smr (cadr Ls))))              ; edge 1: inside "three"
    (check-eq?   (lisp-state (lisp-smr (lisp-smr (car Ls)) (lisp-smr (car Rs)) )) 'code)
    ;; a cut-driven guide rewrite: collapse the cursor onto its own start edge
    (let ([z* ((opt-update zipper-guide*
                 (lambda (gs Ls Rs) (list (car gs) (car gs)))) z)])
      (check-equal? (~a ((opt-get zipper-focus) z*)) "")
      (check-equal? (char-smr ((on-edges (lambda (l r) l) (arg 0) (arg 0)) z*)) 5))
    ;; the row lifts ride the parallel lists: (opt-lref i focal) = edge i's guide
    ;; WITH its cut in view, the guide alone writable
    (define edge0 (compose-opt zipper-guide* (opt-lref 0 focal)))
    (define-values (_g L0 R0) ((opt-get edge0) z))
    (check-equal? (char-smr L0) 5)
    (check-equal? (char-smr R0) 26)
    ;; move edge 0 alone (the put writes the focal guide back into the list)
    (let ([z* (((opt-set edge0) (char-at 7)) z)])
      (check-equal? (~a ((opt-get zipper-focus) z*)) " two\" b ; c\n(d \"thr"))
    ;; the diagonal collapses the cursor onto edge 1, cut context in view
    (let ([z* (((opt-set (compose-opt zipper-guide* (opt-ldiag 1 focal))) (char-at 12)) z)])
      (check-equal? (~a ((opt-get zipper-focus) z*)) "")
      (check-equal? (char-smr ((on-edges (lambda (l r) l) (arg 0) (arg 0)) z*)) 12)))

  ;; --- the middle stage alone: pieces with their per-piece contexts ---
  (let ([z (cursor doc 0 (char-smr doc))])
    (define-values (frs bss ass) ((opt-get (compose-opt zipper-focus* (split-runs bsmr))) z))
    (check-equal? (length frs) 7)
    (check-eq? (lisp-state (list-ref bss 1)) 'code)                   ; before "one two"
    (check-eq? (label (list-ref bss 3) (list-ref frs 3) (list-ref ass 3)) 'comment)))
