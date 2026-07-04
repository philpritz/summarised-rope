#lang racket

;; Sexp focus splitting: split the cursor's focus into s-expression pieces at a chosen depth
;; -- 0 = top-level forms, +inf.0 = all the way to atoms, integers between = bounded depth.
;; The cut decision is purely SPINE-BASED: sand-spines gives a seam an INTEGER head slot at a
;; clean token boundary (start/end) and a half-integer mid-atom/whitespace, and the front
;; spine's LENGTH is the cut's depth + 1. So it reads both off the exported nav interface --
;; no summary internals.

(require "../rope-core.rkt"
         (submod "../rope-core.rkt" experimental)        ; make-guide*, multisect*
         "../summaries/sexp-summary.rkt"                  ; sexp-smr, sand-spines
         "../toolbox/main.rkt"                         ; iso, iso->opt, the opt ops
         "../zipper-core.rkt")                            ; zipper-focus

(provide split-guide* split-iso focus-split)

(define build (make-rope sexp-smr))

(define (split-guide* maxd)
  ;; a clean token boundary <=> sand-spines hands the seam an INTEGER head slot (start/end);
  ;; mid-atom and whitespace-lean get head - 1/2. front spine length - 1 is the cut's depth.
  (define (cut? bs fsl fsr as)
    (let-values ([(front back) (sand-spines (sexp-smr bs fsl) (sexp-smr fsr as))])
      (and (integer? (car front))               ; a real boundary, not mid-atom / whitespace
           (<= (sub1 (length front)) maxd))))    ; depth within maxd
  (make-guide* sexp-smr
    (lambda (bs fsl fsr as) (values #t (cut? bs fsl fsr as) #t))))

;; rope <-> list of sexp pieces at depth maxd. (split-iso 0) = top-level forms ; +inf.0 = atoms.
(define (split-iso maxd) (iso (multisect* (split-guide* maxd)) (curry apply build)))

;; the cursor's focus, split into its sexp pieces at depth maxd.
(define (focus-split maxd) (compose-opt zipper-focus (iso->opt (split-iso maxd))))

;; ============================================================================
;; SCRATCH -- list-sexp <-> string editing. read/print is lossy (whitespace, comments,
;; [ ]{} -> ( ) all normalize), so it breaks GetPut and is NOT a lens; kept as a plain
;; TRANSFORMATION lifted through the lawful zipper-focus. Exploratory, may move/change.
;; ============================================================================
(provide read-all edit-sexp modify-focus)

(define (read-all s)                              ; string -> list of datums (the parse half)
  (let ([p (open-input-string s)])
    (let loop ([acc '()]) (define d (read p))
      (if (eof-object? d) (reverse acc) (loop (cons d acc))))))

;; a focus rewrite: read the focus as data, apply f, print back. f : (listof datum) -> (listof datum).
(define ((edit-sexp f) r)
  (build (string-join (map (lambda (d) (format "~s" d)) (f (read-all (~a r)))) " ")))

(define (modify-focus f) (opt-update zipper-focus (edit-sexp f)))
