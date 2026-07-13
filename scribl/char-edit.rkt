#lang racket

;; A worked example for the manual (Section 2): a char-offset cursor, built as a
;; custom summary system over rope-core + the guide-agnostic zipper-core.  A
;; position is named by a character offset; the whole thing mirrors the sexp
;; system in miniature.  Four atoms define the system -- the summary, the guide,
;; the family split, and the cut's anchors -- and everything else is placement,
;; covering, and navigation written with the opt ops on zipper-guide.

(require "../rope-core.rkt" "../zipper-core.rkt"
         "../toolbox/main.rkt"                              ; reading/writing, enter
         (submod "../toolbox/algebra.rkt" experimental))    ; the curried `lambda`

(provide char-smr            ; the summary
         cursor cover goto   ; placement, the covering re-anchor, navigation
         (all-from-out "../rope-core.rkt")
         (all-from-out "../zipper-core.rkt"))

;; the summary: a position is a char count, so the measure is the length.
;; identity = (string-length "") = 0; combine = +.  A monoid by inspection.
(define char-smr (make-summary string-length +))

;; the index (= guide): data is a head-led list -- here just (offset).  A FRONT
;; offset (>= 0) counts chars from the left, a BACK offset (<= -1) from the right.
;; prop:procedure makes the index callable as a guide: comparing its head against
;; the cut's anchor on its own side (front: L; back: -(R+1)).
(define (cmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))
(define (back? d) (negative? (car d)))
(define (guide-body d L R) (cmp (car d) (if (back? d) (- (add1 R)) L)))
(struct idx (data) #:property prop:procedure
  (lambda (self L R) (guide-body (idx-data self) L R)))

;; the cut's two anchors (front = left-based, back = right-based).  The +1 keeps
;; the families on disjoint number ranges, so a sign tells them apart.
(define (read-cut L R) (values (list L) (list (- (add1 R)))))

;; placement: start a fresh zipper with two index-guides, then navigate to them
;; (one index = a gap).  The guide list rides zipper-core's staged both-edges optic.
(define (cursor rope s [e s])
  (let ([gs (idx s)] [ge (idx e)])
    (((stage-set zipper-guides) (list gs ge)) (start char-smr rope gs ge))))

;; cover: re-anchor the end edge onto its back anchor, so edits between the edges
;; stay wrapped.  Reads both anchors fresh and installs the back one at edge 1.
(define (edge-contexts z i) ((edge-view i) z))
(define (anchors z i) (call-with-values (lambda () (edge-contexts z i)) read-cut))
(define (cover z)
  (define-values (_front back) (anchors z 1))
  ;; the writing consumer keeps the start guide and installs the back anchor at edge 1
  ((compose (writing (lambda ((_Ls _Rs) gs) (list (car gs) (idx back)))) (enter z)) zipper-guides))

;; navigation: reposition the cursor (install fresh guides on the current zipper).
(define (goto s [e s]) (lambda (z) (((stage-set zipper-guides) (list (idx s) (idx e))) z)))
