#lang racket
(require "text-edit/sexp-edit.rkt")

(define (doc z) (~a ((opt-get zipper-focus) (to-root z))))
(define (foc z) (~a ((opt-get zipper-focus) z)))

(printf "=== chained sequence on one cursor ===\n")
(define rope ((make-rope sexp-smr) "(aa bb cc)"))

;; 1. place a gap before bb  (slot 1 of the frame)
(define z0 (cursor rope '(1 0)))
(printf "1. gap before bb         focus=~s\n" (foc z0))

;; 2. insert "xx " into the gap  (the edit = setter zipper-focus)
(define z1 (((opt-set zipper-focus) "xx ") z0))
(printf "2. insert \"xx \"          doc=~s  focus=~s\n" (doc z1) (foc z1))

;; 3. aim one slot right, then insert  (aim = updater zipper-guide, then act)
(define z2 (((opt-set zipper-focus) "yy ")
            ((opt-update zipper-guide (move (slot add1))) z1)))
(printf "3. move +1 slot, insert  doc=~s  focus=~s\n" (doc z2) (foc z2))

(printf "\n=== fresh cursors (each from the original rope) ===\n")
;; open a seg [bb, cc) by spreading the gap's end one slot right
(define s0 (cursor rope '(1 0)))
(define s1 ((opt-update zipper-guide (spread values (slot add1))) s0))
(printf "seg [bb,cc)              focus=~s\n" (foc s1))
(printf "  replace \"BB \"          doc=~s\n" (doc (((opt-set zipper-focus) "BB ") s1)))
(printf "  delete (replace \"\")    doc=~s\n" (doc (((opt-set zipper-focus) "") s1)))

;; cover: keep wrapping the focus across a growing edit
(define c0 (cover (cursor rope '(1 0) '(2 0))))
(define c1 (((opt-set zipper-focus) "b1 (b2 b3) ") c0))
(printf "cover then replace       doc=~s  focus=~s\n" (doc c1) (foc c1))
