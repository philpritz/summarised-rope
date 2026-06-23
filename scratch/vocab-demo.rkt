#lang racket
(require "../sexp-edit.rkt")

(define (doc z)  (~a ((viewer zipper-focus) (to-root z))))   ; whole document
(define (foc z)  (~a ((viewer zipper-focus) z)))             ; the focus (cursor span)
(define rope ((make-rope sexp-smr) "(aa bb cc)"))

;; --- at: jump to an absolute gap (the end slot), then append a child ---
(let ([z (cursor rope '(1 0))])                              ; gap before bb
  (printf "at    : ~a\n" (doc ((setter zipper-focus " dd")
                               ((updater zipper-guide (at '(3 0))) z)))))

;; --- move: gap before bb -> advance one slot -> insert before cc ---
(let ([z (cursor rope '(1 0))])
  (printf "move  : ~a\n" (doc ((setter zipper-focus "xx ")
                               ((updater zipper-guide (move (slot add1))) z)))))

;; --- each: grow the gap into a seg selecting bb, then replace it ---
(let* ([z  (cursor rope '(1 0))]
       [z* ((updater zipper-guide (each values (slot add1))) z)])
  (printf "each  : select ~s -> ~a\n" (foc z*) (doc ((setter zipper-focus "BB ") z*))))

;; --- both: slide a selection one slot right, then delete it ---
(let* ([z  (cursor rope '(1 0) '(2 0))]                      ; seg "bb "
       [z* ((updater zipper-guide (both (slot add1))) z)])   ; -> seg "cc"
  (printf "both  : ~s -> ~s, delete -> ~a\n"
          (foc z) (foc z*) (doc ((setter zipper-focus "") z*))))
