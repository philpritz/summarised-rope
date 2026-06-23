#lang racket
(require "../helper-algebras.rkt")
(printf "view  ~s  (expect 0)\n" ((viewer vdiag) (vector 0 9)))
(printf "set   ~s  (expect #(7 7))\n" ((setter vdiag 7) (vector 0 9)))
(printf "over  ~s  (expect #(1 1))\n" ((updater vdiag add1) (vector 0 9)))
(printf "comp  ~s  (expect #(5 5))\n" ((setter (compose vdiag (vref 0)) 5) (vector (vector 1 2) (vector 3 4))))
