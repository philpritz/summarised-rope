#lang racket
;; THROWAWAY: why a mid-atom split fails on the current monoid.
(require "scope-summary.rkt")

(printf "TOKENIZATION is not split-invariant:\n")
(printf "  leaf \"xy\"           lvl = ~s\n" (sv-lvl (scope-leaf "xy")))
(printf "  leaf\"x\" + leaf\"y\"   lvl = ~s\n" (sv-lvl (scope+ (scope-leaf "x") (scope-leaf "y"))))
(printf "  (one atom \"xy\"  vs  two atoms \"x\",\"y\")\n\n")

(printf "scope consequence -- split the keyword `lambda`:\n")
(printf "  whole \"(lambda (x) \"   in-scope = ~s\n" (sv-in-scope (scope-leaf "(lambda (x) ")))
(printf "  \"(lamb\" + \"da (x) \"    in-scope = ~s\n"
        (sv-in-scope (scope+ (scope-leaf "(lamb") (scope-leaf "da (x) "))))
(printf "  frame kind:  whole \"(lambda\" -> ~s   split \"(lamb\"+\"da\" -> ~s\n"
        (F-kind (car (sv-opens (scope-leaf "(lambda"))))
        (F-kind (car (sv-opens (scope+ (scope-leaf "(lamb") (scope-leaf "da"))))))
