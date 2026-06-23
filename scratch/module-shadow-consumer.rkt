#lang racket
;; Consumes the probe two ways: the default surface, and the `simple` submodule.
(require rackunit
         "module-shadow.rkt"                                  ; default surface
         (prefix-in s: (submod "module-shadow.rkt" simple)))  ; the simple surface

(check-equal? (foo) 'fast-foo   "plain require -> FAST foo")
(check-equal? (bar) 'simple-bar "plain require -> simple bar (no fast variant)")
(check-equal? (s:foo) 'simple-foo "simple submodule -> simple foo")
(printf "OK  default: foo=~a bar=~a   |   simple: foo=~a\n" (foo) (bar) (s:foo))
