#lang racket

;; PATTERN PROBE: simple defs are the file's top-level text, but plain `require`
;; gets the FAST versions (from a submodule), re-exported under the canonical names.

;; the SIMPLE algebra -- the readable top-level text
(define (foo) 'simple-foo)
(define (bar) 'simple-bar)             ; no fast variant

;; the FAST implementations, in a plain `module` submodule
(module fast racket/base
  (provide foo)
  (define (foo) 'fast-foo))

;; default export: the fast `foo` (renamed over the prefix), the simple `bar` direct
(require (prefix-in fast: (submod "." fast)))
(provide bar (rename-out [fast:foo foo]))

;; the simple versions, still reachable for reference / conformance
(module+ simple (provide foo bar))
