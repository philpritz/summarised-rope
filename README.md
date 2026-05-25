# Summarised Rope Zipper Core

This project is an experimental Racket core for a persistent summarised rope with
an editor-facing zipper.

```text
rope   = persistent summarised tree storage
zipper = cursor and main interface into the text
```

The current code lives in:

- `zipper-core.rkt`: rope storage, zipper navigation, guide search, cursor views,
  and basic insertion.
- `summary-algebras.rkt`: example summary algebras and guides, including text
  positions and S-expression navigation.
- `deprecated/`: older locator-based work kept for reference only.

## Zipper View

A zipper stores the current gap and the summary context around it:

```racket
(struct zipper (sys left right before-summary after-summary crumbs) ...)
```

Conceptually:

```text
before-summary | left ^ right | after-summary
```

There are two public summary views:

```racket
(gap-summary z k) ; before left right after
(gap-sides z k)   ; left-total right-total
```

`gap-sides` is the guide-facing view:

```text
left-total  = before-summary + left-summary
right-total = right-summary + after-summary
```

## Guides

A guide is a plain function:

```racket
left-total-summary right-total-summary -> -1 | 0 | 1
```

Navigation is built from guides:

```racket
((navigate guide) z)
((search guide) z)
```

`navigate` can shift outward and then search inward. `search` is local to the
current gap neighborhood.

## S-expression Addresses

The S-expression summary supports structural cursor addresses. Addresses are
slot/gap addresses, not node indexes.

```text
^expr          => '(0)
^expr          => '(0 0 0) ; same start boundary by zero padding
(^define ...)  => '(0 1)
(define ^...)  => '(0 2)
```

Guide makers are named with a `-guide` suffix:

```racket
before-sexp-guide : address -> guide
after-sexp-guide  : address -> guide
```

Address transformers are named with an `-address` suffix:

```racket
next-sexp-address     : address -> address
previous-sexp-address : address -> address
parent-sexp-address   : address -> address
```

Relative navigation freezes the current address, transforms it, builds a guide,
and then navigates:

```racket
((relative-sexp after-sexp-guide) z)

((relative-sexp (compose before-sexp-guide next-sexp-address)) z)

((relative-sexp (compose before-sexp-guide parent-sexp-address)) z)
```

## Example

```racket
(require "zipper-core.rkt"
         "summary-algebras.rkt")

(define sys (system sexp-frontier-algebra))
(define source "(define square\n  (lambda (x)\n    (* x x)))\n(+ 1 2)")
(define rope ((string->rope sys) source #:chunk-size 3))
(define z0 ((start sys) rope))

(define before-name
  ((navigate (before-sexp-guide '(0 2))) z0))

before-name
```

The zipper has a custom writer, so evaluating it prints:

```text
cursor-left:  "(define " ^
cursor-right: "square\n  (lambda (x)\n    (* x x)))\n(+ 1 2)"
left-total-summary:  '(#f #t () 0 (1) #f #f) ^
right-total-summary: '(#t #t (2) 1 () #f #t)
```

## Running Tests

```powershell
& "C:\Program Files\Racket\raco.exe" test .\zipper-core.rkt
& "C:\Program Files\Racket\raco.exe" test .\summary-algebras.rkt
& "C:\Program Files\Racket\raco.exe" test .\tests\current-api-test.rkt
```
