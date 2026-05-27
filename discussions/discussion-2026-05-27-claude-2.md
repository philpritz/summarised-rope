# Discussion — 2026-05-27 (continued) — with Claude

Short follow-up to `discussion-2026-05-27-claude.md`. The earlier session
left the editor algebra and the guide model open. This one settled how
guides are shaped and how relative motion gets expressed.

## Guides as callable structs

A guide is a struct with `prop:procedure`, carrying everything needed to
decide *and* to manipulate its target:

```racket
(struct guide (decide selector read index)
  #:property prop:procedure
  (lambda (self l r)
    ((guide-decide self) ((guide-selector self) l)
                         ((guide-selector self) r)
                         (guide-index self))))
```

- `decide`   : `(slice-l, slice-r, index) -> -1 | 0 | 1`
- `selector` : `full-summary -> slice` (pulls relevant component out of a
              bundle summary)
- `read`     : `(slice-l, slice-r) -> index` (used to compute the current
              index from cursor summaries)
- `index`    : the target address

`(decide, selector, read)` together form the "kind"; the `index` varies
per instance. No separate kind struct — just partial application:

```racket
(define before-sexp-kind (curry guide sexp-decide sexp-of sexp-read))
(define g (before-sexp-kind '(1 0)))
```

## Index transformations are the diff algebra

The previous discussion floated a separate `delta` type with `+/-`. We
dropped that. Index transformations are just functions on the address
space:

```
parent          = cdr
descend k       = (curry cons k)
two up          = (compose cdr cdr)
```

`next-sibling` and friends need a little arithmetic on the tip
(`(λ (a) (cons (add1 (car a)) (cdr a)))`) so they sit outside the strict
cons/cdr fragment, but still inside the index space. No `delta` type, no
inversion, no transport — composition is plain function composition.

## Updating guides

We pulled in the `struct-update` package so guide updates aren't
written via `struct-copy` boilerplate:

```racket
(define-struct-updaters guide [index])

(navigate z (update-guide-index g cdr))
(navigate z (update-guide-index g (curry cons 0)))
```

Caveat: `update-guide-index` transforms the guide's *stored* index, not
the cursor's current index — so it tracks the cursor only when the two
are already in sync. A true read-current-then-transform helper is the
job of a small `relative` we sketched but didn't commit.

## API convention: curry only the factories

Tightened a small rule. Operations that pair a zipper with a guide are
flat (`(navigate z g)`, `(open-split-left z g)`). `sys`-factories stay
curried because their stage-1 value is genuinely useful as a binding:
`(concat-rope sys)`, `(start sys)`, `(split-rope sys guide [select])`.
The recursive-descent closure benefit we recall lives in the internal
`let walk` / `let search` loops, not in the outer API shape.

## Added to `zipper-core-draft.rkt`

- `(struct guide …)` + `prop:procedure`
- `update-guide-index` via `define-struct-updaters`
- `navigate` (rise-then-search; not central but written down for
  future sessions)

Still in the draft, not promoted into `zipper-core.rkt`.

## Things still open

- `relative` helper — read index from cursor, transform, navigate.
- Whether the index lives best as tip-first or root-first lists once
  more transforms are in.
- Seg-guide variant of the same struct shape (decide returns `-2..2`).
- Promoting the draft.
