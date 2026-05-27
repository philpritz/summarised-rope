# Discussion — 2026-05-27 (4) — with ChatGPT

Compact notes from a working session refining the draft zipper/navigation
model. This continues the gap/seg zipper rewrite, but does not replace the
older draft yet. The code sketch was added as `zipper-core-draft-v2.rkt`.

## Main decision

Programmer-facing navigation should be expressed by updating the installed
guide's index, not by directly calling structural open/rise operations.

```racket
(move/update-index z update-index)
```

The updater covers both absolute and relative movement:

```racket
(move/update-index z (const target-index)) ; absolute
(move/update-index z relative-transform)   ; relative
```

The invariant is:

```text
zipper head/gap and installed guide index stay linked
```

Raw guide-index updates are therefore not a public API. A guide index should
not be changed without navigating the zipper to match it.

## Guide in the zipper

The draft now lets `zipper` carry one active guide:

```racket
(struct zipper (sys head before-summary after-summary crumbs guide))
```

This was chosen because otherwise every relative movement has to thread both
`z` and `g`:

```racket
z, g -> z*, g*
```

which means the guide is effectively part of editor-facing cursor state.

This boundary is still provisional. We should revisit whether the guide
belongs directly inside `zipper`, in a wrapper cursor/editor state, or in a
richer navigation-mode abstraction.

## Two guide types

Keep the guide model simple: two guide structs, distinguished by predicates.

```racket
gap-guide? ; guide returns -1 | 0 | 1
seg-guide? ; guide returns -2 | -1 | 0 | 1 | 2
```

`navigate` stays singular and dispatches internally on guide type.

```racket
(navigate z g)
```

`move/update-index` updates the old guide's index, rebuilds the guide via its
factory, then calls `navigate`. Rebuilding matters because an index update may
cross from a gap index to a seg index or back.

## Structural operations

We still like the structural calculus:

```racket
gap->seg
seg->gap
insert
delete
replace = insert ∘ delete
left-bound-gap
right-bound-gap
open-left
open-right
```

But structural open/rise/split operations are internal navigation machinery.
The programmer navigates by changing the guide index.

`left-bound-gap` and `right-bound-gap` stay because they are the primitive
settling operations from a segment to one of its boundaries:

```text
(seg l m r) -> (gap l       (m+r))
(seg l m r) -> (gap (l+m)   r)
```

They may remain low-level, but the capability is still useful for editing and
settling after insert/replace.

## S-expression proof sketch

The S-expression index is intentionally rough for now. It is just a proof of
concept for how one index shape can cover gaps and segments.

```racket
(struct sexp-index (path slot))

;; slot:
;;   '(start 0) = gap before child `start`
;;   '(start n) = segment covering n sexps from `start`
```

Examples:

```racket
(sexp-index '() '(2 0)) ; gap before top-level child 2
(sexp-index '() '(2 1)) ; select top-level child 2
(sexp-index '() '(2 3)) ; select children 2, 3, 4
```

This is not meant to be polished. It is a small proving ground for the
navigator/operation calculus.

## Editing examples

Delete selected sexp:

```racket
(define z-selected
  (move/update-index z0
                     (const (sexp-index '() '(2 1)))))

(define z-deleted
  (delete z-selected))
```

Replace selected sexp:

```racket
(define replacement
  ((string->rope sys) "(lambda (y) (+ y 1))" #:chunk-size 3))

(define z-replaced
  (replace z-selected replacement))
```

where:

```racket
(define (replace z replacement)
  (insert (delete z) replacement))
```

## What draft v2 does not finish

- It does not port the old sexp frontier summary algebra into the new guide
  shape.
- It does not provide final `sexp-gap-decide`, `sexp-seg-decide`, or
  `sexp-read-index` implementations.
- Segment navigation is intentionally conservative in the draft: ascend to the
  root gap, then carve the segment from the whole rope. The lower-level
  `open-seg-*` functions are kept for a later smarter local/rise path.
- The guide-in-zipper API boundary is explicitly not final.

## Current direction

Keep the draft v2 as a separate working artifact. Do not promote it over
`zipper-core.rkt` yet. Next step is to port enough sexp summary/index logic to
exercise `move/update-index` end to end with tests.
