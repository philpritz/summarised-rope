# Discussion — 2026-05-27 (5) — with ChatGPT

Compact notes from a continuation session around the promoted zipper v2 model,
programmer-facing S-expression navigation, pretty printing, and the next summary
porting step.

## Collaboration convention added

Added a project discussion convention:

- Keep project discussions tight and user-led.
- Let the user propose ideas first.
- Do not go off designing or thinking ahead independently.
- Avoid fluff.
- Keep responses compact unless more detail is requested.

This was added to `discussions/conventions.md`.

## Navigation API correction

The older README-facing API suggested navigation like:

```racket
((navigate (before-sexp-guide address)) z)
```

but the latest direction is the draft-v2 interface:

```racket
(move/update-index z update-index)
```

The active guide is part of zipper/editor cursor state:

```racket
(struct zipper (sys head before-summary after-summary crumbs guide))
```

The public movement operation updates the installed guide's index, rebuilds the
appropriate guide through its factory, navigates, and then stores that exact
new guide in the resulting zipper.

Absolute movement shape:

```racket
(move/update-index z (const target-index))
```

Relative movement shape:

```racket
(move/update-index z relative-transform)
```

## S-expression index sketch

The current rough S-expression index shape remains:

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
(sexp-index '() '(2 3)) ; select top-level children 2, 3, 4
```

The intended programmer interface is therefore index-oriented rather than
`before-sexp-guide` / `after-sexp-guide` oriented.

## Pretty printer direction

A custom writer was added to the zipper so interactive values show the cursor or
selection state rather than the raw transparent struct.

Current attached shape:

```racket
(struct zipper (sys head before-summary after-summary crumbs guide)
  #:transparent
  #:property prop:custom-write
  (lambda (z out _mode)
    (print-zipper z out)))
```

For a gap, the desired display is boundary-oriented:

```text
zipper: gap
guide: gap-guide
index: ...

cursor-left:  ... ^
cursor-right: ^ ...

left-total-summary:  ...
right-total-summary: ...

crumbs: ...
```

For a segment, the desired display is selection-oriented:

```text
zipper: seg
guide: seg-guide
index: ...

segment-left:   ...
segment-middle: ...
segment-right:  ...

left-total-summary:  ...
middle-summary:      ...
right-total-summary: ...

crumbs: ...
```

Important correction: the committed printer currently previews the local head
ropes. That is useful but not the final desired display. The desired display is
the whole-document text split around the cursor or selected segment.

## Parked: document-view primitives for pretty printing

Do not use plain `root` for gap document views. Rising to root with `root` does
not necessarily preserve the logical cursor boundary.

The correct future primitive should reconstruct document-side ropes by walking
crumbs outward while preserving the current gap or segment boundary.

Likely primitives:

```racket
gap-document-ropes    : zipper -> values rope rope
gap-document-strings  : zipper -> values string string
seg-document-ropes    : zipper -> values rope rope rope
seg-document-strings  : zipper -> values string string string
```

Rough intended gap reconstruction:

```racket
(define (gap-document-ropes z)
  (match-define (zipper sys (gap left right) before after crumbs guide) z)
  (let loop ([left left]
             [right right]
             [crumbs crumbs])
    (match crumbs
      ['() (values left right)]
      [(cons (opened-left _ sibling _) rest)
       (loop left ((concat-rope sys) right sibling) rest)]
      [(cons (opened-right _ sibling _) rest)
       (loop ((concat-rope sys) sibling left) right rest)])))
```

This was not added yet.

## Editing/index concern — strong parked issue

This is a major concern and should be revisited carefully.

A `seg` index such as:

```racket
(sexp-index path '(start count))
```

denotes an interval in the old document. After mutation, that guide/index may be
stale.

Cases:

- Replacing one sexp with one sexp can probably keep the selected interval.
- Deleting a selected sexp destroys the selected interval.
- Replacing `N` sexps with `M` sexps makes the old count wrong when `M != N`.

Likely rule:

> Editing operations must either preserve the selected interval by construction,
> or explicitly choose a new guide/index. Never silently keep a stale selection
> index.

Possible future shape:

```racket
delete/left
delete/right
replace/select
replace/left
replace/right
```

or a lower-level policy-bearing edit primitive:

```racket
edit/with-index
```

No decision was made.

## Promotion of v2 zipper core

The draft-v2 zipper model was promoted:

- `zipper-core-draft-v2.rkt` was copied into `zipper-core.rkt`.
- `zipper-core-draft-v2.rkt` was then removed.

Commits from this session:

- `3358e2b316b467155f5a8de6b017123a181a26d2` — promote draft v2 to zipper core.
- `72a3e7fc2300d62485797796e58c37a7cdff3589` — remove promoted draft v2 file.

## Next straightforward work: port summaries

The next concrete task is to port the summary / S-expression guide machinery into
the current model.

The summary module should likely provide a guide factory that builds either a
gap guide or segment guide based on the index slot:

```racket
make-sexp-guide : sexp-index -> gap-guide | seg-guide
```

Rough rule:

```racket
(sexp-index path '(start 0)) ; make gap-guide
(sexp-index path '(start n)) ; make seg-guide when n > 0
```

This should eventually supply:

- `sexp-gap-decide`
- `sexp-seg-decide`
- `sexp-read-index`
- any selector needed to project S-expression summary data from full summaries

The goal is to exercise `move/update-index` end to end with tests.

## Status

Complete:

- Added collaboration style convention.
- Added a first custom zipper printer.
- Promoted draft v2 to `zipper-core.rkt`.
- Removed the draft copy.

Not complete:

- Printer does not yet reconstruct whole-document text around nested gaps.
- S-expression summary/index logic is not yet ported.
- Edit operations do not yet have a safe post-edit guide/index policy.

Ready to promote:

- The v2 core file is now promoted as `zipper-core.rkt`.

Still draft:

- Pretty-printer document views.
- S-expression guide factory and summary port.
- Edit/index policy after delete and replacement.
