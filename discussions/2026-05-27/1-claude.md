# Discussion — 2026-05-27 — with Claude

Compact notes from a working session redesigning the zipper algebra
around two head types (gap and seg). Earlier transcripts cover decisions
in fuller form; this file captures the durable shape.

## File layout

- `rope-core.rkt` — stable. Rope ops, summary algebra, and rope-level
  split functions.
- `zipper-core.rkt` — the new zipper algebra (in flux).
- `zipper-core-draft.rkt` — working draft of the open / rise families,
  not yet merged into `zipper-core.rkt`.
- `deprecated-2/` — snapshot of the previous zipper API
  (self-contained, includes its own copy of `rope-core.rkt`).

## Heads and crumbs

```
zipper  = (sys, head, before-summary, after-summary, crumbs)
gap     = (left, right)            ; punctual cursor
seg     = (left, middle, right)    ; selection
```

- `before-summary` / `after-summary` belong on the zipper, not the head
  — they're outer context that survives all shape transforms.
- Crumbs uniformly reconstruct a *gap* — there's no seg crumb. To
  ascend from a seg you collapse to a gap first.
- One pair of crumb types: `opened-left`, `opened-right`. Leaf-specific
  variants were dropped — the open functions don't know or care whether
  the descent split a branch or a leaf piece.
- Crumb `prop:procedure` signature: `(self sys subtree) -> 4 values
  (parent-left parent-right parent-before parent-after)`. The caller
  collapses the current head into one subtree first.

Open future: replace the empty `'()` base of the crumb list with a
"root crumb" sentinel for consistency. Three possible semantics
deferred (1-value identity return / 4-value degenerate / sentinel only).

## Shape transforms (the heart of the algebra)

Two primitives that take a user-supplied function:

```
gap->seg  z decompose   ; decompose : (l r)   -> (values l m r)
seg->gap  z combine     ; combine   : (l m r) -> (values left right)
```

Derived:

| Op                | Effect                              |
|-------------------|-------------------------------------|
| `insert`          | `m = fresh content`, l/r unchanged  |
| `left-bound-gap`  | `(l, m⊕r)`, cursor at left edge     |
| `right-bound-gap` | `(l⊕m, r)`, cursor at right edge    |
| `delete`          | drops m: `(l, r)`                   |

No more `insert-left` / `insert-right` / `shift-left` / `shift-right`.
The seg head holds inserted content as a first-class thing; the user
decides which bound-gap to collapse to.

## `up` and the 2→1 combiner

```
up z combine   ; combine : (left, right) -> rope
```

`up` collapses the current gap's two sides into one subtree via the
user-supplied `combine`, then hands the subtree to the top crumb. The
crumb pairs it with the stashed sibling. Combine choice (typically
`concat-rope`) is the caller's — the leaf-rejoin optimization moves
out of the crumb into the caller.

`up` doesn't preserve the gap position — it just reconstructs the
parent gap with the combined subtree on the descent side and the
sibling on the other.

## n-ary concat

`concat-rope` is variadic — `((concat-rope sys) a b c …)`. Two-arg
callers keep working. Used by the shape transforms and by anyone
folding multiple ropes.

## Guides

- **Gap guide**: `(left-total, right-total) -> -1 | 0 | 1`. Where is
  the target relative to the cursor.
- **Seg guide**: `(left-total, right-total) -> -2 .. 2`. Where is the
  *segment* relative to the cursor (cursor-relative convention).

Seg guide values:
- `+2` segment far to the right (cursor in `a`)
- `+1` segment starts at cursor (left edge)
- `0`  cursor inside segment
- `-1` segment ends at cursor (right edge)
- `-2` segment far to the left (cursor in `c`)

Boundary gap guides derive from the seg guide via signum, no sign
flip:

- Left boundary  (drive to seg = +1): `signum(seg - 1)`
- Right boundary (drive to seg = -1): `signum(seg + 1)`

## Rope-level split functions

In `rope-core.rkt`:

```
split-rope          : (sys guide [select]) rope before after        -> (values left right)
split-whole-rope    : (sys guide [select]) rope                     -> (values left right)
seg-split-rope      : (sys seg-guide [select]) rope before after    -> (values l m r)
seg-split-whole-rope: (sys seg-guide [select]) rope                 -> (values l m r)
```

The contextual versions take `before`/`after` summaries so they can be
chained — the seg split is two chained gap splits with the first's
`l` summary threaded into the second's `before`.

## Movement primitives in the zipper (current draft)

Three sets:

- **One-step opens (no guide)**: `open-left`, `open-right`. Descend a
  single structural level: branch → its children, splittable leaf →
  halved, atomic/empty → push across without a crumb.
- **Guide-driven 2-way opens (gap guide)**: `open-split-left`,
  `open-split-right`. Use `split-rope` on the chosen side; land a gap.
- **Guide-driven 3-way opens (seg guide)**: `open-seg-left`,
  `open-seg-right`, `open-straddling`. Use `seg-split-rope`. Land a
  seg. `open-straddling` combines `left⊕right` first and doesn't push
  a crumb (same level, head change only).

Rise family (`rise-from-left`, `rise-from-centre`, `rise-from-right`):
loop `up` with `concat-rope` until the seg guide indicates the segment
is contained in the current local subtree. Hard-coded combiner, no
empty-crumb safety (simpler for now).

## Movement style and relative motion

Sticking with guide-only navigation: the programmer writes a guide and
a single `navigate` does the work.

Relative motion is mediated by a separate **indexing algebra** that
lives alongside the summary algebra. Four ops:

```
indexing-algebra
  read       : (left-total, right-total) -> address
  guide-of   : address -> guide
  +          : address × delta -> address      ; shift
  -          : address × address -> delta      ; diff
```

- The two type slots (`address`, `delta`) carry domain variation.
- For chars: `address = N`, `delta = Z`. Plain arithmetic.
- For sexps: `address = path`, `delta = path-edit-script` (a small DSL
  of `pop`, `push k`, `+k at tip`). Composition is concatenation;
  identity is the empty script.

Hierarchical moves (parent, next sibling, first child) are *constant
deltas* in this scheme: scripted operations that don't depend on the
current address.

Relative move shape:

```
(relative (compose guide-of <delta-transform> read))
```

## Things still open

- **Root crumb semantics** — sentinel, 4-value degenerate, or 1-value
  identity. Deferred.
- **Whether the gap navigate side wants a 5-valued guide** (rise vs
  descend at one level) instead of going through summary probes.
- **Whether `-` between addresses is required of every indexing
  algebra** or domain-optional.
- **Predicate search** ("first vowel forward") doesn't fit the guide
  model. Separate mechanism if/when needed.
- **Convention on `define` vs `let` style** — currently using
  `define`s for readability; not switching to `let-values` for now.
