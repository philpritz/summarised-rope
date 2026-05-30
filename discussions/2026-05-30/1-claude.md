# Discussion — 2026-05-30 (1) — with Claude

Continues the zipper rework from `2026-05-29/3-claude.md`. Two threads:

1. **Implemented and merged:** rope structs participate in Racket's
   display/write protocol; `rope->string` removed from the public API.
2. **Pure design — not implemented:** several framing shifts that settle the
   descender's shape, the lens framework, and the rope/zipper layering.

## Custom-write hookup (commit 7decf20)

`leaf`, `leaf-range`, `branch` all carry `prop:custom-write` delegating to an
internal `rope-write-text` walk that writes to the port directly. `leaf-range`
skips the substring copy via `write-string`'s start/end args. All modes
(display/write/print) emit text uniformly -- `(write r)` doesn't round-trip
through `read`, but no round-trip was requested.

`(~a r)`, `(format "~a" r)`, `(display r)`, `(with-output-to-string ...)` all
yield/emit the rope's text. Public surface reduces 6 -> 5 exports. 26 tests
pass with all `(rope->string r)` swapped to `(~a r)`.

The "three coerce-and-fold functions" framing in the top docstring was tied
to the old `rope->string` carrier; reduced to two factories plus a one-liner
about the print protocol.

## Ordinary vocabulary convention

Added to assistant memory: when the user uses idiosyncratic terms in design,
the assistant replies in standard technical vocabulary, doesn't mirror.
Pairs with the existing idea-density preference.

## Zipper design -- framing shifts

### Splitter shape evolution (abandoned)

The variadic k-splitter from `2026-05-29/3-claude.md` iterated through:

- **k+1 piece-continuations, each receiving (lsum, piece, rsum)** --
  splitter output = lens forward shape.
- **Plus `on-here` for the between-pieces case**, receiving the full
  focused bisection.
- **Map-then-combine model** -- all on-piece-i fire as transforms; on-here
  receives their *products* and combines. This was the lens forward + repair
  pattern materialised inside the splitter signature.

### The impasse -- "decide before recurse" wiring

The map-then-combine splitter (k+1 on-piece continuations + on-here taking
their products) had a chicken-and-egg under guided search:

- on-here needs to inspect the boundary summaries to pick a direction.
- But if it receives fully-evaluated products from both recursive
  sub-searches, both sides have already been searched -- defeating the point
  of guidance.

Two ways to fix the wiring, both awkward:

- **(a) Pass the guide into the splitter itself.** The splitter consults the
  guide and dispatches to one on-piece only -- but this reverts to "select-one"
  semantics and undoes the map-style generality the variadic shape was meant
  to provide.
- **(b) Curry on-here in two stages.** First application takes the boundary
  summaries; the result function takes the continuation products as thunks
  and forces only the ones it wants. Works but adds two layers of indirection
  plus thunks.

Both paths added complexity to the splitter's signature -- either threading the
guide through (losing the uniform shape) or layering lazy machinery on top.
Stepping back to "the splitter is too complicated" sidestepped the question:
with `test direction` + `go-left` / `go-right` as separate primitives, there's
no map, no on-here, no thunks, no chicken-and-egg. The caller decides before
stepping; each step is one cheap operation.

### The split-and-stay-at-gap framing (current)

Smaller primitive set, but doing the bisection separately for the direction
query and the descent doubles work (bisection is potentially expensive once
balancing is in). Bundling is needed.

A descent step:

1. **Split** (one bisection). Push L and R as crumb stashes. Head
   transitions from `seg(t)` -> `gap`.
2. **Guide queries at the gap** -- reads cached summaries on the stashes;
   no re-bisection.
3. **Dispatch:**
   - `-1`: absorb L into head; R stays in crumb.
   - `+1`: absorb R into head; L stays in crumb.
   - `0`: stay. The gap **is** the destination.

One bisection, one guide query, one descent. The bisection's products live
in the zipper state, reused for free by anything downstream. "Cursor between
characters" is just "stop after split."

### 3-way unification

Every split is a 3-way structure `(left | middle | right)` from one bisection
of t into `(L, R)`; the three cases differ only in which slot the focus sits:

- **Gap:** `(L, empty, R)`.
- **Go-left:** `(empty, L, R)`.
- **Go-right:** `(L, R, empty)`.

`roper`'s smart-empty-drop recovers the original t identically in all three
cases. Consequences:

- The variadic k-splitter idea locks at k=2 forever. No higher arity.
- Gap vs seg = "is the middle empty?" No separate types.
- The lens framework collapses to a *single* lens -- `split-lens(guide)` --
  whose forward picks one arrangement and whose repair is always
  roper-the-three.

### Lens as (middle, repair) with head-shaped middle

```
split-lens(guide):
  forward : head_in -> (head_out, repair)
              head_in  = (b, t, a)
              head_out = (new-b, middle, new-a)
              repair   = lambda head* -> (b, roper(left-stash, head*'s rope, right-stash), a)
                         (captures b, a, left-stash, right-stash)
```

`middle` being a head (rope + anchors) is what unlocks the rise containment
test working locally off the head's anchors. Crumbs become just repair
closures stacked; rise = pop + apply. No bespoke crumb data type.

## Layer decision (rope vs. zipper)

### Rope-core (4 public exports)

- `summariser`, `roper`, `bisect` (new), `prop:custom-write` hookup.
- Internal helpers also useful: `atom?`, `empty-rope`, `rope-algebra`.

A genuinely pure rope library -- make, summarise, walk structurally, display.

### Zipper-core

**Types:**
- `head` struct -- `(before-sum, rope, after-sum)`.
- `zipper` struct -- `(guide, head, crumbs)`. **Guide is part of state.**

**Helpers** (operate on internals, curried guide where applicable):
- `(descender guide) -> head crumbs -> (values head' crumbs')`
- `(ascender guide) -> head crumbs -> (values head' crumbs')`
- `rise : head crumbs -> (values head' crumbs')`
- `(contains? guide) -> head -> boolean`
- `cut : head guide on-descend on-stop -> ...` -- CPS bisection + guide dispatch.
- `fixpoint` -- iterate any step to convergence.

**Public ops** (take zipper, dispatch to helpers, repack):
- `start`, `navigate`, `with-guide`, `to-root`, `edit-head`, `insert`,
  `delete`, `text`, `view`, `at-root?`, `at-gap?`.

### Conventions

- Helpers take `(head, crumbs)` unpacked from zipper; public ops do the
  unwrap/wrap.
- Guide travels in the zipper state; public ops pluck it out and curry into
  helpers.
- Everything that takes a guide curries it (matches the `-er`/`-r` factory
  convention).
- `rise` doesn't take a guide; the guide enters only at termination via
  `contains?`.

### One module for now

Keep rope and zipper in the same file (`rope-core.rkt`) for now. If ever
separated, the rope side shrinks to genuinely structural primitives only
(struct definitions, raw bisection, raw concat, balancing). Everything
semantic -- summaries, guides, lenses -- migrates upward. Sharper cut than
`2026-05-29/3-claude.md`'s "rope library vs navigation framework."

## Status

**Implemented and on master (commit 7decf20):**

- `rope-core.rkt` `prop:custom-write` hookup; `rope->string` removed.
- 26 tests pass.
- `feedback_ordinary_vocabulary.md` added to assistant memory.

**Pure design, not implemented:**

- The `cut` / `descender` / `ascender` / `rise` / `contains?` code paths.
- The `head` struct (zipper-side).
- The `zipper` struct with guide as a field.
- The public op surface (start, navigate, edit-head, insert, delete, text,
  view, etc.).
- The `bisect` extraction in rope-core (the existing `splitter` already does
  it inline -- four-line pull-out).

## Open / parked

- **smr recovery.** Still open from `2026-05-29/3-claude.md`. Descender draft
  recovers smr via `(rope-algebra t)`; alternatively, smr could live as a
  field on the zipper (constant across document).
- **Atom-case signal.** Return same state for fixpoint convergence is the
  simplest; alternatives are a sentinel return or an exception.
- **The lens menagerie question is settled** (was parked in
  `2026-05-29/3-claude.md`) -- there is one lens, parameterised by the guide.
  `lens-by-guide`, `lens-pick-piece` etc. dissolve into compositions of
  split-lens applications.
- **Insert/delete mechanism question is settled** (also parked in
  `2026-05-29/3-claude.md`) -- under the 3-way unified head, edits transition
  between gap (empty middle) and seg (non-empty middle) via `edit-head`. The
  crumb repair handles either uniformly thanks to roper's empty-drop.
- **`view`'s shape** -- windowed read needs to descend into a window region,
  snapshot, return text. Mechanism (fork zipper vs. transient descent + rise)
  still unspecified. Connects to the still-open S-specific module question
  (display dimensions in the summary).
- **Balancing** -- still deferred. The cost-of-bisection arguments anticipate
  it being added later.
