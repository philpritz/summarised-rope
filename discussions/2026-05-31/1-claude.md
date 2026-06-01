# Discussion — 2026-05-31 (1) — with Claude

Implements the zipper rework designed in `2026-05-29/3-claude.md` and
`2026-05-30/1-claude.md`, then carries it well past that design: a rope/zipper
file split, a guide-construction mechanism, and a path-addressed sexp summary.
All work is on the feature branch `claude/2026-05-30/zipper-impl` (one branch for
the session; not merged to master).

**Caveat up front — this session is Claude-heavy.** The user set the design
directions; Claude wrote all the code and made many lower-level choices. The
"Claude-authored — refine carefully" section below lists what to comb through.

## The arc

### Zipper implemented, then the descent refactored

First cut (commit `1e4a925`): `head` (before-sum, rope, after-sum) + `zipper`
(guide, head, crumbs); `cut` (CPS, on-descend/on-stop) + `step` + `descender` +
`ascender` + `contains?` + `rise`; public ops. Then reviewed another session's
op-surface proposal and reworked the descent:

- **The head is the lens.** `arrange smr b ls m rs a` returns `(values head put)`
  — the refined focus and a `put` (head -> head) that rebuilds the parent. **A
  crumb is exactly a put.** `rise` = pop + apply; `to-root` = apply all.
- **Guides do not enter the lens.** `cut`/`split-lens(guide)` dissolved: the three
  arrangements (`split-left`/`split-right`/`split-gap` = `(∅,L,R)` / `(L,R,∅)` /
  `(L,∅,R)`) are guide-free and inlined into `pick`. `pick` (curried over the
  guide) bisects once, reads the guide at the L|R boundary, returns the step.
  This collapsed the double-bisect the 2026-05-30 note worried about.
- `descender` is now a flat loop on `pick` until the focus can't split.

### Layering: rope vs zipper, split into two files (`babc287`)

Decided **guides live in the zipper, the rope is guide-free.** Concretely:
- `rope-core.rkt` — pure rope: structs, summariser, roper, `bisect` (the one
  split primitive), custom-write. `splitter`/`rope-splitter`/`seg-splitter`
  **removed** (`989cf0a`). Exports the structural primitives the zipper needs:
  `bisect`, `rope-algebra`, `atom?`, `empty-rope`, `empty-rope?`.
- `zipper-core.rkt` — everything that reads a guide. `(require "rope-core.rkt")`.

### nav = a gap/seg mode switch; edits flip it; `realign-cursor`

The zipper's "guide" slot is a **`nav`** — not one guide but a switch over two:
a gap (point) guide and a seg (span) guide, plus the live `mode`. `navigate`
dispatches: gap -> `descender` (a point), seg -> `carve` (a span).
- `insert` -> seg mode, `delete` -> gap mode, then **`realign-cursor`** (= a
  re-`navigate`) so the cursor re-materialises against the live guide on the
  edited rope. Named after weighing words; chosen over "reharmonise".
- **Gap navigation always lands in a gap** (`atom->gap`, `6699f73`): at a single
  element, place it on the guide's side and leave an empty focus. Single-element
  selection/deletion is therefore a seg op, not a gap op.

### Guide construction + a shared, editable index

`nav` carries index-first **deciders** (`gap`/`seg` : index -> guide) and one
**shared index** the two modes read; their alignment is in their bodies.
- `point`/`span` build a gap/seg pair from a summary projection; `axis field i
  mode` bundles a one-dimension nav.
- `run-axis count starts? ends?` — navigate maximal runs (symbols), edges read
  off the flags (the old word-start/word-end shape).
- `addr-axis before after` — a nav whose index is an *address* (path), gap =
  before the form, seg = the form (carved between the before/after guides).
- Edits (plain `struct-copy`, no lens library): `with-index` / `move` (the shared
  index), `gap-mode` / `seg-mode`, `with-guides` / `with-axis`.

### Summaries (`summaries.rkt`)

- `char-count` (projection = identity).
- `sexp` — first a flat version (chars/opens/closes/min-depth + atom run-merge),
  then **redone as the opens-FRONTIER monoid** ported from
  `deprecated-2/summary-algebras.rkt` (`1e70810`): `closes` / `forms` / `opens`
  (a **stack**), with `merge-sexp-frontier` reconciling the left's opens against
  the right's closes and the atom-seam merge. Its value is the cursor's tree
  position, so guides address a node by **path** (`'(0 2 1)`). Plus the addresses
  (`sexp-next-address`, `next`/`previous`/`parent-sexp-address`,
  `sexp-path-compare`) and the `before`/`after-sexp` guides. Structural moves are
  `move` with a path function.

## Decisions (durable)

- Rope is guide-free (`bisect` only); all guide/lens logic is in the zipper.
- The head is the lens; `arrange` builds `(head', put)`; crumbs are puts.
- One bisect per descent step (`pick`); the guide-free arrangements are inlined.
- `nav` = (gap-decider, seg-decider, shared index, mode); deciders are index-first.
- Gap navigation always yields a gap; element selection is seg.
- `insert`/`delete` reharmonise via `realign-cursor`.
- The sexp summary is the opens frontier (a stack) for path-addressed navigation.

## Status

On branch `claude/2026-05-30/zipper-impl`, **99 tests pass** (`raco test
rope-core.rkt summaries.rkt zipper-core.rkt`). Runnable: `racket zipper-core.rkt`
(the `module+ main` demo) and `racket examples.rkt` (a structural editing
session). The deprecated-2 sexp test cases are ported as integration tests and
pass, so the frontier port is faithful on those inputs. Three conventions were
added (`discussions/conventions.md`): verify-premises, one-branch-per-session,
ask-when-ambiguous.

## Claude-authored — refine carefully (for a later pass)

The design directions were the user's; the following were Claude's to write or
decide, and want a careful human read:

- **All the code.** It was reviewed at the design level, not line by line.
- **The frontier port is the highest-risk piece.** `merge-sexp-frontier`,
  `drop-start-sexp-atom`, `add-inner-sexp`, `sexp-leaf`, `sexp-combine` are
  intricate. Lifted from deprecated-2 but *adapted*: added a `chars` field, an
  `atoms` run-count, and an **empty-`sx` identity (chars=0 short-circuit)**
  instead of the old `#f`. The ported tests pass, but edge cases beyond them
  (unbalanced input, deep/odd chunkings, the `closes` list) are unverified.
- **Guide monotonicity.** `split-at`'s binary search assumes each guide is
  monotone over offset. This held (and a real `runs-end` non-monotonicity bug was
  found and fixed mid-session, `d54e4d6`), but the assumption isn't proven for
  the frontier guides — only exercised by tests.
- **Judgment calls made under "do it however":** the `arrange`/`pick`/`descender`
  shape; `atom->gap`'s side-choice; `insert`/`delete` reharmonising but **not
  auto-advancing** (a fixed seg-guide would snap weirdly — flagged, not solved);
  the combinator API (`point`/`span`/`axis`/`run-axis`/`addr-axis`); naming.
- **The 1-indexed path quirk** (`'(0)` = whole form, children from `'(0 1)`;
  trailing zeros normalise to the parent boundary) is inherited from the old
  scheme — confirm it's the wanted ergonomics.

## Open / parked

- **insert auto-advance.** Reharmonise keeps the cursor at the guide's target; to
  advance past typed text needs a shiftable/inspectable index (the old
  movable-index question). Parked.
- **opens-dimension seg swallows.** `(span sx-opens)`-style seg runs to the next
  open paren (not a clean unit); opens is for gap navigation only. Path/symbol
  segs are the clean units.
- **`view`** (windowed read) — still unspecified; ties to display dims in the
  summary.
- **Balancing** — still deferred; `pick`'s single bisect keeps it cheap to add.
- **Two navigation families coexist** — flat (char/symbol via `axis`/`run-axis`)
  and structural (path via `addr-axis`). Whether to keep both or consolidate is
  open.
- **Full structural moves.** `parent`/`next`/`previous` sibling are in; descend
  (`child`) and richer paredit moves aren't yet.
