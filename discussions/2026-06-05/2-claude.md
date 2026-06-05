# Discussion — 2026-06-05 (2) — with Claude

Mostly an implementation pass on `zipper-core.rkt`: rebuilt the navigation machine
around an explicit lens, pushed `bisect` inside the guide-choosing layer, and bundled
the cursor into an opaque `zipper` object — built, 67 tests pass (35 in `zipper-core`,
32 in `rope-core`). Plus a short design reversal dropping the `measure` from the index
entirely (designed, not built), and a factory rename in `rope-core.rkt`. Design-led by
the user; Claude wrote the code and the lower-level choices.

## Part A — The zipper machine, rebuilt around a lens (built)

Starting point: `arrange` took `(smr b ls m rs a)` and returned a refocused head plus a
put-back closure. Two recognitions drove the rewrite.

### `arrange` is a lens — name it so

`arrange` returns `(focus, put)` — a getter result and a put-back closure. That pair is
a Store (Costate) comonad value; the function `head -> (focus, put)` is a lens in its
concrete (Store-coalgebra) form. Renamed `arrange -> lens`, curried `((lens smr) split
h)`.

- `lens` works on heads, not ropes. `(lens smr) : splitter -> head -> (values head
  (head -> head))`. The only rope-touching layer is the splitter.
- Naming: the combinator is `lens` (the abstraction you build and pass); the returned
  pair is a store value — the representation, not the name. (`store` was weighed and set
  aside as the shape-of-output.)

### Splitters — the partition vocabulary

A splitter is `rope -> (values ls m rs)` with `ls·m·rs = the focus`. `lens` applies it,
makes `m` the new focus, summarises `ls`/`rs` into the anchors, and the `put` rejoins.
Five builders:

```
(edge-l mt) (edge-r mt)         ; gap at a boundary (empty focus)
(seam l r mt)                   ; gap between two halves
(half-l l r mt) (half-r l r mt) ; one bisect-half becomes the focus
```

- The bisect family takes the halves `l r`; it does not re-bisect. Chosen so a descent
  step bisects exactly once (the seam read needs the halves anyway). A `rope -> triple`
  splitter that re-bisected would double the work per level.

### `toward` + `drive` — bisect lives inside the lens choice

`descend` used to call `bisect` directly — a layering leak (bisect is the rope
primitive; it should appear only under a lens). Fixed with a guide-aware chooser:

```
(toward g smr)   : head -> (values focus put)   ; one descent step, returns the lens pair
(drive step smr) : (h k) -> (h k)               ; apply step, push crumb, repeat until gap
(descend g smr)  = (drive (toward g smr) smr)
```

`toward` bisects once, then reads the guide at three positions and returns the lens pair
for the chosen splitter. (`(probe l r)` = `(g (smr b l) (smr r a))` — the guide reading
the cut `l|r` against the anchors `b`/`a`; `refocus` = `(lens smr)`.)

```
(match* ((probe mt t) (probe lt rt) (probe t mt))   ; left edge | seam | right edge
  [(0 _  _) (refocus (edge-l mt)       h)]   ; gap at left edge
  [(_ _  0) (refocus (edge-r mt)       h)]   ; gap at right edge
  [(_ 0  _) (refocus (seam   lt rt mt) h)]   ; gap at seam
  [(1 -1 _) (refocus (half-l lt rt mt) h)]   ; seam -1 -> into left half
  [(_ 1 -1) (refocus (half-r lt rt mt) h)]   ; seam +1 -> into right half
  [(-1 _ _) (error … "ascend further")]      ; target sits in `before`
  [(_ _  1) (error … "ascend further")])     ; target sits in `after`
```

Decisions:

- bisect inside the chooser, not in `descend`. Chosen over the prior
  bisect-then-route-in-`descend` form for the clean layering (bisect → splitter → lens,
  nothing reaching past the splitter). Cost: the per-level carry optimization is gone —
  `toward` recomputes both edge reads each level (3 reads vs the old 2+depth). Reads are
  `g` over cached summaries (O(1)), so it's a constant factor, not more bisects.
  Deliberate.
- Table reads left-to-right like the document (left edge | seam | right edge, seam in
  the middle column). Zeros (gap landings) first, seam routing next, errors last.
- Outward edge reads raise. Left edge `-1` = target in `before`; right edge `+1` =
  target in `after` — the focus doesn't contain the target, i.e. `ascend` didn't climb
  far enough. They `error`, turning the `ascend` precondition into a checked invariant
  (consistent with `contains?`: `L≥0 ∧ R≤0`).
- `half-*` rows pin their inward edge (`Le=1` / `Re=-1`). Necessary: when the target is
  left of the whole focus, the before-anchor already overshoots it, so all three probes
  read `-1` — and an unpinned seam row (`(_ -1 _)`) would route that `(-1 -1 -1)` into
  the left half instead of letting it fall through to the error. seam=0 never occurs in
  an error, so the zero rows need no pin.
- The literal `-1|0|1` table is gap-guide specific; a seg guide (`-2..2`) needs its own
  chooser — which is the point of `toward` being one instance `drive` can run.

### `to-root` as a fold

```
(foldl (lambda (crumb h) (crumb h)) head stack)   ; crumbs are head -> head; fold them, top first
```

Replaced the rise-recursion. `rise` now serves only `ascend`.

### The zipper object

The cursor was two threaded values `(h k)` plus `smr` passed on every call. Bundled into
an opaque `zipper` struct `(head stack smr)`:

- `smr` rides inside the zipper — captured once at `start`, never threaded again. The
  guide `g` stays the only per-call argument to `navigate`.
- Public layer wraps; the internal machine is untouched. Chosen over rewriting the
  machine to thread the struct: `lens`, the splitters, `toward`, `drive`,
  `ascend`/`descend`, `rise`, `contains?` keep their `(values h k)` threading and stay
  private. Only the six public ops wrap/unwrap.
- `head` is fully internal — in no public signature. To get there, `over` became a rope
  edit (`f : rope -> rope`, not `head -> head`; no longer needs `smr` since anchors
  don't move), and reading is `focus`/`to-root`.

Pruned public surface:

```
(provide start navigate over to-root focus gap?)
; start : smr rope -> zipper          navigate : g z -> zipper
; over  : f z -> zipper (f rope->rope) to-root  : z -> zipper
; focus : z -> rope                   gap?     : z -> bool
```

The old head-level `gap?` became internal `at-gap?`.

## Part B — Reversal on measures: the index is just a path (designed, not built)

`2026-06-05/1` (Part A) settled on carry the measure: index `(path start measure)`,
`measure 0` = gap, `>0` = selection, edits patch `measure`. This session reverses that.

### Drop the measure; the seg is unmeasured

The scalar `measure` is redundant with the focus content's own summary. The zipper's
focus is the seg, with intrinsic extent (it's a rope), and the right-edge coordinate is
recoverable any time as `smr(before, focus)`; the head already caches `before`/`after`
as summaries, so that read is O(1). Storing a separate extent number stored a derived
value beside the thing it derives from.

New position:

- index = `path` (start absorbed into the path; no measure) — one structural address.
- seg = the live focus, unmeasured. Empty focus = gap; non-empty = selection. gap /
  one-form / many-forms are not three index shapes — they are three sizes of the focus
  off one path.
- Default is a gap; a cover op grows the focus to the whole form it sits at the start
  of; a later op extends rightward over following siblings. Left-anchored, growing right.

### What this deletes

The whole measurer apparatus from `2026-06-05/1` Part A:

- the forward reader `read : content -> measure`;
- the rejection policy (block / restructure / hold-raw) — nothing measured, nothing
  rejected; the seg is free in what it can contain (a lone `(` is fine);
- the by-text / by-number measure-edit families;
- the "merge the measurer into the guide" step — with no measurer, the guide stays the
  plain `(left-total right-total) -> sign` comparator already built.

It sharpens the two channels: summaries drive guides (decisions, lossy, fine); the
actual ropes (focus + crumbs) carry content/extent (lossless). The rope content is the
extent channel — there is no third "measure" channel.

### Still "carry, don't derive"

`2026-06-05/1` rejected deriving an address from a summary (lossy → drift). This doesn't
reintroduce that: the left-edge anchor (`path`) is carried, the extent is the actual
content (lossless), not a summary projection. The dropped `measure` was itself a
lossy/failable derivation (`read` ill-defined on fragments); removing it is more in the
carry-don't-derive spirit.

### Open / parked

- Form- vs char-granular. `path`-only works because the cursor sits between/around whole
  forms. A char-precise gap (cursor mid-token) would need `start` (a within-leaf offset)
  back. So dropping `start` leans the parked char-vs-structural fork toward structural;
  char-level would be a separate mode. Not finally decided.
- Cover direction. Cover the form the gap is at the start of (rightward) vs the
  enclosing parent — pin the default gesture before building multi-form extend on it.
  Leaning rightward (matches the left anchor).
- Stored marks. A saved index is now just a `path` (left-edge address); how it rebases
  under edits is the still-parked marks / multi-cursor question — simpler now (no extent
  to patch), unresolved.
- The seg/edit layer, `index-core`, `summaries` remain unbuilt (as in `2026-06-05/1`).

## Other

- `rope-core.rkt` factory rename for clarity: `summary -> make-summary`, `rope ->
  make-rope` (the factory names collided with the data words). `smr` unchanged. Chosen
  over `make-summariser` / `make-rope-builder` (accurate but verbose) and reverting to
  `summariser` / `roper` (would undo `2026-06-05/1`'s `-er` drop). `make-` is the
  idiomatic Racket factory prefix; the mild "make-X usually returns an X, here a
  function" stretch was accepted.
- Conventions: added one bullet to `## "Draft" means draft in chat` — `"Draft" means in
  chat.`

## Status

- `zipper-core.rkt`: 35 tests pass (67 across both cores). Navigation machine built and
  pruned to the six-name zipper API; `bisect` confined to `toward`; not-contained
  raises. Tests dogfood the public API (one white-box check on `zipper-stack`/
  `zipper-head` for ascend depth and landing offset).
- `rope-core.rkt`: behavior unchanged, factory rename only.
- Part B is all design — the path-only / unmeasured-seg model supersedes `2026-06-05/1`
  Part A's measure-carrying index, and is not built.
- On `claude/2026-05-30/zipper-impl`; not merged to master.
