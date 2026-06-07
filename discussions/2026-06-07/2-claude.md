# Discussion — 2026-06-07 (2) — with Claude

Picks up directly where `2026-06-07/1` left off: that session settled the index
encoding (A), chose the anchor direction, and designed — but did not build — the
**gap/seg unified carve machine**. This session is that machine, worked as an
*options exploration*. Little is closed; the value here is the map of forks and
what each buys. All chat sketches; nothing built or tested.

## The constraint everything hangs on
A seg guide's two edges are `gs = signum(P+1)`, `ge = signum(P-1)` of a 5-state span
position `P`. They are shifted by 1, so they coincide only where `|P| > 1` — never at
a boundary, where a gap landing needs both `= 0` (`gs = ge`). So **a gap is not a
degenerate seg**; the gap/seg distinction is real and must be *placed* somewhere.
The session is about where.

(Framing, recorded so it isn't re-derived: the `head` `(before rope after)` already
holds a possibly-empty middle, so **the data already unifies** — gap = empty middle.
Only the *machine* — guide arity / terminate / landings — was gap-committed. So the
question is the machine and the guide, never the head.)

## Fork 1 — how the descent treats gap vs seg
The central fork; all still live.

- **all-gaps:** the descent only knows gap guides; a seg is composed *above* it (two
  gap navigations assembled). Keeps the old `toward` untouched; needs the core to
  expose primitives (`split-rope`, a three-way `recut`) and gives up a shared descent.
- **everything-seg / two-edge:** the descent reads two edges; a gap is the diagonal
  `gs = ge`. One machine, shared descent — but retires the old single-guide `toward`.
- **two gap guides (the direction we leaned):** the *atom* is the gap guide; a cursor
  is a **pair** `(gs, ge)`, a point the diagonal `(g, g)`. This is the all-gaps and
  two-edge ideas reconciled — there's no single "seg guide" in the descent, so "can't
  reduce a seg to a gap" stops biting (you never have one to reduce).
- **single coerced `G`:** drive the descent with one 3-state `G` = *where is the whole
  segment relative to a cut* (`-1` left / `0` touching / `+1` right). Shown below to be
  the same machine as two-edge.
- **distinguish in the core:** a kind tag, or two entry points `navigate-gap` /
  `navigate-seg`; the core branches. *Against* the Part-E "core never names the guide
  kind" rule. Parked.
- **guide carries its own resolution (strategy):** the core exposes primitives + a
  generic `resolve` that calls the guide's own method; core knows neither kind. Most
  decoupled; keeps the old `toward` live as the gap strategy.
- **thin core / toolkit:** the core exposes only primitives (`bisect`, `lens`,
  summary reads, `split-rope`, `recut`); *both* gap and seg navigation are assembled
  above it. Distinct from the strategy option (the guide isn't a strategy object;
  the assembly is just client code).
- **seg as two anchors:** no seg-guide concept at all — a seg is two anchors resolved
  to two gaps; the cursor holds two marks. Lines up with `future/`'s two-marks. (This
  is the same instinct the "two gap guides" model formalizes.)

## Fork 2 — index / address representation
- **uniform `[start end]`, gap `[n n]`** (leaned): one shape, point = diagonal; agrees
  with `future/` two-marks. Cheap because an index is a *position, not a projection* —
  so `[n n]` is a recoverable gap and the Fork-1 obstruction does **not** apply here.
- **store `n`, coerce to `[n n]`:** same as above with worse ergonomics — a union
  lifted on demand; still needs the same conversion. No gain.
- **gap-primitive `n`, seg a composite:** keeps the single index as base, but spreads
  the gap/seg split across two layers (representation *and* descent).

## Fork 3 — where the distinction is coerced
The distinction must surface at exactly one site; candidates weighed:

- **at `index → guide`** (leaned): each endpoint independently becomes a gap guide;
  equal endpoints give equal guides, so the diagonal falls out with no special case.
- **in the zipper descent (the `G` coercion):** coerce the seg guide down to one
  3-state `G` for routing; the descent is the original machine with `g → G`.
- **at `carve`:** keep descent uniform; `carve` decides gap vs seg (see Fork 5).
- **carry two guides, coerce nowhere:** the two-edge descent keeps `gs`/`ge`
  throughout; the distinction is just "are they equal."

These aren't exclusive — the leaned shape places it at `index→guide` for the index
and at `carve` for the extraction, with the descent uniform in between.

## Fork 4 — the descent's concrete form (these are equivalent)
- **two-edge 4-probe `match*`** — columns `start@edge | start@seam | end@seam |
  end@edge`; chop the half wholly outside the span; straddle returns `#f`.
- **single-`G` original 7-case** — the *original* `toward` verbatim with `g → G` and
  the three landings `place → carve`; routing and ascend-errors unchanged.
- **Equivalence (so it stays settled):** the routing reads only
  `(positive? gs@seam)` and `(negative? ge@seam)` — exactly `G`'s two non-zero cases.
  So two-edge and single-`G` are one machine; they agree on every strict case and make
  the same parked choice at a boundary-on-seam. The only difference is **carve
  bookkeeping** (two-edge keeps `gs`/`ge` in hand; single-`G` re-derives them).
- Sub-fork: `drive` folded into `toward` (recursive `(head,k) -> (head,k)` with
  early-break clauses) vs. `toward` as a single step with a separate `drive`. Parked.

## Fork 5 — carve
- **inline vs separate:** carve inside the descent (the straddle clause carves) vs.
  **separate, composed in `navigate`** (`ascend → descend → carve`; descend stops at
  the straddle). Leaned separate — descent stays pure discard-a-half, all reassembly
  confined to carve, run once on the smallest containing node.
- carve = two context-aware `split-rope`s (split at `gs`, then at `ge`) = design-note
  001's `split-span-rope` on the current core.
- **how carve tells gap from seg:** *detect the plateau* (`G`'s zero is a single point
  for a gap, a plateau across the interior for a seg) vs. *carry the gap/seg bit down
  from the index*. Open. The plateau is design-note 001's parked **0-plateau / fiber**.

## Fork 6 — where the guide machinery lives
- **two raw args** `navigate gs ge z` — simplest, but admits a **wrong-order bug**
  (caller can swap start/end).
- **opaque guide object** with `left-edge`/`right-edge` accessors, built only by smart
  constructors — safe by construction (can't misorder), Part-E-clean.
- **constructors in `zipper-core`, exported to the summary** — cycle-safe (the
  machinery is algebra-independent; the summary supplies only `P`/`g`). **Revises Part
  E's placement** (guide structure was to live in the summary file); justification —
  the struct/signum are generic and belong with the `descend` that consumes them.
  Trade: `navigate` takes a guide object, and the core names the guide type.
- **constructors in the summary file** — Part E as written.

Cross-cut: all-gaps / strategy / toolkit keep the old `toward` *live*; everything-seg
retires it (gallery only); distinguish-in-core keeps it but bolts on a kind-switch.

## Rejected
- **signum trick inside the descend** — feed `descend` a single 5-state seg guide and
  derive the edges internally. Rejected: it can never land a gap (the `(_ 0 0 _)`
  case is unreachable, since `signum(P±1)` never both read `0`). The signum
  construction survives only as one *pair-constructor* among others (for unit spans —
  the word/sexp containing the cursor), and even it emits two gap guides.

## Artifacts and side-changes
- Nothing built or tested — chat sketches only (`toward` in its equivalent forms,
  `split-rope`, `seg-split`, `carve`, `navigate`, `ascend`, `contains?`).
- **`design-notes/function-gallery.md`** created (unnumbered — its own thing): a
  curated home for deprecated functions kept for their shape; first entry the gap-only
  `toward`. Alternatives weighed for where to keep it: this session's note, a numbered
  design-note, a `deprecated-5` snapshot, a git tag — chose the standalone gallery.
- **`conventions.md` amended** (under *"Draft" means draft in chat*): landing work
  needs the user's own words ("write up" / "add it" / "push"), or "go ahead" to
  confirm an offer; a revision request or hook prompt is never sign-off. Prompted by a
  mid-iteration push that had to be reverted.
- Background: verified xi-editor's `Interval` — endpoints were always `usize` offsets
  (not characters), and the open/closed flags were dropped for plain `Range<usize>` —
  reinforcing `2026-06-07/1`'s "polarity lives in the reader, not in storage."

## Status
- **Settled:** the constraint (seg can't reduce to gap); the two-edge ≡ single-`G`
  equivalence; signum-in-descend rejected.
- **Leaned, not closed:** gap guide as the atom / seg as a pair / point as diagonal;
  index `[start end]` with gap `[n n]`; descent uniform with the distinction at
  `index→guide` and `carve`; carve separate from descent.
- **Open:** Fork 1 at the top (all-gaps vs two-gap-guides vs strategy vs toolkit);
  guide-machinery placement (Fork 6, incl. whether to revise Part E); carve's gap/seg
  detection (plateau vs carried bit); `drive` folded vs separate; the parked
  boundary-on-seam / 0-plateau / affinity cases.
