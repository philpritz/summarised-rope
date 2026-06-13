# Discussion — 2026-06-07 — with Claude

One long arc on the **guide / index / anchor system** for editing. Mostly design; one
built artifact (`sexp-summary.rkt`, 58 tests). Design-led by the user; Claude wrote code
and the lower-level choices, and ran a literature survey.

## Part A — The tension, and where the literature lands (framing)

The core tension: a **guide is lossy/non-invertible** (`summary → index` discards
information — two positions can share a projection), yet **editing needs a stable,
round-tripping address**. The very property that makes a guide good at *finding* a
position makes it unsafe for *restoring* one after an edit.

Survey of how other systems make a boundary survive edits — three strategies, and
everything reduces to one of them because a structural summary has no inverse:

- **anchor to content identity** — Peritext (`(side, opId)`), Yjs (`RelativePosition` +
  `assoc`). Robust; needs stable ids.
- **carry a number, patch it** — Emacs markers, CodeMirror (`mapPos`/`assoc`),
  ProseMirror (`bias`), OT. Robust on an *additive* metric.
- **don't store, re-derive via a guide** — finger-tree split by a monotone predicate. No
  stored state; ephemeral only.

Also noted: Xi's "rope science" was *least happy* with `Interval` open/closed endpoints
(the same boundary problem, unclean); edit lenses don't offer a *persistent index* at all
(positions live inside edits; alignment is the separate "matching lenses" problem).
Switching an anchor's **side**: only Emacs mutates in place
(`set-marker-insertion-type`); everyone else recreates.

Mapping to our own history: the **measure** idea (`2026-06-05/1`) = the *patch* camp
generalized from gap to seg; the **switch-side / read-the-other-summary** idea = the
*anchor* camp. They are the two horns of `2026-06-01`'s A/B fork. The summary read is a
**find**, not a **hold**: a stored structural count overruns under edits (the
`2026-05-28` warning), so holding still needs patch (measure) or anchor (identity).

## Part B — Direction chosen: anchor, with measure kept swappable

Pursue the **anchor** approach; keep the **measure** interchangeable behind one interface.

- **L2 law** — *resolve is gravity-independent*: both sides name the **same gap**;
  switching side changes lean, never position. This is what guarantees "both anchors
  point to the same gap," and it disqualifies a bare stored count as a pin.
- **Pin protocol** — a pin = a **coarse** structural locator (read from the summary) + a
  **fine** pin that fixes the exact gap. The fine pin is the swap point: *atom identity*
  (pure anchor, needs ids) / *within-fiber offset* (anchor that borrows a small measure) /
  *structural-only* (no fine pin). **Parked** which — it's the char-vs-structural fork in
  new clothes.
- The structural index is a **lossy/non-injective projection**: a *range* (fiber) of char
  positions maps to one address, so re-resolving can shift the cursor within the fiber.
  This is exactly Flutter/Blink **`TextAffinity`** (upstream/downstream at an ambiguous
  boundary); the "index as a set" view is abstract interpretation's γ. **Set aside** the
  cursor-shift for now, on the user's call.

## Part C — Index encoding: **A** (signed integer), over B

Two options for naming a gap from either side, given a frame with N children (N+1 gaps):

- **A** — one signed integer; sign = direction. Front 0-based (`0..N`), back =
  `−(k_right+1)` (`-1` = after last). Front `p`, back `p−(N+1)`.
- **B** — magnitude + an explicit direction flag; both 0-based, but `+0` and `−0` are
  *different gaps*, so the flag is mandatory.

**Chose A.** It is the **two's-complement construction**: a frame's gaps form
**ℤ/(N+1)ℤ**, the back rep is `front − modulus`, so the index supports real modular
arithmetic (`next/prev = ±1`, `flip = ±modulus`). It is self-describing (no field;
per-edge polarity for segs falls out of the sign), and it makes the ambiguous-zero state
*unconstructable* — whereas B reintroduces `±0` and then patches it with a flag. The
much-debated **`+1` offset is not a wart — it is the modulus.** Identities:
`front − back = N+1`; `N = front − back − 1`. *B wins only if* you later want the index
magnitude itself additive/storable, or direction grows past one bit (**parked**).

## Part D — Summary representation: both frontiers innermost-first (built)

- Store **`opens` innermost-first** (it already builds that way during the fold; just drop
  the `(reverse opens)` in `sexp-leaf` and in `merge-sexp-frontier`, and make
  `add-inner-sexp` a head op). `closes` is already innermost-first. *Over* the deprecated
  outermost-first `opens`, which existed only to feed root-first addressing — abandoned
  with that addressing.
- Safe because the monoid stores **counts** (additive); the polarity (`−(k+1)` view) lives
  in the **reader**, never in storage. The non-additivity objection dissolves under this
  split — the `+1` offset is precisely what breaks additivity, so it cannot be stored.
  *[Reversed as of 2026-06-10: the offset IS storable once both stacks carry it
  symmetrically and the combine compensates — see `2026-06-10/2` (signed storage), now
  in `sexp-summary.rkt`.]*
- Result: the summary is **symmetric around the seam** — `k_left = (car opens)` of
  `before`, `k_right = (car closes)` of `after`; `N` by zipping the two (lazy; usually
  only the innermost is needed).
- **Index also innermost-first** (head = innermost), so `flip` is a head op. *Over*
  root-first (better for compare / seg-prefix sharing, worse for flip); chose
  innermost-first per the user, accepting reverse-on-compare.

## Part E — Layering (designed)

- **`zipper-core` stays guide-agnostic**: `navigate` takes a *bare callable*; it exposes
  the summaries (`gap-sides` / `gap-summary`); and it owns the **zipper object** plus
  `set-guide` (install + **re-navigate**) and a `zipper-guide` accessor.
- **The guide struct, `flip`, and the concrete ops live in the summary file**,
  instantiated with the sexp algebra.
- No import cycle, *because the guide is a `prop:procedure` callable* — `zipper-core` only
  ever *calls* it, never names its type. So all guide structure can live in the summary
  file. *Over* putting the summary upstream of the zipper (inverts the dependency,
  recreates the `2026-06-01` cycle) — rejected.
- `set-guide` re-navigates uniformly (handles both index-swap = flip and guide-swap =
  navigation). `flip` restamps via `set-guide`; by L2 the re-navigation returns to the
  same gap (fiber caveat). A short-circuit (restamp without re-navigating) is **parked**.

## Part F — Gap/seg unification (designed; re-affirms `2026-06-02/1`)

A guide = a **pair of boundary comparators `(gs, ge)`**; a **gap is the diagonal
`gs = ge`**. The machine *always* carves with both; the gap is the empty-middle
degenerate — no special case. *Over* separate `navigate-gap`/`navigate-seg` ops (more
surface, splits the machine) — kept only as a possible staging step.

Comparator convention: `−1` left of the cut, `0` at it, `1` right. **`0` is the terminate
signal.**

- **Gap**, seam reads 0 → land an **empty focus at the seam**: the `seam` splitter returns
  `(lt, ∅, rt)`, `lt`/`rt` summarise into the anchors (real ropes parked in the crumb),
  `drive` stops on the empty focus. Not "the whole tree" — an empty middle, with the
  halves as context.
- **Seg**, straddle (`gs ≤ 0 ≤ ge`) → carve at *both* cuts into `(l, m, r)`; the **middle
  `m` (the slice subtree) becomes the focus**. Gap = the `gs = ge → m = ∅` degenerate.
- **Parked** (per `2026-06-02/1`): strict-descent convention, the `0`-plateau/dead-zone
  (the fiber), and a seg boundary landing exactly on a seam (inward-pin).

## Part G — The flip, concretely (designed)

Generalized index change reads from the three cut summaries `before | <seg> | after`:

- `edge-contexts`: the **start** edge sees `before | smr(seg, after)`; the **end** edge
  sees `smr(before, seg) | after`; a gap (empty seg) collapses both to `before | after`.
- `read-anchor (edge side depth …)`: `front` from the left context's `opens`, `back` from
  the right context's `closes`.
- `flip` = read the innermost count off the **opposite** edge, apply its view
  (`as-front = id`, `as-back = −(·+1)`), swap the index head. It's an involution; same gap
  at flip-time.

## Built

`sexp-summary.rkt` — the opens/closes frontier algebra ported onto the current
`rope-core` (`make-summary`), with `opens` innermost-first. **58 tests pass**, including
an **associativity battery** (chunked-rope summary == single-leaf summary across 9 inputs
× chunk sizes 1–5) that validates the innermost-first merge. Worked cut `(_ _ ^)`:
`opens "(_ _ "` = `(2)` (k_left), `closes ")"` = `(0)` (k_right).

## Status

- **Built:** `sexp-summary.rkt` (frontier algebra, innermost-first, 58 tests).
- **Designed, not built:** the A index encoding; the `guide`/cursor/`flip` layer; the
  gap/seg unified carve machine; the pin protocol; the `zipper-core` changes (`set-guide`,
  `gap-sides`/`gap-summary`, guide-agnostic `navigate`).

## Open / parked

- The fine pin (atom-ids / within-fiber offset / structural-only) — the char-vs-structural
  fork.
- Cursor-shift within a fiber (affinity) — set aside.
- Anchor vs measure for *holding* a seg (the pin's swappable fine half).
- `set-guide` re-navigate vs a flip short-circuit.
- The `0`-plateau and seg at-seam touching cases (`2026-06-02/1`).
- B encoding, if an additive index is ever wanted.
