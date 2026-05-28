# Discussion — 2026-05-28 (1) — with Claude

Compact notes from a design session on segment indices, edit safety, and what
inserting an open paren should do. Pure design — no code came out of this
thread. Same-session housekeeping is noted at the end.

## Gap as a degenerate seg

Taken as central, not a convenience: a gap is a seg with an empty middle.

```text
(gap l r) ≡ (seg l ∅ r)
```

The two marks coincide; a point cursor is just the degenerate selection.

## Seg-guide design contract

Expected property of any well-behaved seg guide:

```text
start (left mark)  reads from the before-summary
end   (right mark) reads from the after-summary
```

Each mark is pinned to its own outer side — start as an offset from the left,
end as an offset from the right — rather than the end riding on `start + count`.
This is a discipline on the guide author, not new machinery. How strict the
"left-only-from-before / right-only-from-after" rule has to be is still soft.

## Why the contract matters: delete is safe by construction

Deleting the middle leaves the before/after summaries unchanged, so both
anchors still resolve to the same boundaries. The seg collapses to a correct
gap with no drift into neighbouring sexps. This resolves the parked
stale-index concern (see `2026-05-27/5-chatgpt.md`) as a *design discipline*,
not a post-edit fixup.

Contrast: a relative `(start, count)` end is defined *through* the middle, so
deleting the middle makes the count overrun into the following sexps.

## Offset vs anchor index — left open

Two dual schemes, deliberately not resolved:

- **Offset `(start, count)`**: gap = count 0, seg = count > 0. Shape is readable
  from the index statically, so `make-sexp-guide` can pick gap-guide vs
  seg-guide without resolving summaries, and `move/update-index`'s gap↔seg
  crossing is crisp (count crossing 0). Delete = count→0. Matches the
  two-guide-type model and the draft `(start count)` index. Costs: edits must
  update the index (coupling), insert needs `rope→offset`, and sexp-count is
  ill-defined for partial fragments.
- **Anchor (before/after)**: edits stay index-agnostic and robust to ill-formed
  inserts. Cost: gap/seg shape becomes a data-dependent runtime fact (the
  anchors happen to coincide), which weakens static factory dispatch.

They are duals: anchor = stable boundaries under middle edits (editing's home
turf); offset = fixed-width forward selection ("next 3 sexps").

## Inserting "(" — where the model cracks

The frontier summary handles "(" fine — it is one more entry on the `opens`
frontier, and the summary monoid is imbalance-robust. The real breakage is
that the seg *head* (arbitrary rope middle) is strictly more expressive than
the sexp *index* (whole balanced sexps only). Insert can drive the head into a
state no index can name, so the head↔index linkage invariant only holds on
balanced states.

## Decision: index cleanliness over insert behaviour

Privilege clean, always-balanced indices over the assumption that "insert puts
the segment onto the inserted middle." A lone "(" is not a nameable seg, so we
give up insert-selects-the-middle rather than admit unnameable cursor states.

## Emergent benefit: structural editing for free

If a cursor state must always be index-nameable, inserting "(" cannot leave an
unbalanced state. The general invariant is: **the segment stays balanced**, and
"(" is any behaviour that maintains that. Families:

- complete within the segment (e.g. insert a balanced unit), or
- restructure around it — push the open into context and descend a level
  (parens become crumbs, never middle content; the dual of `up`), or grow the
  segment to absorb an existing close.

The document is never transiently unbalanced; structured / paredit-style
editing falls out of the invariant rather than being bolted on.

## Parked: a concrete "(" / ")" picture (draft)

Proposed as a picture, with known issues:

```text
(f ^a b c)   --"("-->  (f (^a b c))   ; "(" wraps forward up to the next ")"
             --")"-->  (f () ^a b c)  ; ")" retracts the wrap to empty, ejects
```

Virtues: balanced at every keystroke; "(" and ")" are exact inverses; a ")"
can never dangle. Issues: the greedy swallow is visually surprising; ")" is
redefined as a shrink/eject verb, so there is no "confirm this wrap" action
(you would navigate away to keep it); borrow-vs-insert of the close is
ambiguous. Possible smoothing: incremental slurp/barf — "(" slurps one form,
")" barfs one. Open forks: greedy vs incremental; ")" as a shrink verb.
**Parked** — to think on more.

## Status

Landed (concept): gap = degenerate seg; the before/after anchor contract;
index-cleanliness over insert behaviour; structural editing as the benefit.

Open: offset-vs-anchor fork; strictness of the anchor contract; the "(" / ")"
insertion behaviour (parked); the structural-edit verb set; whether a lower
raw-text layer exists.

No code was produced from this design thread.

## Housekeeping this session

- Removed stale `zipper-core-draft.rkt`.
- Rebuilt all zipper update sites in `zipper-core.rkt` with `struct-copy` (no
  struct-update dependency); `start` stays the only full constructor.
- Added the "Merging to master" convention to `discussions/conventions.md`.
