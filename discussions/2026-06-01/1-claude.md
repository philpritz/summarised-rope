# Discussion — 2026-06-01 (1) — with Claude

Reworked the zipper's *editing* model and pushed it (`7f4e03e`) on
`claude/2026-05-30/zipper-impl`. The whole session was really one decision — the
shape of the *index* — plus its downstream consequences. Design-led by the user;
Claude wrote the code and the lower-level choices. Each option is carried in
enough detail to reconstruct it.

## The index (the substance)

**The fork.** A cursor is addressed by an index. On an edit the cursor must stay
the seg the guide names (the invariant), and ideally insert covers exactly the
typed text while delete marks the hole (so delete+reinsert round-trips). What
shape of index makes edits behave?

**Basic A — pin each edge to a summary.**
The left edge is the cut where the before-summary reaches a value; the right edge
the cut where the after-summary reaches a value, counting from the end.

- *Pull:* an insert at a gap changes neither anchor (only the empty middle
  fills), so a seg pinned left-from-before / right-from-after keeps both edges and
  the typed text lands between them — auto-covers, no index change. (The rule:
  name each edge from the side the edit spares.)
- *Rejected — two-sided burden:* the from-the-left address is natural ("2nd child
  of the 1st form"); the paired from-the-right one ("…also 5th from the end") is
  not, and you'd need it for every address.
- *Worse for structure:* a flat metric gets the right address for free
  (`total − n`); a structural summary needs a whole *mirror* of the opens frontier
  (a closes-stack read right-to-left) — a second monoid the author must write.

**B — one from-the-left index, patched on each edit.**
Store a single from-the-left coordinate (char `n`, or a path); inserting `k`
rewrites it (point `n` → span `[n, n+k]`, the delta being the inserted summary).

- *Rejected — unstable index:* every metric edit perturbs it; structural edits
  need bookkeeping (siblings renumber, depth shifts on unbalanced input).
- *A and B are one boundary in two coordinate systems,* tied by
  `n_left + span + n_right = total` — B stores the left coord absolutely and
  patches when `total` moves; A pins the invariant anchor and never patches.
- *Corollary:* harden B against edits and it *becomes* A.

**Idea 3 — a pure structural path (the prior design).**
Address a form by its tree path, e.g. `(0 2)`; its extent is intrinsic (the
matching close), no char offsets.

- *Pull:* a path is coarse — blind to length — so it's stable under interior
  edits (typing inside a form doesn't move its path), and is one-sided and natural.
- *Rejected for editing — the slurp:* the path's last element is a sibling index
  (from-the-left); deleting the form renumbers later siblings, so `(0 2)` now names
  the *next* form, and delete→reinsert can't refill the hole (it knew which number,
  not where).
- *It's B's fragility at the structural level.* Kept for *navigation*, where no
  edit happens so it never bites.

**Chosen — frame path + a local both-ends char span: `((start end) path)`.**
A structural path to a *focus frame*, then within it two char distances — from the
frame's start, from its end.

- *It's A localised:* the from-the-right reference is the frame's own end, already
  pinned by the cheap one-sided path, so the second index shrinks to one local
  offset against that end.
- *Read, don't subtract:* get that offset by navigating to the spot and reading it
  off the frame's anchor — `total − start` fails because the whole has abstracted
  local structure away (a closed form's child count is gone).
- *Payoffs:* delete collapses the two distances onto the hole (no slurp,
  round-trips); the path keeps idea-3's interior stability; a gap is the zero-width
  case (`start + end = frame length`).
- *Convergence of all three:* idea-3's path (one-sided global addressing) carrying
  A's offsets (both-ends local anchoring).

**Resilience.** The index is two layers, which is why it generalises across summaries.

- *Positional floor:* additive distances from both ends — survives every edit,
  always round-trips; needs only an additive metric (text always has chars).
- *Structural layer:* the path — stable under interior edits; *re-homes* from the
  still-valid position under a structural edit.
- *Recipe:* carry the boundary fragments that reconstruct the local frame's
  extent, then read off the anchors. Wants a tree (laminar) structure; degrades as
  delimiters get implicit (indentation).

## Downstream decisions (consequences of the index)

Each had a real fork; a line apiece.

- **Move/edit split.** Movement keeps a single-coordinate gap; editing plants the
  both-ends seg. *Over* "carry the both-ends index always" (my take) — the cheap
  coordinate need never survive an edit (every edit goes through the plant), so
  movement shouldn't pay for the second anchor. The shape is Vim's, parameterised
  by the summary.
- **`to-seg` explicit, gated on the right summary.** *Over* a hidden `resolve` the
  plant reaches (my take) — the gate is *forced* (the right edge can only come from
  the after-summary), and it makes "becoming editable" an explicit act.
- **Layering.** Generic machinery in `zipper-core`, concrete guides in `summaries`
  (its only caller). *Forced* by dependency direction — `summaries` already needs
  `zipper-core`, so the guide type can't live in `summaries` without a cycle.

## Status

Pushed (`7f4e03e`); **88 tests pass**. `racket examples.rkt` runs a navigate+edit
session over char and sexp (insert covers; delete leaves a gap at the hole;
reinsert round-trips).

- `zipper-core.rkt` — rewritten: `guide` struct, `point`/`copoint`/`local-span`,
  move/seg cursor, `to-seg`/`select`/`insert`/`delete`. Old nav/mode layer removed.
- `summaries.rkt` — added `char-guide`, `sexp-guide`, `sexp-carve`,
  `sexp-form-span`; sexp summary unchanged.
- `examples.rkt` — the demo.

## Open / parked

- **`sx-chars-to-close` enrichment** — *parked, not rejected.* The clean "read
  `end` off the after-summary" route; the combine is intricate (context-dependent,
  needs the k-th unmatched-close position), so `end` is computed by carving the
  frame instead — equivalent result.
- **Symbol-run navigation** (old `run-axis`) — not carried across; a future guide.
- **`to-gap` / sexp char-gap insert** — not wired; sexp editing goes via `select`.
- **README** — pre-rework (stale since 05-29).

## Conventions

Added *Record the alternatives, not only the choice* to `conventions.md`; this
note follows it.
