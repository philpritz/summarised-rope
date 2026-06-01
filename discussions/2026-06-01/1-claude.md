# Discussion — 2026-06-01 (1) — with Claude

Reworked the zipper's *editing* model and pushed it (`7f4e03e`) on
`claude/2026-05-30/zipper-impl`. The whole session was really one decision — the
shape of the *index* — plus its downstream consequences. Design-led by the user;
Claude wrote the code and the lower-level choices. Each option is carried in
enough detail to reconstruct it.

## The index (the substance)

**The fork.** A cursor is named by an *index*; after an edit you re-resolve it
against the changed document and it must still point where you meant: the cursor
stays on the seg the guide named (invariant), an insert covers exactly the typed
text, and a delete leaves a hole a reinsert refills (delete→reinsert round-trips).
What naming scheme survives an edit?

Running example: the document `(a (b c) d e f)`. By *tree path* `(0)` is the whole
form and children are 1-indexed, so `(0 2)` is the second child `(b c)`, one of
five children.

**Basic A — pin each edge to the side the edit spares.**
Name the left edge from the left context (the cut where its summary reaches a
value) and the right edge from the right context (counting from the end).

- *Pull:* an insert at a gap changes neither context — only the empty middle
  fills — so both edges hold and the typed text lands exactly between them.
  Auto-covers, no patching.
- *Rejected — two-sided burden:* you now need a from-the-right address for every
  seg. Child `(b c)` is "the 2nd from the left" but "the 4th from the right" (of
  five) — the from-the-left name is natural to write, its from-the-right twin is
  not, and you'd supply both for every address.
- *Worse for structure:* a flat metric gets the right address for free, as
  `total − n`. A structural summary doesn't — a from-the-right *path* needs a whole
  mirror of the summary (for the sexp opens-frontier, a closes-stack read
  right-to-left) that the author must write.

**B — one from-the-left coordinate, patched per edit.**
Store a single from-the-left coordinate (a char offset `n`, or a path); on
inserting `k` characters at the point, rewrite point `n` → span `[n, n+k]`.

- *Rejected — unstable index:* every edit perturbs it (a metric index shifts on
  any insert; a path needs sibling renumbering), so it must be rewritten each time.
- *A and B are one boundary in two coordinate systems,* tied by
  `n_left + span + n_right = total`: B stores the left coordinate absolutely and
  patches when `total` moves; A pins the invariant anchors and never patches.
- *Corollary:* harden B against edits and it *becomes* A.

**Idea 3 — a pure structural path** (`(0 2)`), the prior design.
Its extent is intrinsic (the matching close); no char offsets.

- *Pull:* a path is coarse — blind to length — so it survives interior edits
  (typing inside a form doesn't move its path); one-sided and natural to write.
- *Rejected for editing — the slurp:* the path's last element is a sibling index,
  named from the left, so an edit before it changes what it means. Delete `(b c)`
  and `(0 2)` now names `d` (what was the third child) — the selection slid onto
  its neighbour, and delete→reinsert can't refill the hole, because the path
  recorded *which number*, not *where*.
- *It's B's fragility at the structural level.* Kept for navigation, where nothing
  edits so it never bites.

**Chosen — frame path + a local both-ends char span: `((start end) path)`.**
A path to a focus *frame*, then two char distances inside it — from the frame's
start, from its end. To select `(b c)` the frame is its parent (the whole form)
and the span is `(b c)`'s char offsets within that parent.

- *It's A localised:* the from-the-right reference is the frame's own end, already
  pinned cheaply by the one-sided path, so the second anchor shrinks from a global
  from-the-right address (the mirror monoid) to one small local offset.
- *Read, don't subtract:* get that offset by navigating to the spot and reading it
  off the frame — not `total − start` from the document summary, which has
  abstracted local structure away (once `(b c)` is folded into the whole, the fact
  it held two children is gone). Localise first, then read.
- *Payoffs:* delete collapses the two distances onto the hole (no slurp,
  round-trips); the path keeps idea-3's interior stability; a gap is the zero-width
  case (`start + end = frame length`).
- *Convergence of all three:* idea-3's path (one-sided global addressing) carrying
  A's offsets (both-ends local anchoring).

**Resilience.** The index is two layers, which is why it generalises across summaries.

- *Positional floor:* additive distances from both ends — survives every edit and
  always round-trips; needs only an additive metric, and text always has chars.
- *Structural layer:* the path — stable under interior edits; under a structural
  edit it *re-homes* from the still-valid position the floor provides.
- *Recipe & limit:* carry the boundary fragments that rebuild the local frame's
  extent, then read off them. Wants a tree (laminar) structure; degrades as
  delimiters get implicit (indentation rather than parens).

## Downstream decisions (consequences of the index)

- **Move/edit split.** Movement keeps the cheap single-coordinate gap; an edit
  first converts it to the both-ends seg (the *plant*). Chosen over carrying both
  ends at all times — every edit goes through that conversion, so movement never
  needs the second anchor. It's Vim's shape (roam freely, then commit to an edit),
  parameterised by the summary.
- **`to-seg` explicit, gated on the right summary.** The gap→seg conversion takes
  the right summary as an argument rather than hiding inside the edit step — the
  gate is *forced* (the right edge can only be read from the after-summary), and
  surfacing it makes "becoming editable" deliberate.
- **Layering.** Generic machinery in `zipper-core`, concrete guides in `summaries`.
  Forced by dependency direction — `summaries` already requires `zipper-core`, so
  the guide type can't live in `summaries` without an import cycle.

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

Added two note-writing conventions to `conventions.md`, both followed here:
*Record the alternatives, not only the choice*, and *Write for a reader who wasn't
there* — a note must stand alone, decipherable from the repo without the
conversation (the examples above were reworked to that end).
