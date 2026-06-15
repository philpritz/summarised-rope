# Documentation brief

This brief is a statement of the desired goals and overviews of the manual's
sections.

The manual has three sections:

- **Section 1 — General use:** driving the shipped sexp system to build an
  editing sequence.
- **Section 2 — Defining custom guides & summaries:** building a new
  summary/guide system over the rope.
- **Section 3 — Internals / technical details:** the machinery itself.

## Section 1 — General use

Teaches everything needed to drive an existing summary system (here, the sexp
instantiation) to build an editing sequence, and no more.

1. **Frame** — drive the shipped sexp system; one document is carried from the
   first line to the last; each step states *what* the operation does, gestures
   at *how*, and gives the *why*; the mechanism stays out (that is §3).
2. **The cursor** — a zipper over the rope; a gap (a point) versus a seg (an
   interval); it prints as the document with the focus marked.
3. **Addressing (spines)** — name a position as an innermost-first, 0-based slot
   list; hand-write a front spine to place a cursor.
4. **Reading & editing** — read with `zipper-focus`; replace, delete, insert,
   and wrap all go through it; every write re-navigates.
5. **Flipping & stability** — meet the drift first (a cursor that slips under
   edits), see why through families, and reach for `cover`/`flip` as the answer.
6. **Movement** — reposition without editing: `move` (a whole new index) and
   `spread` (per-edge slot).
7. **A full sequence** — `chain` a complete editing sequence and read it back.

## Section 2 — Defining custom guides & summaries

Builds a complete custom system over the rope through one small worked example,
then validates it and names the obligations.

1. **Frame** — the rope and zipper are guide-agnostic; supplying a summary and
   guides over it is what makes them act; taught through one small custom system
   built piece by piece.
2. **The summary** — define an example summary via `make-summary` (a
   `string-summary` leaf measure plus a `combine`).
3. **A guide** — a comparator `(L R) → {-1,0,1}` over the summary; install it and
   navigate.
4. **Flipping** — the two anchorings of a position and the flip between them,
   shown in miniature (the same families as §1, at small scale).
5. **The laws** — run the `summary-laws` battery; what each law group catches
   when it fails.
6. **Reference** — the required contract: `string-summary`, `combine`
   (associative, identity = `(string-summary "")`), values with a sensible
   `equal?`, and the guide signature, monotonicity, and framing.

## Section 3 — Internals / technical details

The machinery itself. (To flesh out.)
