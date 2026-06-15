# Nomenclature

The project's working vocabulary — the metaphors it leans on, the load-bearing
terms, and the abbreviations used inside the code. A living reference, not a dated
decision note: when a name is coined or retired, it changes here.

## The two metaphor families

Names come from two streaks, and most terms belong to one:

- **Woodworking / physical** — the rope is stock you shape. You `carve` it, `frame`
  a guide in its context, `cover` a region, `sand` a spine smooth. Verbs of
  working a material.
- **Structural / arithmetic** — the index and its algebra. `multisect`, `flip`,
  `base-left/right` — positions and the arithmetic that re-bases them.

The split is deliberate: a physical verb names *an operation on the rope*; an
arithmetic term names *a fact about the index*. When a new name is needed, pick the
family by which of those it is.

## Glossary

**Cutting and shaping**
- **cut** — a boundary position between two pieces of a rope.
- **guide** — a comparator `(L R) -> {-1,0,1}` that names one cut by deciding which
  side any candidate boundary falls on.
- **multisect** — the one split primitive: n guides → n+1 pieces.
- **carve** — cut the focus exactly at its boundaries, the middle becoming the new
  focus.
- **frame** — bake outer context into a guide, so it judges as if it saw the whole
  document.
- **sand** (**sand-spines**) — read both full spines at a cut: the all-left `front` and
  all-right `back`, each innermost-first, with the ½ refinement sanded into the head (a
  form start or the close-adjacent end slot reads integer; a mid-atom cut or whitespace
  leans ½, whitespace binding to the previous form). Smooths the cut to its fine-grained
  position: `(sand-spines L R) -> (values front back)`.

**Indexes and spines**
- **index** — a position in the document, named off the frontier summary.
- **spine** — the per-level slot list realizing an index, innermost-first.
- **anchor** — one of the two indexes naming the same position, one read off the text
  to its left, one off the text to its right; under edits each follows its own side.
  **flip** is the involution between the two; **re-anchor** swaps a cursor edge to the
  other; **cover** is the re-anchoring that keeps a cursor wrapping its focus across edits.

**Heads and cursors**
- **head** — `before · focus · after`: the focus rope flanked by the summaries of
  everything outside it.
- **gap** / **seg** — an empty focus (a single point) vs a non-empty one (an interval).
- **seam** — the boundary between a branch's two children, where the left child's text
  ends and the right's begins; `descend` reads the guide there, `concat` fuses there. A
  position between pieces (like a `cut`), not the node that straddles it.

**Summaries**
- **summary** (**smr**) — the cached monoid value at every rope node.
- **frontier** — the sexp summary value, the struct `(frontier head closes forms opens
  tail)`: signed `opens`/`closes` stacks, `forms`, and the `head`/`tail` char classes
  (`'atom | 'open | 'close | 'ws`) of its first and last chars.
- **battery** — the optional law suite (identity / associativity / homomorphism) offered
  to a summary's writer.

## Inner argument abbreviations

Short binding names used inside the implementation. The convention favours
two-letter mnemonics for the parts that recur across modules; single letters
survive where a function is small and local.

**The head triple** — `before · focus · after`:
- **`bs`** — before-summary (the summary of everything left of the focus).
- **`fr`** — focus rope (the focus itself).
- **`fs`** — focus summary (the summary of the focus rope).
- **`as`** — after-summary (everything to the right).

  *(Legacy: zipper-core binds these `b` / `t`|`m` / `a`. `bs` is preferred over `b`
  because `b` also reads as the back-spine in `sexp-edit`.)*

**At a cut** — the two sides a guide sees:
- **`L`** / **`R`** — the summaries left and right of a candidate boundary.

**The machine**:
- **`smr`** — the summary algebra (the folding function).
- **`h`** — a head; **`k`** — the crumb stack (the continuation); **`z`** — a zipper.

**Guides**:
- **`g`** — a single guide; **`gs`** — the installed guides (a vector of any number).

**Ropes and halves**:
- **`l`** / **`r`** — the left and right child ropes of a branch.
- **`lt`** / **`rt`** — the two halves from a bisect; **`mt`** — the empty rope.
