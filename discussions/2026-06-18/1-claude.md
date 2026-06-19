# Discussion — 2026-06-18 (1) — off-boundary char positions — with Claude

A **design / exploration session**: how a char position that isn't on a sexp
boundary is resolved when switching between char and sexp guides. Two approaches
weighed; **snap chosen**, exact built this session and reverted.

The setup: a char cursor sits anywhere; a sexp guide names only form boundaries.
Only **char→sexp** needs resolving — sexp→char is exact (sexp boundaries sit at
real char offsets).

## Exact — the summary carries the precision; no shift on switch

- **① Block = an atom + its trailing whitespace**, indexed by a char offset,
  stored at each frontier edge as a pair **`(atom-chars . ws-chars)`**: sum = the
  offset axis (block length), atom count drives seam fusion, ws count is the
  searchable whitespace length. A cut strictly inside a block reads an integer
  offset, carried innermost by `sand-spines`.
- **② Whitespace:**
  - *folds into the preceding atom's block* — ws cuts become integer offsets, the
    ½-lean is gone entirely. Orphan ws (right after `(`/`)`, no atom to its left)
    still collapses to the form boundary.
  - *stays a ½-lean* (original) — only atom interiors get offsets; fractional
    components remain. Removing the lean *without* folding regressed editing
    (form-starts landed before their separator), so folding is what makes
    lean-removal safe.
- **③ Frames:**
  - *no offset* — only atoms (leaves) carry offsets; frames use structural
    descent. Form-starts stay uniformly flush — atoms and frames agree at the
    boundary. They differ *inside*, which is inherent: a leaf's interior is chars,
    a branch's is child slots.
  - *offset too* — a per-frame char count in `opens` gives frames a char extent,
    making the innermost always a char offset, at the cost of **degeneracy**: a
    frame-interior cut is named both by descent and by the frame offset (accepted).

## Snap — the summary stays form-granular; the switch shifts the cursor  *(chosen)*

- char→sexp **shifts each cursor edge to a real sexp boundary** — a `z→z` op
  reading each edge's spine off `sand-spines` and re-installing a snapped
  `slot-guide`. No summary precision; the shift lives in the converter.
- **Direction policy:**
  - *outward* — left edge to the last boundary at/before, right to the next
    at/after. Widens to whole sexps; never crosses; always contains the original.
    **(default)**
  - *inward* — left to the next, right to the last. Narrows to sexps strictly
    inside; can collapse or cross.
  - *nearest* — each edge to its closest boundary; can cross.

## Why snap over exact

The cursor-shift resolves the mismatch on its own, so the summary needs no char
precision — and exact's machinery (block pairs; or the ½-removal with its
orphan-ws collapse; or per-frame char counts + degeneracy for frames) is more
than the problem warrants.

## Status

- Nothing landed. `summaries.rkt` / `sexp-edit.rkt` reverted to committed;
  **1239 tests green**.
- Snap chosen but unbuilt: `to-sexp` (snap) / `to-char` (exact) converters + a
  char-guide helper are the next step, and need the bundle so there's a char
  cursor to convert from. Outward is the default; inward/nearest parked.
