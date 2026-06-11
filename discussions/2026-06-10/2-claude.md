# Discussion — 2026-06-10 (2) — with Claude

Mostly an **implementation session**: the June-9 anchor/index design taken to built and
tested. `sexp-summary.rkt` rewritten (signed `frontier` struct, completion counting),
`sexp-edit.rkt` replaced outright by a spine-index guide layer. Suite green (**261**). In parallel the user refactored `rope-core`
(`multisect` as values, `frame`) and `zipper-core` (`zipper-lift`, `peek`); the new
layer sits on those. The summary/edit files are working surface, open to change —
decisions there are recorded lightly.

## Anchors (design, now tested)

`anchor-left`/`anchor-right` are reads off the cursor's two summaries (06-07/1 Part G),
each from its **own side**, staged on the reads: `((anchor-left L R) g)` — guide to
guide. The sharpened claim, previously only implied ("same gap at flip-time"): **a flip
yields a different guide, which merely coincides positionally on the present text and
diverges after editing** — now a regression test (insert at a gap: the front index
re-resolves with the left neighbour, the back with the right).

For now **both sides anchor in the before-sexp** (the user: for simplicity): both
families point at form starts; the sign carries only the derivation side. *Parked with
this:* end-based anchors, lean/gravity across whitespace, tight own-end segs — segs are
half-open `[start, start)`.

## The summary & index layer (built)

- **Signed storage**: `opens` entries `+(k+1)`, `closes` `−(k+1)` — a value reads
  `(… (negatives) forms (positives) …)` and indexes read directly off the stack heads;
  no ±0. Reverses 06-07/1 Part D ("the +1 offset breaks additivity, so it cannot be
  stored") — disproved by the battery; the combine compensates at four marked sites.
- **Completion counting**: frames count on the enclosing level at their `)`, not their
  `(` (the inclusive bump was inherited, not reasoned). A frame's interior then
  prefix-extends its own start slot, and **spine comparison is naively lexicographic**
  — the interim repair machinery (lookahead window, ceiling clause, per-level moduli)
  dissolved with its premise.
- **The ½ is a read, not storage** (`fine@`), and belongs to **atoms only** — the one
  structurally invisible interior; frames' are spine-visible as depth. The 06-09
  uniformity bar holds at every cut (`front − back = N+2`).
- An index is a plain list (innermost-first, `forms` at the tail), sign-dispatched;
  `spine-cmp` runs on lazy SRFI-41 streams (variadic `stream-map`) with `−inf` tails.
  Surface: `fine@ spine-cmp slot-guide sexp-guides cursor focus edge-contexts anchors`.

## Status

`sexp-summary.rkt` **96** (incl. a 16-string × 5-chunk associativity battery);
`sexp-edit.rkt` **123** (guide-sign sweeps at every cut × target × side, flat and
nested; editing end-to-end; anchor divergence); whole suite **261**.

## Open / parked

- End-based anchors / gravity / tight segs (parked above).
- A frame's end slot is nobody's start — the start guide never reads 0 there.
- Top-level `forms` is direction-neutral, so the back tail's `−(forms+1)` stays
  reader-side.
- Spine address arithmetic (next/prev/parent, flip as data) undesigned. `README.md`
  still documents the pre-rewrite API.
