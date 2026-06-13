# Summarised Rope

An experimental Racket core for editing structured text over a persistent
summarised rope: a rope whose nodes carry a monoid summary, navigated and edited
through a guide-driven zipper, with S-expressions as the worked structure.

```text
rope-core.rkt     persistent summarised rope; the split primitive
sexp-summary.rkt  the sexp summary algebra (signed frontier)
zipper-core.rkt   the cursor machine; guide-agnostic
sexp-edit.rkt     spine indexes, anchors, re-basing, re-anchoring
summary-laws.rkt  optional law battery for summary writers (needs rackcheck)
```

`design-notes/` and `discussions/` hold the rationale (dated, decision-level);
`deprecated*/` are earlier generations kept for reference; `future/` parked
directions.

## The pieces

### rope-core

`make-summary` builds a summary algebra; `make-rope` builds (and concatenates)
ropes, coercing strings. `multisect` is the one split export: `(multisect
guides)` is a splitter cutting the rope at each guide's boundary — n guides give
n+1 pieces as values; no guides is the balance halve. `frame` bakes outer
context into a guide.

### sexp-summary

A string summarises to a signed frontier: `opens` — one `+(k+1)` per unclosed
open, innermost-first — `closes` (`−(k+1)`), `forms` (completed forms), and the
seam flags (`starts/ends-atom?`, `starts/ends-form?`). The combine is
associative (battery-tested across chunkings), so summaries merge across any
split. Counting is by completion: a frame counts on its enclosing level at its
`)`, which makes spine comparison naively lexicographic. The summary fn is
`sexp-smr`.

### zipper-core

The cursor machine, agnostic about what guides mean. A zipper is a head
(`before-summary · focus-rope · after-summary`), a crumb stack, and its
installed guides. A guide is a comparator `(L R) -> {-1,0,1}` (`+1` = the
target boundary is right of the cut); a cursor is a 2-guide vector `(start
end)`; a gap is `start = end` (empty focus), a seg is `start < end`.

Surface — five names:

- `start` — a zipper over a rope, no guides installed yet.
- `guide` — the three-faced navigation accessor on the installed pair: `(guide
  z)` reads; `((guide gs) z)` installs a 2-vector; `((guide f) z)` installs
  `(f current)`.
- `focus` — the three-faced editing accessor on the content: `(focus z)` reads
  the focus rope; `((focus c) z)` swaps in content (string or rope); `((focus
  f) z)` swaps in `(f current)`. `delete` is `((focus "") z)`; insert is a swap
  at a gap.
- `to-root` — fold the crumbs back into the whole document.
- `on-edges` — `((on-edges c f g) z)`: the cursor's two edge cuts — the focus
  folded onto the side each edge doesn't face — spread over `f` and `g` and
  combined by `c`. Generic over the zipper's own summary.

The invariant: **every write navigates**. The lift composes `navigate` in as a
permanent last op, so installing guides moves the cursor to where they point,
and a `focus` swap re-navigates with the installed guides on the new text. Both
accessors' write faces funnel through the one lift, so accessors composed over
them inherit the invariant — one navigation per write, at the outermost face.
`to-root` is deliberately outside the lift: homing must not navigate back down,
and the guides survive it.

## Indexes, anchors, flipping

An index is a **spine**: the per-level slot list, innermost-first, read straight
off the frontier. The head's sign picks the **family**: a *front* index (head
≥ −½) is derived only from the text to its left; a *back* index (head ≤ −1) only
from the text to its right. Both name the same position on the present text;
under edits each follows its own side. `fine@` reads both spines at a cut;
`slot-guide` turns an index into a guide, sign-dispatched; `cursor` navigates a
fresh zipper to one or two indexes.

At a cut the front and back heads differ by the **modulus** — the cut's frame's
completed-form count plus one. That single number is the flip data one index
alone cannot carry: re-basing is head-only (the path components are a left-based
name shared by both anchorings), so `base-left` / `base-right` shift the head
between families and `flip` (an involution) swaps it. `modulus` reads it from a
pair of summaries; `edge-modulus` reads it at a cursor edge, where `anchors`
reads both spines.

**Re-anchoring** is the flip as a cursor operation: `(re-anchor z i side)`
re-derives edge `i`'s guide from the chosen side's anchor (`'front` | `'back`)
and installs it through `guide`'s modify face. `(cover z)` flips the second
guide onto its right anchor: the start then reads only the text before the seg,
the end only the text after, so an edit between them touches neither — the
cursor keeps covering whatever replaces the focus, across swap, delete, and
insert, and the operation is idempotent.

## Example

```racket
(require "sexp-edit.rkt")

(define text "(aa (p q) cc)")
(define-values (^q _)                       ; front spine at the cut before q
  (fine@ (sexp-smr (substring text 0 7)) (sexp-smr (substring text 7))))

(define z  (cover (cursor ((make-rope sexp-smr) text) ^q)))   ; covered gap at ^q
(define z1 ((focus "x ") z))
(~a (focus z1))             ; "x "  with its trailing space -- the cursor covers it
(~a (focus (to-root z1)))   ; "(aa (p x q) cc)"

(define z2 ((focus "x y ") z1))             ; edits chain through the cursor
(~a (focus (to-root z2)))   ; "(aa (p x y q) cc)" -- q held its ground
```

## Domain and open edges

- Targets land on **form starts** and on a frame's **end slot** — slot N of an
  N-child frame, the gap before its `)`. Both read an integer spine head on each
  side: the end region carries a raw back head of −1, unconfusable with a
  mid-atom cut, whose straddled atom pushes the close entry to ≤ −2. A cursor in
  a frame's trailing whitespace lands at the plateau's left edge.
- Segs are half-open `[start, end)` — the end index is the start of what
  follows.
- Spine address arithmetic (next/prev/parent as index operations) is
  undesigned.
- Edits straddling a cursor edge are outside the covering protocol — re-cursor
  for those.

## Running tests

```powershell
& "C:\Program Files\Racket\raco.exe" pkg install --batch --auto rackcheck   # once; the law tests need it
& "C:\Program Files\Racket\raco.exe" test .\rope-core.rkt .\sexp-summary.rkt .\zipper-core.rkt .\sexp-edit.rkt .\summary-laws.rkt
```

1195 tests as of 2026-06-13, the summary-law battery included.
