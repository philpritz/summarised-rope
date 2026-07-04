# Discussion — 2026-07-04 — the lines-zip: scrolling a summarised rope — with Claude

A **design/exploration session**: a scrolling line viewport designed and
benchmarked in chat. One module landed; the exploration — the options weighed and
the cost model behind them — is the artifact. Building a **lines-zip** (an up/down
scrolling window over the document's lines) drove a sequence of forks, each settled
by where the cost actually goes.

**Landed:** `text-edit/lines-zip.rkt` — the slack-buffered viewport
(`open-lines-zip` / `scroll` / `lines-zip-window`), on the linecol scroll path; 10
tests. (The `toolbox/` + `text-edit/` folder moves and the bundle-owner protocol
landed the same session; those are the commits' record.)

**The cost model — what every fork turned on:**
- Navigation is O(log n) in tree ops, but the constant is the **summary algebra
  recomputed at each `rope-join`**. On the lisp bundle a combine rebuilds an 8-arm
  per-mode hash, so navigation is **24× the linecol cost** (~1641 µs vs ~69 µs per
  scroll; splitting the line off the focus is ~35 µs / ~2 µs — nearly free).
  Conclusion: keep `lisp-smr` off the scroll path — scroll on linecol, colour the
  visible lines lazily (that's `lisp-view`'s job).
- An adjacent scroll is **O(1), not O(log n)**: flat ~70 µs from 2 000 to 128 000
  lines. The crumb stack makes locality free — `rise` climbs a crumb or two, never
  re-descends from the root.

**Forks:**

- **A view over the zipper, not a zipper↔flat-zipper iso.** The first shape was a
  flat zipper `before · deque-of-lines · after` with a lawful `iso` back to the
  machine zipper. Rejected: it is not an iso once you move — the frame is inherently
  linecol (scroll is by rows), so the forward map forgets the cursor's guide
  identity (a *forgetful retraction*, not invertible; a mid-line or sexp cursor
  can't be recovered, and carrying the guides verbatim is an iso only for a frozen
  frame, which is moot). So the lines-zip is a **view parametrised by a linecol
  frame `(n, m)`**: the band is derived by re-guiding the zipper to those rows; the
  frame is integers, guides rebuilt fresh each move, never carried or lost.
  Consequence: the display frame (linecol, coarse) and the edit cursor (exact,
  anchored) are separated — the lines-zip owns only the former.

- **Slack buffer over a strict focus.** Strict = the navigated band *is* the window,
  so every scroll re-navigates (~160–188 µs). Slack = the band is a buffer `[N, M)`
  holding the window plus overscan `s` on each side; scrolling *within* the slack is
  index math (no navigation), refill only at the buffer edge — once per `s` scrolls.
  **~8×** faster (21 µs vs 160 µs). Refill re-centres so slack is restored both
  sides, which is what stops boundary thrash. Cost accepted: the buffer slice
  (`lines-zip-window`) is O(buffer) per scroll — the residual cost, since navigation
  is gone.

- **Loose (tolerant) guides over exact line cuts for the buffer.** The buffer only
  has to *contain* the window+slack, not sit on exact line boundaries. A loose guide
  returns `0` over a line-count band, so the descent halts at a coarse rope boundary
  without drilling to the newline. **~2×** cheaper refill (134 µs vs 298 µs), and it
  overshoots → more free slack → rarer refills. Not new machinery: `within-ratio`
  (the balance comparator `bisect` already uses) is the same tolerant guide on
  subtree size; an exact guide is the width-0 case. Under a loose guide, `ascend`
  guarantees the focus contains the *inner* band and `descend` tightens it to the
  *outer* band — the focus squeezed between the two interpretations. Cost accepted:
  the buffer edge is representation-dependent (lands wherever the tree offers a cheap
  boundary) — fine for a covering buffer, wrong for an anchored cursor — and it
  starts mid-line, needing an `LN` offset to slice.

- **Smart reframe over dumb re-fetch.** A move reuses the overlap with the current
  window and fetches only the non-overlapping rows: local scroll fetches 1 and
  reuses `h−1`; a far jump fetches all `h` but in one navigation, so its cost is
  distance-independent. Dumb re-splits all `h` every move. Smart is flat in `h`,
  dumb linear (2.4× at h=100). Largely subsumed by slack (which navigates rarely
  anyway) — but it's what makes a refill and a jump cheap.

**Parked / not built:**
- The **crumbs↔lines-zip iso** (context-to-context, `∂Tree` vs `∂List`, guides
  excluded) — cleaner in theory, but needs the flank *ropes* extracted from the
  opaque crumb closures; the re-navigation overlay sidesteps the whole gap.
- The **put** (editing through the viewport) — structurally present in every stage,
  not exposed; read-only for now.
- A **visible-window deque** for O(1) slice (currently `lines-zip-window` is
  O(buffer)); the loose-guide `LN` tracking; surfacing `within-ratio` / the
  tolerant-guide notion as a first-class concept.
