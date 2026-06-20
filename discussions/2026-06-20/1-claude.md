# Discussion — 2026-06-20 (1) — Scribble docs for rope-core's export surface — with Claude

An **implementation session** (with a design thread on doc style): stood up the
Scribble documentation for rope-core's public API and the conventions governing such
docs.

**Landed:**
- `scribble/rope-core.scrbl` — the four exports (`make-summary`, `make-rope`,
  `multisect`, `frame`), each with live `@examples`.
- `rope-core.rkt` — a `(module+ internal …)` re-exporting structural internals
  (`rope-leaves`/`rope-height`, the `leaf?`/`branch?` node shape). Public surface
  unchanged.
- `scribble/scribble-discussion-conventions.md` — the standard for export-surface
  docs (rough-render default; one `.scrbl`/file, a section per export; keep/offload
  calibration; **Function doc style**). Holds up `rope-core.scrbl` as the example; the
  style rules live there, not in this note.

**Forks decided:**
- **Demonstrate via an internals submodule, not the public API.** Fusion is invisible
  through the public surface (`leaf?`/`rope-leaves` internal). Over: provide
  `rope-leaves` publicly (rejected — leaks a structural internal the abstraction
  barrier keeps private); reflection / `eval:alts` (rejected — break example
  reproducibility). Chose a narrow `(module+ internal)`, inspection tier over a broader
  extension tier.
- **Returned function named after its code-internal name** (`make-rope → build`). Over
  `rope` (rejected — overloads the struct/value noun, collapsing the function/value
  split `smr` deliberately keeps) and `rp` (parked).
- **Contracts real over invented:** `any/c` not a made-up `summary-value`, but the
  explicit union `(or/c string? rope? summary-part? any/c)`; a shorthand only where the
  expanded arrow is unreadable.

**Parked:**
- The session's premise — **trimming `rope-core.rkt`'s comments to typical density**
  (rationale now in the scribble) — not started; a trial trim was reverted when work
  moved to chat-drafting.
- `rp` builder name (needs a code rename + nomenclature entry); `guide` as a defined
  contract (recurs in `multisect`/`frame`); `@defmodule` + `info.rkt` `scribblings`
  packaging; an "Internals (for tinkerers)" doc section.
