# deprecated-6

**Per-file pre-change snapshot, not a whole generation** — like `deprecated-5`,
this copies the live file just before its own change, rather than archiving a
whole pre-rewrite generation.

Kept as the pre-trim reference for the **comment trim**, applied to one core file
at a time: paring the source back from its essay-dense commenting to a "literate
but disciplined" level — section dividers plus terse load-bearing why-notes stay
inline, while design narrative, behaviour, and worked examples move to the Scribble
docs (`scribble/rope-core.scrbl`, `scribble/zipper-core.scrbl`), and the deep design
record stays in `discussions/`. No code behaviour changes; only comments move or shrink.

- `rope-core.rkt` — the heavily-commented state just before its trim. **481
  lines** (with tests) — the size baseline to compare the trim against.
- `zipper-core.rkt` — the cursor machine just before its trim. **335 lines**
  (with tests) — the size baseline to compare against.
- `summaries.rkt` — the general summary toolkit just before its trim. **188 lines**
  (with tests) — the size baseline to compare against.
Each snapshot freezes only its own comments; its `require`s point at the live root files via
`../../`, not at frozen dependency copies. `rope-core.rkt` pulls `../../helper-algebras.rkt`;
`zipper-core.rkt` pulls `../../rope-core.rkt` and `../../helper-algebras.rkt`; `summaries.rkt`
pulls `../../rope-core.rkt` and `../../summaries/sexp-summary.rkt` (its tests also `../../summaries/summary-laws.rkt`).
All `raco test` standalone here: 34 for `rope-core.rkt`, 25 for `zipper-core.rkt`, 157 for
`summaries.rkt` (`rackunit`, plus `rackcheck` for `summaries.rkt`).
