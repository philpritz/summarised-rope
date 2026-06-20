# deprecated-6

**Per-file pre-change snapshot, not a whole generation** — like `deprecated-5`,
this copies the live file just before its own change, rather than archiving a
whole pre-rewrite generation.

Kept as the pre-trim reference for the **comment trim**: paring the source back
from its current essay-dense commenting to a "literate but disciplined" level —
section dividers plus terse load-bearing why-notes stay inline, while design
narrative, behaviour, and worked examples move to the Scribble docs
(`scribble/rope-core.scrbl`), and the deep design record stays in `discussions/`.
No code behaviour changes; only comments move or shrink.

- `rope-core.rkt` — the heavily-commented state just before the trim. **481
  lines** (with tests) — the size baseline to compare the trim against.
- `helper-algebras.rkt` — copied alongside as `rope-core.rkt`'s sole local
  dependency (its `on`), so the relative `(require (only-in "helper-algebras.rkt"
  on))` resolves inside this folder. Its own pre-trim state, if it is trimmed later.

Self-contained here: `rope-core.rkt` needs only `(require racket/generic)` and
this sibling `helper-algebras.rkt`; it `raco test`s standalone (34 tests in its
own test submodule, `rackunit`).
