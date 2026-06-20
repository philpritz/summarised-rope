# deprecated-4

The whole pre-rewrite generation, kept as reference — the **Claude-written**
mock-up flagged in `discussions/2026-06-01/1-claude.md` ("a mock-up, to be cleaned
up and rewritten"), designed collaboratively but written by Claude and reviewed
only at the design level. The 2026-06-02 cleanup kept only the rewritten
`rope-core.rkt` in the active tree and moved everything else here; the zipper and
summaries are rewritten from scratch next session (the stack-machine direction in
`discussions/2026-06-02/1-claude.md`).

Self-contained — the four files build together against the old rope (their
relative `require`s resolve within this folder).

- `rope-core.rkt` — the old variadic summary rope (`summariser` / `roper` /
  `bisect`, the `leaf` / `leaf-range` / `branch` structs, the `rope-algebra` /
  `atom?` / `empty-rope` / `empty-rope?` exports). **243 lines** — the size
  baseline for the rewrite.
- `zipper-core.rkt` — the old crumb/guide zipper (the move/seg cursor, the
  `pick` / `descender` descent). To be rewritten as a stack machine.
- `summaries.rkt` — the old char + sexp-frontier guides.
- `examples.rkt` — the old navigate+edit demo.

For the rope's size reduction, compare `rope-core.rkt` here (243 lines) against the
active one (184).
