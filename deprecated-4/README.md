# deprecated-4

Snapshot of `rope-core.rkt` as it stood before the 2026-06-02 cleanup rewrite,
kept for reference — specifically so its size can be compared against the rewrite.

This is the **Claude-written** rope library: designed collaboratively, but written
by Claude and reviewed only at the design level, never combed through line by line
— flagged as "a mock-up, to be cleaned up and rewritten later" in
`discussions/2026-06-01/1-claude.md`. The current cleanup does exactly that: combs
through it and rewrites it. This copy preserves the pre-rewrite version to diff
against.

- `rope-core.rkt` — the variadic summary rope: `summariser` / `roper` / `bisect`,
  the `leaf` / `leaf-range` / `branch` structs, and the `rope-algebra` / `atom?` /
  `empty-rope` / `empty-rope?` structural exports. **243 lines** — the size baseline.

The rewrite (on `claude/2026-05-30/zipper-impl`) prunes the public surface to
`{summary, rope, bisect}`, folds the nodes under a `tree` parent, drops
`leaf-range`, and adds a per-node `size` field for balancing. Compare against the
live `rope-core.rkt`.
