# future/

Sibling-to-`deprecated/` in the opposite temporal direction. Each
subfolder is an exploratory project: a snapshot of the present core
that develops in a direction we're not yet committed to. If a future
project earns its place, it gets promoted up to the top level and the
old top-level becomes deprecated. If it doesn't, it stays here as a
record of what we tried and why.

```text
deprecated/    archives — what we used to do
future/        explorations — what we might do next
top level      the current core
```

## What we're trying to explore

The core innovation under exploration in `future/` is **moving away
from a single-mark cursor to a two-marks-on-a-loop cursor**.

In a traditional editor (and in our current core), the cursor is one
mark — a position between characters. Selection is a *mode*: a
second mark exists only while the user is selecting, and operations
behave differently depending on whether they're in point mode or
selection mode.

The shift we're testing: drop the mode. The cursor is **always** two
marks. A point cursor is just the degenerate case where the two marks
coincide. The rope is conceptually circular, so the two marks divide
it into two arcs; operations act on one designated arc, and a
single primitive — swap — picks the other one.

The consequences ripple through the whole algebra:

- One head shape instead of two (gap and seg collapse into one).
- One guide kind instead of two.
- One `navigate`, one `relative`, no dispatch on guide shape.
- Insert / delete / replace stop being mode-dependent — they always
  act on whatever's between the marks.
- Editor verbs like *extract* and *wrap* fall out as compositions
  involving `swap`.
- Summary algebras don't change; the circle is a positional concept,
  not an algebraic one. A pinned "origin" mark recovers the linear
  text case from the loop.

The mathematical claim is that this unifies the editing algebra under
a small Z/2 action (the arc-swap), with no special cases for point
vs. selection.

## Status

Sketch only. See `001-circular-two-marks/` for the developing project.
Other future entries may appear alongside as we explore variants or
adjacent directions.
