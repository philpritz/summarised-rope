# Discussion — 2026-06-15 (3) — with Claude

A **design / exploration session**. No code landed; the idea is the artifact: *how a display layer attaches to the summarised rope.*

- **Display information is a deque-valued summary.** Render the screen as a cached summary on the rope whose **value is a lazy catenable deque of rows, each row a deque of cells** (a deque of deques: outer = rows, inner = cells). The value at every node is that subtree's laid-out fragment; the root's value is the whole screen, maintained incrementally — the "rendered content as a cached node value" idea taken to an actual sequence rather than a scalar measure.

- **It is the monoid homomorphism `string -> deque`.** Each char maps to a small deque-fragment; the combine is deque concat; built lazily. Lawful by construction (the summary battery holds: concat is associative, empty deque the identity). As `make-summary` with the per-char map `h`:
  ```
  h(c)    = [[c]]        ; ordinary char: one row, one cell
  h('\n') = [[], []]     ; newline: close this row, open a new one
  A ++ B  = A.init ++ [A.last ++ B.head] ++ B.tail   ; fuse at the seam
  ```
  **Line-splitting falls out of `h('\n')`** — not a separate pass. The left fragment's last row fuses with the right's first row, touching only the two boundary rows.

- **Lands in existing vocabulary:** combine = `concat`; split-by-measure = a **guide** / `multisect`; the deque's two ends + a focus = the **`head`** (`before . focus . after`), in 2-D.

- **`string → deque` alone is too weak — highlighting needs the surrounding context.** A cell's *style* (inside a string? a comment? the head atom of a form? at what depth?) is not a function of its char alone; it depends on the ambient context. So the per-char map can't be context-free: each cell must read the **frontier** already cached around it (open/close stacks, forms, head/tail char classes). The display map is `char · context → styled cell`, in the spirit of `frame` (bake outer context in) — not the bare homomorphism. **This is what we work out next.**

## Status

- Unimplemented sketch — design only, nothing signed off. The structure that realizes the deque (and whether it is *cached per node* or *lazily rendered per viewport*) is left open.
