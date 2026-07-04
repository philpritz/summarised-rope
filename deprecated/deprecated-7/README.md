# deprecated-7

**Per-file pre-change snapshot, not a whole generation** — like `deprecated-5`/`-6`,
this copies the live file just before its own change.

Kept as the pre-replacement reference for the **van Laarhoven lens layer**:
`make-lens` (the store-coalgebra constructor), the `const-box` view tag, the curried
ops `viewer`/`setter`/`updater`, `iso->lens`, and the derived lenses `list-of` /
`lref` / `ldiag` / `varg` / `vdiag` — composed with plain `compose`, one body
serving view and set with the foci handler `k` picking the functor. Replaced by the
record optics (`opt`, with `get`/`set`/`f` fields and the accessors as the ops) that
carry read-only context on the values channel; see the live `helper-algebras.rkt`.

- `helper-algebras.rkt` — the whole helper library as it stood with the lens layer
  (isos, lenses, combinators, lockstep, the experimental submodule). **521 lines**
  (with tests). Self-contained — no requires — so it runs standalone here:
  `raco test` gives 94 (main) plus the experimental submodule's nested tests.
