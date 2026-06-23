# Discussion — 2026-06-21 (2) — zipper-core lens surface — with Claude

An **implementation session** (design thread on lenses): zipper-core's two three-faced accessors are now lenses over a lens algebra in `helper-algebras`.

**Landed:**
- `helper-algebras`: a lens = store-coalgebra `s -> (values focus put)` (`iso`'s partial sibling); curried ops `viewer` / `setter` / `updater` + `compose-lens`.
- `zipper-core`: `zipper-guide` / `zipper-focus` are now lenses (the put re-navigates via the lift); call sites swept across zipper-core / sexp-edit / bench / char-edit. 567 tests green.

**Forks:**
- **Curried over uncurried** — `(setter l x)` is a `z -> z` command, so writes compose (`delete = (setter zipper-focus "")`, no lambdas in compose-chains); uncurried `(lens-set l x z)` reads cleaner but forced `(lambda (z) …)` at every command site.
- **`viewer` / `setter` / `updater` over `lens-view` / `lens-set` / `lens-over`** — shorter, read naturally curried; over the self-documenting `lens-*` prefix. Lenses kept `zipper-guide` / `zipper-focus` (over `guide-lens` / `focus-lens`, which doubled "lens").

**Parked:** a `store` struct reifying the peek (the engine destructures it on the spot, so `values` suffices).
