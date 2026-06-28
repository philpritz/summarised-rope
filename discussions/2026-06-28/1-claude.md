# 2026-06-28 — sexp depth-split lens + list-style focus editing

**Mixed session**; the substance is the sexp split optic and the editing method it opens.

## The sexp split lens (`sexp-split.rkt`)
- **`split-guide*` / `split-iso` / `focus-split`** — split the cursor's focus into s-expression pieces at a **depth knob**: `(focus-split 0)` = top-level forms, `(focus-split +inf.0)` = all the way to atoms, between = bounded depth. Spine-based — uses `sand-spines`' **½-refinement** to tell a clean cut from inside an atom or whitespace, and the spine length for the depth. Replaced an earlier internals-poking `forms-guide*` (dropped).
  - `(define (f x) (+ x 1))`: depth 0 → `("(define (f x) (+ x 1))")`; depth 1 → `("(" "define " "(f x) " "(+ x 1)" ")")`; depth ∞ → atoms.

## New editing method — the focus as a list
- `focus-split`/`focus-lines` turn the focus into a **list of pieces** edited with **ordinary list operations**; the iso's join + the zipper's put rebuild and re-navigate. Structural editing, no bespoke commands:
  - `(updater (focus-split 0) reverse)` — reorder top-level forms.
  - `(updater (focus-split 0) (λ (fs) (remove (second fs) fs)))` — delete a form.
  - `append`/`take`/`drop` to insert, `map` to rewrite in place, `(compose (focus-split d) (lref i))` to single out one piece.
  - verified: `"(a)(b)(c)"` → delete 2nd → `"(a)(c)"`, reverse → `"(c)(b)(a)"`. Same shape gives line editing via `focus-lines`.

## Secondary
- **`edit-sexp`/`modify-focus`** (scratch): rewrite the focus as plain s-expressions. A **transformation, not a lens** — read/print is lossy (fails GetPut); kept as `rope→rope` through the lawful `zipper-focus`.
- **`iso->lens`** added to `helper-algebras`. 942 tests green.
- Parked: a whitespace-exact tree rep for lossless edit-as-data; pruning the split descent.
