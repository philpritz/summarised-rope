# Discussion — 2026-06-16 — editing vocabulary — with Claude

Cursor/edit verbs become plain `vector -> vector` functions over the guide vector
(`slot-guide` now a `guide` struct carrying its index), through `zipper-guide`'s
*existing* faces — +`at` +`both` +`slot` +`lift` +`guide-index`, -`pure`,
`move`/`spread` reshaped `z->z` → `vector->vector` — with crossing failing fast
(provisional `carve`/`toward` guards) and anchor flips (`re-anchor`/`cover`) left
as `z->z` (open). 1239 green, uncommitted.

---

# Discussion — 2026-06-16 (1) — with Claude

A **mixed session** — two landings under a design arc, tightening the boundary
between the sexp summary and the navigation layer.

- **Moved the sexp summary's read boundary into `summaries.rkt`.** `cut-kind` +
  `sand-spines` relocated from `sexp-edit`; the sexp surface narrowed to
  `sexp-smr` + `sand-spines`. The `frontier` struct, the leaf/combine, and the
  field readers (`sexp-opens`/`-closes`/`-forms`/`-head`/`-tail`) went internal;
  four dead edge predicates (`sexp-starts/ends-atom?/form?`) deleted; `bench`
  re-pointed off `sexp-forms` (it reads the form count off `sand-spines` now).
  *Chosen over* also moving the spine algebra — only the cut→spines bridge belongs
  with the summary; the spine arithmetic is summary-agnostic and stays in
  `sexp-edit`.

- **Extracted `lexicographic` into `helper-algebras`.** A generic, curried
  first-difference 3-way order — `((lexicographic cmp) l1 l2) -> {-1,0,1}` —
  beside `on`/`iso`/`fixed`. `spine-cmp` shrank to a one-liner: zip the two
  co-indexed spines into one `(front . back)` cut, reverse to outermost-first, and
  defer to `(lexicographic cut-cmp)`. Fell away: the SRFI-41 streams,
  `spine->stream`, the `-inf` padding, and the explicit `length`. *Early exit on
  the shorter spine* (a prefix is the lesser) replaced the `-inf` padding; named
  `lexicographic` over `lex`, which reads as "lexer" in a tokenizing codebase.

- **Cut element = plain `(front . back)` pair**, *chosen over* `(slot . modulus)`:
  the modulus (`front − back`) is derived on demand by `flip`/`base-*`, not stored.

- **Explored and set aside:** replacing the ½-refinement (`sand-spines`' head
  lean) with `(before, after)` integer-boundary pairs. The ½ keeps
  `front − back = N+1` (the per-level modulus) uniform across *every* cut kind,
  which `flip`/re-basing depend on; the neighbour-pair names different boundaries
  when the cut is between two, losing that uniformity. The ½ stays.

## Status

- Complete and landed: all of the above. 1239 tests green; `bench` compiles.
  Working tree, uncommitted.
- A separate, in-flight command-vocabulary redesign in `sexp-edit` (the `guide`
  struct carrying its own index; verbs `at`/`move`/`spread`/`both`/`slot`/`lift`)
  sat on the working tree alongside this work and was left untouched.
- Open thread: `sexp-edit`'s header paragraph still describes the old
  `-inf`-padded comparison ("pad with -inf … first non-zero componentwise
  verdict") — stale after the `lexicographic` move, not yet updated.

## For the next session

This note is short, so **append your session's summary to the end of this file**
rather than opening a new dated note.
