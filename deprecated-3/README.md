# deprecated-3

Pre-rewrite `rope-core.rkt` and `zipper-core.rkt`, kept as reference for the
2026-05-29 variadic rope rewrite (see `discussions/2026-05-29/2-claude.md`).

- `rope-core.rkt` — the struct-based summary algebra (`summary-algebra` with
  `empty`/`leaf`/`append` fields), plus `string->rope` / `concat-rope` /
  `split-rope` / `seg-split-rope` and the leaf-range machinery.
- `zipper-core.rkt` — the crumb-based zipper. To be rewritten in a later session
  on top of the new rope (crumb = `(side . sibling)`, rise = join).
