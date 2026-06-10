# Discussion — 2026-06-10 — with Claude

Cleanup session, built and green (100 tests). Much of it was style and shape work on
`rope-core` / `zipper-core` (gallery idiom, currying, naming, surface trims) whose
outcomes are evident from the files; recorded here are the forks.

## Decisions

- **Context lives in the guide.** `frame` bakes `b`/`a` into a guide (user's design).
  Over a keyword frame on the splitters (context would recur on every signature) and
  over framing at splitter build time (in the zipper, guides are fixed per navigate
  but context varies per head).
- **`multisect` is the one split export** — no guides = the balance halve, so it
  subsumes `bisect`; pieces as values. `bisect`/`leaf?` went internal (one external
  caller each); an interim `halve` export was superseded by the no-guides default.
- **One op protocol for the zipper** — `config -> smr -> (head stack -> values head
  stack)` — with `zipper-lift` (user's proposal) distributing smr and resealing: the
  only place a zipper is opened; structs stay unexported.
- **Atomic halt = an empty half, on EITHER side.** The user's constraint: halve must
  stay ambiguous about which side of an atom the empty lands; a one-sided check would
  lean on `split-leaf`'s rounding across the module boundary. A direct rope/head test
  would re-expose `leaf?` in disguise — ruled out.
- **Surface**: eight names (`start navigate to-root replace insert delete peek`).
  `wrap` dropped (expressible in `replace`), `insert` kept (the more basic function),
  `over` unexported (muddles the vocabulary), read-outs collapsed to `peek`.

## Parked

- **`edges`** — guides × head -> the two edge reads; would cover every machine read
  (`contains?` a sign test on it; a seam is a child head's edge). Parked for the
  current design.
- **Exporting `zipper-lift`** — a second consumer earns it. **README rewrite** —
  still describes the pre-rewrite API.

## Supersedes (dropped)

From 2026-06-09: the `return` continuation, `b t a` splitter args, the `leaf?` base
case, `(verb z config)` arg order, the read-out accessors, `wrap`/`over` exports.

## Status

Built & green: rope-core 32, sexp-summary 58 (unchanged), zipper-core 10; sexp-edit
updated and smoke-tested end-to-end. Nothing committed (sits with the 2026-06-09
work); sexp-edit still lacks a test submodule.
