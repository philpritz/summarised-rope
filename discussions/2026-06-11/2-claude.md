# Discussion — 2026-06-11 (2) — with Claude

An **implementation session**: a pretty printer that became the zipper's default
printing, then a compression pass over the editing layer that reshaped the
surface to five names. Suite green (**463**).

## Printing (landed)

A zipper prints as its document with the cursor marked inline -- gap `‸`, seg
`⟦…⟧` -- via `prop:custom-write` on the struct, exactly as ropes print as their
text; the REPL narrates editing sequences for free.

- **Position by re-cutting**: the printer re-cuts the root with the installed
  guides (`multisect`). This *leans on the guide--focus alignment* (noted in the
  code comment; the user wants the lean visible and not multiplied). The
  guide-free alternative -- reconstructing before·focus·after off the crumbs --
  was sketched (crumb-as-struct; crumbs as two-faced arity-dispatched closures;
  head-as-three-ropes dissolving the stack) and **parked**: the user wanted
  neither layer touched for a debug aid.
- Underline-row rendering rejected (fails multi-line); inline marks chosen.
- A `step` filmstrip helper landed, then was **cut**: presentation, not zipper
  algebra -- the REPL already prints each step.
- Cost accepted: custom-write hides the transparent struct fields.

## The editing layer compressed (user-led)

Surface now **five names**: `start guide focus to-root on-edges`.

- **`zipper-lift` navigates unconditionally** -- `navigate` (= carve . descend .
  ascend as ONE op) is composed in as the permanent last op; `(zipper-lift)`
  bare is plain re-navigation. The invariant moved from per-verb discipline to
  structure: every write navigates because every write is a lift. `renavigate`
  dissolved. `to-root` is the one motion *outside* the lift (homing must not
  navigate back down); it folds crumbs directly.
- **`focus`, the editing accessor** -- three-faced twin of `guide` (read | swap |
  transform), the user's extension of a read-only `focus` proposal. It subsumes
  `replace` (deleted, not aliased; delete = `((focus "") z)`); `over*`/
  `replace*`/`to-root*` dissolved into their callers. The modify faces cost one
  navigation, at the outermost face, unchanged.
- **Faces as internal definitions returning a `match-lambda`** -- over
  `define/match` (no body prelude for local definitions) and over self-applying
  one-liner equations (correct but too dense). Staging via `curry`
  (racket/function): verified it fires exactly at full arity (the hazard is
  variadic functions, which fire early -- never curry the smr). SRFI 232 is the
  syntactic analogue, not bundled with Racket. `guide`'s arity guard dissolved
  into the `(vector _ _)` pattern (a match error replaces the bespoke message).
- **`on-edges` replaces `peek`** -- the cursor's two edges as cuts, spread over
  per-edge functions, combined (the spread-combine shape; Haskell's `on` is the
  same-function special case). Generic over the zipper's own smr, so
  `edge-contexts` stopped naming the sexp algebra; the 06-10 parked `edges`
  idea, landed in combinator form. `peek` (briefly `zipper-peek`) deleted: with
  `focus` covering the cursor reads, its one consumer was `edge-contexts`.
- **`sexp` renamed `sexp-smr`** (the smr, distinct from the `sexp-*` readers).

## Supersedes

From 06-11/1: the five-name surface (`peek` gone, `focus`/`on-edges` in,
`replace` folded into `focus`); from 06-10/1: `replace` as the one editing verb.

## Status

Built & green: **463** across the four files. README now stale on: `replace`/
`peek`, `(make-rope sexp)`, the five-name list, the test count. Nothing
committed this session. Conventions gained the "let's have X" clause (landed)
and the two-kinds-of-session section (landed alongside this note).
