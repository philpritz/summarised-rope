# Discussion — 2026-06-19 (3) — zipper-core fixed-point pare-down — with Claude

An **implementation session**: the navigation machine in `zipper-core.rkt` re-expressed through a shared `fixed`-point combinator and an `arg` projection, then `navigate`'s stages internalized. **1484 tests** green; bench compiles. Authored by Claude, directed at a low level by the user.

**Landed:**
- **`fixed`** (`helper-algebras`) reworked `[good-enough?]` → `[same? equal?] [key list]`: halt is an equality over a projection (Racket's `remove-duplicates` `#:key` shape), `key` applied to the value-tuple *as arguments*. First real caller.
- **`arg`** added to `helper-algebras` — the projection / K combinator (`(arg 0)` = Haskell `const` for two args); single-walk `take` + `list->vector`.
- **`ascend`/`descend`** are now `(fixed step eq? (arg 0))` — fixpoints of the self-halting steps `rise`/`toward`; `contains?` folded into `rise` (its no-op is ascend's halt).
- **Crossing guard** — two provisional twins (in `toward` and `carve`) collapsed into one `uncrossed` stage in `navigate`, after `ascend`.
- **`navigate`** absorbs its four stages (`ascend`/`descend`/`uncrossed`/`carve`) as locals; `rise`/`toward` stay top-level. Header op-list refreshed.
- **`deprecated-5/zipper-core.rkt`** — pre-pare-down snapshot.

**Forks decided:**
- **`fixed` stop-test = equality-over-projection, positional not keyword.** Over (A) an arbitrary whole-tuple predicate — rejected: bare `eq?` compares the fresh wrapper list (infinite loop), forcing `(on eq? car)`; and (B) element-wise `andmap` equality — rejected: loses derived-convergence. Positional over `#:key`: the keyword's only edge (set `key`, keep default `same?`) never applies since every caller overrides `same?`, and the file defines no keyword params.
- **`arg` projects the values directly** (not `car` on the listed tuple) — kills the wrapper footgun. **Single-walk `take`** over full `list->vector` (pessimizes the hot `(arg 0)` to O(n)) and `map`+`list-ref` (O(k·n)). **Raw lambda** over `cut` (no `srfi/26` dependency). Modulo/negative indexing **dropped**.
- **One `uncrossed` stage** over the twins: the guard is input-validation only (never fires for a well-ordered cursor), so a single catch-all after `ascend` suffices; `toward`'s partial early-catch was redundant.
- **`navigate` stages internalized** over top-level defs — tidier single definition, at the cost of independent testability and the header listing.

**Status:** pare-down landed, 1484 green. (`helper-algebras`/`zipper-core` match HEAD; the outstanding uncommitted diff is the separate rope-core rewrite.)

---

## Addendum — 2026-06-20 — scribble docs; `chain` → internal

- **Scribble docs under `scribble/`** (standalone `.scrbl` per source file, rope-core house style; conventions in `scribble-discussion-conventions.md`): landed `rope-core.scrbl` and `zipper-core.scrbl`. Each source file gains a `(module+ internal)` dev bucket reached via `(require (submod "….rkt" internal))` — rope-core re-exports structural internals (`rope-leaves`, `leaf?`, …) to back examples; zipper-core holds the editing traces.
- **`chain`/`run-chain` → zipper-core's `internal`** (they print a trace, so dev tooling not the navigation/editing API). The surface is now the five ops `start`/`to-root`/`zipper-guide`/`zipper-focus`/`on-edges`; sexp-edit's `(all-from-out)` drops `chain`; the doc puts it in a "Tracing (dev)" section. Named `internal` over a distinct `dev`/`trace` — a general bucket for later (*parked:* keeping `chain` public as a REPL convenience). *Finding:* `chain` bypasses the `run-chain` contract (it expands to the module-internal `run-chain`), so `cmd/c` guards direct calls only — corrects the old provide comment, and makes public-vs-internal a signalling call, not a safety one.
- **Tests:** green.
