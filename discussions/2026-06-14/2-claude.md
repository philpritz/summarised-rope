# Discussion — 2026-06-14 (2) — with Claude

A **mixed session**: the bulk is a design arc — structuring and drafting the Scribble manual (medium); the landings are short — a char-count summary system written as the §2 worked example, and `chain` moved into `zipper-core`. Nothing committed; all in the working tree.

## The manual (design — medium)

The documentation is three sections: **1** general use (driving the shipped sexp system to build an editing sequence), **2** defining a custom summary system, **3** internals. A fixed `scribl/brief.md` states each section's goal as a numbered arc; the `.scrbl` files implement it and are the revisable artifacts — the brief guides them, not the reverse.

Decisions:

- **One central example per section, sequenced, with *what / how / why* at each step.** §1 carries a single document (`(aa (p q) cc)`) through placement, reading, editing, movement. The flip/stability arc is the heart, and it is taught **problem-first**: the reader inserts at a plain cursor, watches it *drift* (the caret slips off the spot as the form count shifts), and only then reaches for `cover`/`flip` as the answer — not a feature on a list. *Alternative rejected:* explaining families up front, which presents the fix before the problem is felt.
- **Evaluated examples** (`scribble/example`), so the self-printing cursor (`(aa ‸q) cc`) renders live. The printed marks are the whole charm; static blocks would go unverified. The example evaluator resolves the module require against the repo root (build from there).
- **§1 mechanism deferred to §3.** Flip's *why* (families) is §1; its *how* (modulus, signed spines) is §3. Keeps §1 usage-level.
- **§2 is build-then-use.** Construct a custom system end to end, then drive it — the inverse of §1's explain-as-you-go, fitting a more reference-like section. The construction code is shown statically; usage runs live against the file.
- **Example modules live in `scribl/`, not the repo root** — they are doc scaffolding, so they stay off the orientation import list in `CLAUDE.md`.

Landed (drafts, rendered, examples evaluate): `scribl/general-use.scrbl` (§1), `scribl/custom-systems.scrbl` (§2), `scribl/brief.md`, plus the `scribl/doc-html/` renders. §3 unwritten.

## The char-count system (implementation — short)

`scribl/char-edit.rkt` — a second instantiation of the summary/cursor machinery, as the §2 example. A position is a character offset; the system is **four atoms** — the summary (`(make-summary string-length +)`), the guide, the family split `back?`, and `read-cut` (cut → its two anchors) — and everything else (placement, cover, navigation) is thin `zipper-guide`/`on-edges` usage.

The forks that shaped it:

- **The index *is* the guide**, via `prop:procedure` (a struct carrying the index data, callable as the comparator). *Considered:* keep index and guide separate, bridged by an extractor (or get/set lens). *Why rejected as separate:* the lens needs the index in a *field*, so the guide must be a struct, and `zipper-core` *calls* guides, so that struct needs `prop:procedure` anyway — the two options converge. Option 1 is the representation; the "lens" is just its accessor/constructor. Mirrors the `iso` struct already in `helper-algebras.rkt`.
- **`cover` reads both anchors fresh and installs the back one** (sexp's own route in `anchors`/`re-anchor`), *not* re-basing a held index by the modulus. *Why:* the modulus / `anchor-left` / `anchor-right` arithmetic is only needed for a standalone `flip` (the other family of an index you hold *without* re-reading the cut); `cover` re-reads the cut, so it just picks. This cut `cover` from ~12 lines to ~3.
- **No new core abstraction was added.** Explored a generic `edit-edge` transform and a `make-cursor-layer` constructor taking the four atoms. *Concluded unnecessary:* `zipper-guide`'s modify face already installs-and-re-navigates, and `on-edges` already reads the cut, so every cursor op is just those two composed — `edit-edge` needs no new export. So the char ops are written straight on `zipper-guide`; the layer/constructor were dropped. (Parked, not dead: a shared layer would only earn its keep once several systems want to share the placement/cover boilerplate.)

## `chain` → `zipper-core` (implementation — short)

`run-chain` + the `chain` macro moved from `sexp-edit.rkt` to `zipper-core.rkt` (exported), re-exported through `sexp-edit` via `(all-from-out …)` so every use still resolves. It is summary-agnostic — prints a zipper via its `prop:custom-write`, applies `zipper -> zipper` commands — so it is general editing machinery, and it sits next to `zipper-show` as its multi-step sibling. The same `chain` now traces the char session unchanged. **1218 tests green** (unchanged count; `chain` isn't itself tested, so the re-export was checked by hand).

## Uncurrying the machine ops (implementation — short)

In parallel with the above: `zipper-core`'s machine ops were uncurried from `((op guides) smr)` to a single level `(op smr guides)`, since both outer layers are ambient for a whole lift run. The struct now leads with the pair (`(zipper smr guides head stack)`) so the lift reseals via `(curry zipper smr gs)` and a `pass` thrush — `srfi/26` drops out. 1218 tests green.

## Status

- Working tree only, **uncommitted**: the four `scribl/` artifacts, the `chain` move in `zipper-core.rkt` + `sexp-edit.rkt`, and the machine-op uncurrying in `zipper-core.rkt`.
- 1218 tests green.
- §1 and §2 are rendered drafts whose examples evaluate live; §3 unwritten.

## Open / parked

- **§2 draft loose ends:** construction code shown statically (`@racketblock`) is not pinned to `char-edit.rkt`; the shown `(require "rope-core.rkt" …)` omits the `../` the `scribl/`-dir file actually needs.
- **A shared cursor layer** (`make-cursor-layer` / a reusable `edit-edge`) — explored, parked; returns only if multiple summary systems want the boilerplate shared.
- **Fold the sexp system onto the `cover`-via-`anchors` shape** — not done; would confirm the char pattern carries to the real case.
- **§3 internals** — unwritten.

## Performance bench (implementation — short)

*A parallel strand: a timing harness in `bench/bench.rkt` (off the import list, like `scribl/`), plus a generator extraction in `sexp-edit.rkt`. No core behaviour changed; `sexp-edit` green (501 tests).*

`measure`/`bench` warm up, run K trials, report min/median per call — **reporting only, no assertions** (counts/laws were the assertable alternative; the ask was a reporting tool). The load-bearing choice is the **gen/op split**: inputs built untimed, only the op timed — what lets nav read flat against build's linear. Confirms build O(N); split/nav flat; sexp nav up to ~73k chars ~0.4 ms; `to-root` ~0.02 ms (no guide reads).

**Generators → a `gen` submodule** of `sexp-edit.rkt`, so the bench imports them without running the test suite or adding a file; `gen:resize` pins the axis to (max-kids, depth) not rackcheck's `size`.

**Deep DAG:** both children of every branch the same object (`r_{k+1} = (branch r_k r_k)`) — `2^d` copies of base in O(d) nodes. At `d=5000` the doc is `12·2⁵⁰⁰⁰` forms (1507-digit count) but `size`/`forms` are d-bit **bignums**, so we **count guide calls** instead of timing: navigating to the middle form costs exactly **`d + 11`** — clean O(d) = O(log N). The one place counting beat timing.

**Status.** Six suites, runs end to end. Working tree only, **uncommitted**; the `gen` submodule is the only core change.
