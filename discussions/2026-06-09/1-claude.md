# Discussion — 2026-06-09 — with Claude

The session that **built** the navigate+edit stack the prior sessions had only designed —
guided `bisect` + `multisect` (rope-core), the crumb zipper machine + editing verbs
(zipper-core), and sexp navigation/editing (new `sexp-edit.rkt`) — and then ran a long design
arc on the **address index** that is mostly *not* yet in the code. This note is careful to mark
which is which: **Parts A–C are built and tested; Parts D–F are design**, some of it only
sketched in chat (see "Sketched in chat, not in the code"). Design-led by the user; Claude wrote
the code and the lower-level choices.

Convention note: **the code uses the original comparator convention `+1` = the target boundary is
RIGHT of the cut** (`-1` left, `0` at it). The user raised the *flipped* (`+1` = left) reading
**this session** because they found it easier to read; it was not adopted in the code, **since the
ported sexp guides and the guided `bisect` already speak the original** — flipping would have
meant negating every guide. (Cosmetic; recorded only so the convention is unambiguous.)

## Part A — Built: the cut primitives (`rope-core.rkt`)

- **Guided `bisect`** — `bisect` is a `case-lambda`: `(bisect t)`/`(bisect t ge?)` keep the
  balance split; **`(bisect b t a g)`** descends a boundary edge reading `g` at each seam, and
  **binary-searches the straddling leaf** for the exact char where `g` flips. `smr` is recovered
  from the node. *Over* a separate `gbisect`: case-lambda keeps one name and the 32 tests intact.
- **`multisect`** — `(multisect guides)` → a splitter; apply to `(t return)` or `(b t a return)`;
  `for/fold`s one guided `bisect` per guide threading the before-summary `bAcc`, and calls
  `(return p0 … pn)` with the pieces. The **return continuation** is so the caller can pass back
  the various trees in whatever shape it wants — `values` for a 3-piece carve, `list`, a lambda.
- **`bisect`/`multisect` take `b t a`** — the user's reason: **for consistency**. The mechanism
  it buys: `bisect` frames the guide itself (`g (smr b left) (smr right a)`) as it descends,
  which let the old separate `edge->rope-guide` lifting step be folded away.

## Part B — Built: the zipper machine + editing (`zipper-core.rkt`, rewritten)

The crumb stack machine. `head = (before · focus · after)` + a crumb stack; ops thread
`(head stack) -> (values head stack)`.

- **`toward`** — four-column `match-let*-values`, with the **`leaf?` base case** (descend halts
  at a leaf; carve does within-leaf). A branch bisects into two non-empty halves, so the old
  emptiness `(equal? lt mt)` guards are gone.
- **`ascend`** rises until `contains?`; **`descend`** iterates `toward` to a fixpoint; **`carve`**
  cuts the focus via `multisect` through the `lens`.
- **`navigate`** = `(compose (cut zipper <> <> smr) carve descend ascend)`, guarded to 2 guides.
  **`(cut zipper <> <> smr)`** (SRFI-26) seals `(values h k)` into a zipper. *Over* a curried
  `((zipper smr) h k)` via `define-match-expander`: `cut` keeps `zipper` a plain struct so the
  user can put `smr` curried into the composition while `match`/accessors stay intact.
- **Editing verbs live here** — `insert`/`replace`/`delete`/`wrap`, `over`-based,
  `zipper -> zipper` so they chain; `to-root` folds back. `zipper-core` stays guide-agnostic.

## Part C — Built: sexp navigation + editing (`sexp-edit.rkt`, new)

`before-sexp-guide`/`after-sexp-guide` ported from the deprecated locator work (`opens`
reversed, since the current summary stores it innermost-first). They translate summary→sign in
two steps: `sexp-next-address` turns the LEFT total into the cut's address, then the guide
compares to the target and **refines the `=` case with the seam flags** (`starts-form?`/
`ends-form?`/atom flags) so the cut lands exactly at a form start/end, not in whitespace or
mid-atom. `index->guide`, `sexp-guides`, `cursor`, `carve`, `base-left`/`base-right`/`flip` are
built. (`sexp-summary.rkt` unchanged — the 7-tuple stays.) Demonstrated: navigate to a sexp
address, rename/delete/wrap/insert through the zipper.

## Part D — Settled design: representation, the unified index, the reversed sign

- **Guide = a comparator `(L R)->{-1,0,1}`; cursor = a 2-vector.** *Over* a fn returning a `rel`
  pair / two named functions / a `prop:procedure` struct: a **vector** is length-checkable
  **without applying it** (the navigate guard is `(= (vector-length guides) 2)`), and it brings
  the whole `vector-*` library.
- **Unified address `(cons (vector s e) rest)`**: gap = `s=e`, seg = `s<e`. One constructor.
- **The sign picks the anchor side, reversed this session.** `+p` → end of form `p-1` (a
  left-anchored index is sexp-END based), `−p` → start of form `p` (a right-anchored index is
  sexp-START based); `flip = negate`. The user's reason for the reversal: **so the guides don't
  cross** — flipping the anchor of the *second* edge must never land it before the first. Under
  the first attempt (`+p` = start) the end edge's flip stepped leftward, landing before the
  start edge whenever the span between them was empty; reversed, the end edge's flip steps
  rightward, so `start ≤ end` survives a single-edge flip (the well-formedness the rel pair of
  `2026-06-08/2` makes structural). *Built* in `index->guide` (and the file header matches).

## Part E — Settled design: the `½` fine position (and what of it is built)

Mid-atom, the cursor sits half-way through the atom, so the index is a half-integer:
`(foo ba^r)` reads `front 1.5 / back -1.5`, `(foo bar^)` reads `+2 / -1`. The modulus identity
`front − back = N+1` holds **uniformly** (both sum to 3 with `N=2`). This uniformity was the
user's **justifiability bar**: *they would only accept baking it in if the same formula adds up
whether or not you're within an atom* — the load-bearing dependency of the whole `½` thread.

- **The `½` is computed at the cut** (`ends-atom?(before) ∧ starts-atom?(after)`) — it needs both
  sides, so it **cannot be a stored summary field.** Proof on `"b"`: `"a"++"b"="ab"` (1 atom) and
  `"a "++"b"="a b"` (2 atoms) force `F("b")` opposite ways, but `"b"` can't see its neighbor, so
  no additive one-sided value exists. The `±0.5` only relabels the existing mid-atom correction.
- **Built consequence: keep the 7-tuple.** `starts-atom?`/`ends-atom?` are load-bearing (the
  monoid `combine` merges split atoms with them; the `½` derives from them). `starts-form?`/
  `ends-form?` are only the guide refinement and *could* be dropped → a 5-tuple; **deferred,
  7-tuple kept.**
- **NOT built:** the guide simplification this motivated — folding the `½` into the address read
  (`cut@`) so each guide is a plain `(- (cmp … target))` — was only sketched in chat. The live
  guides still use the flag-based `case` refinement (Part C).
- *Parked:* a char-count fine pin (`(structural . chars)`) is the genuinely additive way to store
  a fine position, but it commits to carrying char offsets everywhere (the char-vs-structural fork).

## Part F — Open: encoding A (modular slot) vs the fiber-edge sign

Referencing the last slot `dd^` showed the current sign-scheme is **not** the encoding A of
`2026-06-07/1` Part C, and does not support modular arithmetic. Encoding A (`N` forms = `N+1`
slots `0..N`): `front p ∈ 0..N`, `back = p−(N+1) = −(k_right+1)`; on `aa bb cc dd`, `^aa = +0 =
−5`, `dd^ = +4 = −1`. `front` and `back` are two reps of the **same** slot (`front − back = N+1`,
the user's "the right side is one more than its literal offset" — the `+1` is the modulus), so
slots form **ℤ/(N+1)ℤ**, `next/prev = ±1`, `flip = ±modulus` (position-preserving).

Our code instead uses the sign for the **fiber edge**: our `−1` = "start of form 1" (A's `−1` =
"after the last form"), and `flip = negate` *moves* the cursor across the fiber while A's
`flip = ±(N+1)` *keeps* the position. **The reconciliation (the next step):** these are two
orthogonal coordinates collapsed onto one sign — (1) which slot (encoding A, modular), (2) which
char-edge of that slot's fiber (the affinity, the `½`/a side). Make the index the modular slot
pointer; carry the affinity separately.

## Sketched in chat, not in the code

Recorded so they aren't mistaken for built:
- the `½`/`cut@` guide simplification (Part E) — live guides still use the flag refinement;
- the **shadowed `< = >`** (a binary type-dispatching `cmp` over a renamed `rkt<`) — `sexp-edit`
  still uses a plain `sexp-path-compare`. *Decided, if built:* don't export the shadowed ops
  (the user's condition);
- **`pass`/`guides@`** — discussed as the structure-application combinators; defined nowhere;
- **encoding A** for the index (Part F).

## Status

- **Built & green:** rope-core (guided `bisect` + `multisect`, **32**), `sexp-summary`
  (unchanged, **58**), zipper-core (machine + verbs, **9**), `sexp-edit` (compiles; navigate +
  edit a sexp demonstrated end-to-end).
- **Design only (this session):** the `½`/`cut@` guide form, shadowed `< = >`, `pass`/`guides@`,
  encoding A. See above.

## Open / parked

- **Encoding A for the index + splitting the affinity out (Part F)** — the main open thread.
- **Read the back index directly off the `closes` list (not `front − modulus`)** — the user's
  earlier instruction; **superseded** by the anchor-basis sign (`base-right = −k_left`), so the
  live code reads `k_left` and negates rather than reading `closes`. Parked unless encoding A
  revives it.
- **Drop `starts-form?`/`ends-form?` → 5-tuple** — deferred; 7-tuple kept.
- **Char-count fine pin** — the additive way to store a fine position; the char-vs-structural
  fork. Parked.
- **Flipped (`+1` = left) convention** — cosmetic; the user found it easier to read, not adopted
  because it would mean negating every ported guide.
- **Rejected:** a `fine@` helper (folds into the address read); a stored `±0.5` summary field
  (the additivity proof, Part E).
