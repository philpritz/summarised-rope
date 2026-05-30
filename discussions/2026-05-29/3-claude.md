# Discussion — 2026-05-29 (3) — with Claude

Planning session for the zipper rework on top of the variadic rope (continuing
`2026-05-29/2-claude.md`). Pure design for the zipper layer plus a round of
small rope-core hygiene (renames). No zipper code yet — the goal was to settle
the **shape of the surface** before writing, so the window/display requirement
wouldn't corner us later.

## Public zipper API: five (six) verbs

The ~20-function old zipper surface collapses to:

```
start     rope -> cursor
navigate  cursor guide -> cursor       ; move + select + reshape
insert    cursor content -> cursor     ; edit  (subsumes replace)
delete    cursor -> cursor             ; edit  (= insert empty)
view      cursor window-guide -> text  ; windowed read
text      cursor -> string             ; whole-doc / selection read
```

`get`/`put` are internal mechanism, not exposed — keeping them private is what
protects the anchor invariant (a public `put` could move the anchors or install
a malformed head). `replace` folds into `insert` because `edit = set-the-middle`
covers both (typing with a selection = replace; typing at a point = insert).
`delete = insert empty`. `move/update-index` dissolves because the guide is now
an argument to `navigate`, not stored on the cursor.

## Navigate decomposes to three structural verbs

```
rise    : shed useless ancestors        — pop crumb, join sibling via roper
narrow  : shed useless descendant-sibs  — push crumb, drop whole sibling
carve   : k-splitter on the local node  — produce the new head(s)

navigate = carve ∘ narrow ∘ rise
```

- **`rise`** is one verb gated by a **containment test** at the head's two
  edges:
  ```
  contained ⟺  guide(before, smr(head, after))     >= 0
            AND guide(smr(before, head), after)     <= 0
  ```
  Replaces `up` + every directional `open-*` rise.
- **`narrow`** is the mirror of rise: at each `(branch L R)` ask the guide "can
  I drop a whole child?" Stop when neither side can drop. Purely structural,
  guide-edge reads only — no leaf bisection, no rejoining.
- **`carve`** is the only fine step (bisect leaves, rejoin within the region).
  It runs once on the tightest node that narrow handed it. Gap and seg unify
  here — the guide decides which `k-splitter` arity.

The old `open-split-*`, `open-seg-*`, `open-straddling` all collapse into
`narrow` (structural descent) + `carve` (one k-splitter call). The plain
`open-left`/`open-right` (which cracked rope nodes by hand) disappear — that's
exactly what `splitter` / `rope-splitter` now expose.

## Editing reduces to one mechanism

Taking **gap ≡ seg with empty middle** (the 2026-05-28 anchor contract) as
central:

```
edit-head  : (head -> head) -> zipper   ; in-place: set the middle, anchors fixed

insert  z content   = edit-head replacing middle with content
delete  z           = edit-head replacing middle with ∅   (= insert empty)
```

- Generalized `gap->seg` / `seg->gap` HOFs are **dead**: every call site feeds a
  trivial callback that never re-splits `l` or `r`. Drop both.
- `left-bound-gap` / `right-bound-gap` aren't edits — they're cursor reshapes
  (collapse selection to one edge). They fold into `navigate`, no separate verb.
- Anchors-fixed *is* the safety contract: editing the middle leaves
  before/after summaries untouched, so the cursor doesn't drift.

## The splitter family generalizes — and goes variadic+continuation

```
n=1   rope-splitter   guide in [-1, 1]   ->  2 pieces
n=2   seg-splitter    guide in [-2, 2]   ->  3 pieces
n=k   k-splitter      guide in [-k, k]   ->  k+1 pieces
```

The general `k-splitter` is the only splitter exported. Input variadic, output
via a **continuation handler** (because Racket `values` is awkward for variable
arity; this matches the existing `splitter`'s on-l/on-r/on-here pattern):

```racket
((k-splitter guide on-pieces)  before  t1 … tk  after)
;; on-pieces receives:        (before' s1 … sj after')  — symmetric with input
```

The continuation always receives `before' … after'`, even though `k-splitter`
itself preserves them. The uniform shape lets *other* operations of the same
form — narrowing lenses — pass *new* before/after where the totals shift.
"Variable totals" is a per-operation property, not a per-shape one.

## Head lives in the zipper, not in rope-core

Initial plan was to put a `head` struct in rope-core (since `k-splitter` takes
that shape). Reversed: **rope-core stays a pure rope library** with raw
variadic+continuation `k-splitter`; the zipper owns the head abstraction at its
side of the boundary. Matches discussion-2's generic-vs-specific cut.

```racket
;; zipper-core
(head    before piece … after)                 ; variadic constructor
(head-with  h  (lambda (before piece … after) …))  ; continuation deconstructor
```

`head-with` works with `match-lambda*` for variable-k destructuring:

```racket
(head-with h
  (match-lambda*
    [(list before pieces … after)  …]))
```

Predicate / individual accessors stay internal to the zipper — all reads flow
through `head-with`.

**Resulting rope-core public surface: 4 exports**

```
summariser  roper  rope->string  k-splitter
```

## Crumbs as lenses

Each narrowing step is a **lens** = (forward, repair):

```
forward  :  lsum  t1 … tk  rsum   →  lsum* s1 … sj rsum*
repair   :  lsum* s1' … sj' rsum*  →  lsum  t1' … tm' rsum
```

- The original `lsum`/`rsum` are restored by the repair — captured as the
  crumb's stash (monoid is still non-invertible from focused state).
- **`k-splitter` is the trivial-repair lens instance** — outer sums preserved,
  repair just unsplits. It doesn't need to carry its repair around.
- **Edits propagate automatically.** When you `edit-head` the focused head, the
  next `rise` calls the top repair on the *current* (edited) child — and the
  parent it produces contains your edit by construction. No explicit lift step.
- `crumbs` is a stack of repair functions; `rise` = pop & apply.

Two zipper primitives, mirroring the genuine asymmetry between focus-shifting
and in-place ops:

```
edit-head   z (head -> head) -> zipper        ; in-place: lens stack untouched
apply-lens  z lens           -> zipper        ; focus-shifting: push repair
```

Specific narrowings (`lens-by-guide`, `lens-pick-piece`, "first half of t1",
etc.) are small constructors returning a `lens` record, then handed to
`apply-lens`. The general "transform-tree" combinator *is* `apply-lens`; the
generality lives in what the lens does.

## Reads — `view` and `text`

**Reads are navigate's read-only twin**: same carving, return text instead of
moving the cursor.

```
text  = rise-to-root + rope->string of pieces
view  = rise-to-root + k-splitter with a window-guide → middle's text
```

The window is just a `seg`-shape extraction by a *display measure* (lines /
chars / visual width). So window extraction needs no new structural machinery —
it reuses `k-splitter`.

**The one upstream requirement: display dimensions live in the summary.** A
product monoid: navigation guides read the structural fields, the window-guide
reads the line/char fields, off the *same* cached summary. This constrains the
S-specific algebra module, not the zipper.

## Rope-core renames (implemented this session)

Continuing the variadic style and clarifying which things are factories:

- `summary-algebra` → **`summariser`** (factory: returns the `summary` fn)
- `rope` (builder) → **`roper`** (factory: `((roper smr) part …)`)
- `split` / `split-rope` / `seg-split` → **`splitter`** /
  **`rope-splitter`** / **`seg-splitter`** (factories carry `-er` suffix)
- Threaded summary-fn parameters renamed `sys` → **`smr`**; the minted function
  inside `summariser` keeps the canonical name `summary`. (`smr` distinguishes
  the *passed handle* from the canonical function.)
- `rope-summary` (cached value) / `rope-algebra` (the fn) accessors kept —
  useful disambiguation.
- 26 tests passing under the new names.

Under the design above, the splitter trio will collapse to a single
`k-splitter` (variadic+continuation). **Not yet implemented.**

## Conventions added (implemented this session)

In `discussions/conventions.md`:

- **Questions are questions, not instructions.** Answer the question; do not
  infer a plan, design decision, or go-ahead. *Above all, do not close off
  design avenues unilaterally* — laying out options is the assistant's job;
  ruling them out is the user's. Exception: questions probing the assistant's
  own work can be acted on if the choice is unjustified.
- **"Draft" means draft in chat.** Drafting is collaborative — show inline,
  revise, write into a file only on explicit sign-off. "Add this" / "put this
  in" referring to concrete content is a direct go-ahead. **Sign-off must be
  explicit and affirmative** ("write it", "add it"); a request to change the
  draft is a new revision round, not approval. **"maybe X"** depends on state:
  keep revising if still in chat, apply-by-default if already in the file
  (unless serious objection), answer rather than apply if it carries a question.

## Status

**Implemented and on disk** (this session, uncommitted on master at planning time):

- `rope-core.rkt` — `summariser` / `roper` / `splitter` / `rope-splitter` /
  `seg-splitter` / `smr`. 26 tests passing.
- `deprecated-3/README.md` — appended the pre-rewrite root README for reference.
- `discussions/conventions.md` — the two convention additions above.

**Pure design, not implemented:**

- The whole zipper rework: 5/6 public verbs, `rise`/`narrow`/`carve`,
  `edit-head`/`apply-lens`, head + crumbs-as-lenses, `view`/`text` as
  navigate's read-only twin.
- The single `k-splitter` collapsing `splitter` / `rope-splitter` /
  `seg-splitter`.

**Open / parked:**

- **`smr` recovery** — how zipper ops get the summary fn back when combining
  summaries. Three options still in play: expose `rope-algebra`, carry `smr`
  as a zipper field, or pass as an argument.
- **The narrow algorithm** — per-level piece-selection turning a guide into a
  specific lens.
- **Lens constructors** — `lens-by-guide`, `lens-pick-piece`, etc.
- **The S-specific module's shape** — concrete algebra (frontier monoid +
  display dims) and its guides. Needed before `view`'s window-guide can exist.
- **Balancing** — wholly hidden in rope-core; still the right-leaning fold for
  now (deferred per 2026-05-28/2).
