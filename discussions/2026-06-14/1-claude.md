# Discussion — 2026-06-14 (1) — with Claude

A **mixed session**, three parallel threads landing together: (A) cleanups to the sexp summary value and spine layer, (B) a rename/comparator refactor plus a command vocabulary for the cursor, (C) a tree-first scaffolding for the index test — three document isos and a generic iso-law kit. The iso arc (thread C) is the durable design; the rest is implementation the diff carries. All suites green — **1218 tests** (from 1195).

## Thread A — sexp summary value + spine layer (`sexp-summary.rkt`, `sexp-edit.rkt`)

- **`frontier` edge flags → two symbols.** The four booleans (`starts/ends-atom?`/`form?`) collapse to a `head` and a `tail`, each a char class via `char-class`: `'atom | 'open | 'close | 'ws`. The struct now reads `(frontier head closes forms opens tail)` — left-to-right like the fragment; predicates rederived from the symbols, so `sexp-edit` and the tests were untouched.
  - *Over the booleans:* they conflated `)`/ws at a head and `(`/ws at a tail; the symbols keep all four distinct. Class named `'atom`, not `'word` (user's call).

- **`sexp-leaf` → regexp-tokenize fold.** `sexp-tokens` splits a fragment into parens and maximal atom runs (`#px"[()]|[^()\\s]+"`); the fold dispatches `open`/`atom`/`close`, with **atom-start and frame-close both `bump-sexp`** (register one form at the current level).
  - *Alternatives:* (a) a **megaparsack** parser — rejected: a dependency for a constantly-run ≤32-char measure, and it makes only the *three-region structure* declarative while the signed completion-counting (the real difficulty) stays equally remote. (b) a **char-scan fold with `in-atom?`** — fine, but the regexp gives one token per atom, dropping the flag and making atom/close symmetric. Chosen for "clarity and concision above all" (user).

- **`fine@` → `sand-spines`** (+ nomenclature). The `sand-spines` term previously named an unbuilt op, unreferenced in code and discussions, so repurposing it onto this cut-read was safe (user intended it for this job).

- **`cut-kind` + whitespace binds left.** One predicate classifies a cut from the two char-classes touching it — `tail` of L, `head` of R — into `start`/`end`/`mid`/`lean`; `sand-spines` is a `case` over it. **Whitespace binds to the previous form** (leans −½), so the end slot is the *close-adjacent* cut (`head = close`), not its trailing whitespace.
  - *Why:* whitespace belongs to the previous form (user); the two touching classes suffice to classify every cut. Subsumes the old `bh = -1` end-test, which had wrongly flushed whitespace-before-an-open — the `"^ ("` run-up ambiguity a prior session flagged; **fixed in passing**.
  - *Alternatives:* **bind-right / snap-to-next-form** — rejected: un-pins a form-start from its leading whitespace, needs lookahead. **Char-pin channel** (integer spine + char offset) — parked; the char-vs-structural fork.
  - *Cost accepted:* the end slot's trailing whitespace now lands tight before `)` (also avoids an insert fusing onto the previous atom). One test expectation updated.

## Thread B — accessor rename, comparator, command vocabulary (`zipper-core.rkt`, `sexp-edit.rkt`)

- **Accessor rename** `guide → zipper-guide`, `focus → zipper-focus`. Disambiguates the *accessors* from the two concepts sharing their names: a `guide` is the comparator `(L R) → {-1,0,1}`, the `focus` is the focused content. `zipper-guides`, `slot-guide`, `sexp-guides` untouched.
- **`component-cmp` to the ordinary convention** — `-1` if `a<b`, `+1` if `a>b` — with `pick-cmp`'s args swapped (`(component-cmp c …)`) so `spine-cmp`/`slot-guide` still read `+1 = target right of cut`. Behavior-preserving.
  - *One comparator, conventional:* rather than keep it guide-flavored (`+1` when `a<b`) and add a second, oppositely-signed comparator for index work — *two opposite comparators in one file is a trap*; the guide's sign is recovered by argument order in `pick-cmp`.
- **Command vocabulary** (new exported section): `pure`, `move`, `spread`, `chain`. A command is `zipper → zipper`; content edits go through `zipper-focus`, anchoring through `cover`.
  - **`spread` is two functions; numbers lifted with `pure`.** `(spread fl fr)` applies `fl`/`fr` to the two edges' innermost slots; a number is made absolute explicitly — `(pure n)`. *Reason (user): not polymorphic; use `pure` for lifting numbers.* Rejected: sniffing `procedure?` and treating a bare number as a delta.
  - **`cover` stays the anchoring verb** (end → right anchor), not a general `anchor 'left 'right`. *Reason (user): keep the word cover.*
  - **All content editing through `zipper-focus`** — no `insert`/`delete`/`wrap` aliases: `(zipper-focus "")` deletes, a function wraps, a string replaces. *Reason (user).*
  - **`chain` is a source-capturing macro** — threads a zipper through the commands, printing each command's *source* beside the zipper it produces (a function can't see the syntax). Parked: the plain-function variant returning the *trail* of intermediate zippers (scanl, for undo/inspection).
  - **Movement confined to the current sexp:** `spread` changes only the edges' innermost slots (`car`), leaving the path (`cdr`); `move` repositions by the whole index. *Reason (user): left/right stay in the same sexp.*

### Parked — index comparison

Comparing two indices by plain outermost-first lexicographic order (`index-cmp`, from a `lexicographic` combinator) is **only valid when both share an anchoring**. The two anchors of a position differ in the **head only** (the path is left-based either way); the head may be front-based (`≥ -1/2`) or back-based (`≤ -1`), the two related by the modulus — so a raw numeric compare across bases is meaningless. E.g. the gap before `bb` in `(aa bb cc)` is `(1 0)` left-anchored but `(-3 0)` right-anchored (modulus 4): same position, incomparable heads.

The fix is to **canonicalize first** — read the position's `front` via `anchors` (left-based in every component), or re-base a raw index with `(base-left (edge-modulus z i))` (head-only). Comparison was only needed for crossing-detection when *clamping* an edge against the other; `spread` is unclamped, so none of `index-cmp`/`lexicographic`/the clamp landed. They return only if clamping does — and then in canonicalize-then-compare form, not as a bare lexicographic.

## Thread C — tree-first scaffolding for the index test (`helper-algebras.rkt`, `sexp-edit.rkt`)

The document passes through five states, related by three free isos and one projection:

```
  shape ◀─A─▶ spines        tree ◀─C─▶ pieces ◀─B─▶ text
    ▲                        fold↑    tokenize     concat
    │ forget atoms            │      / parse       / lex
  tree ──(projection)─┘
```

- **A `shape ↔ spines`** — fold / unfold. Pure structure.
- **C `tree ↔ pieces`** — tokenize / parse.
- **B `pieces ↔ text`** — concat / lex.

Landed:

- **`helper-algebras.rkt`:** `iso-law?` (`(equal? ((iso∘ (inverse i) i) x) x)` — round-trip `to` then `from`) and `check-iso-laws` (sweep a corpus, return the inputs that don't round-trip). The formerly-orphaned `iso` gets its first consumer.
- **`sexp-edit.rkt` test module:** the generator (`gen:shape` → `gen:populate`) and the three isos, each checked on worked examples, a corpus, and 100 random cases.

Decisions, with grounds:

1. **The content side factors `tree ↔ pieces ↔ text`, not `tree ↔ text`** (user: "the proper one isn't tree to text rather tree to pieces and pieces to text"). `pieces` is the natural intermediary — the unit cutting actually produces; `tree ↔ text` (render/parse) is just the composite.
2. **`shape ↔ sexp` rejected as the iso.** Forgetting atoms (`atoms → ()`) is lossy — a retraction, not a bijection (user: "it's not a proper iso"). The proper isos all keep their information (pieces carry atom text; spines carry structure), so each genuinely round-trips.
3. **The free/bridge split is the point.** A, B, C are provable by construction (fold inverts unfold, concat inverts split, parse inverts canonical render) — scaffolding giving ground truth on both the structure side (A) and the text side (B/C). The claim actually under test — why `sand-spines`/`slot-guide` exist — is the **bridge**: that a spine, run through the guides on the text, lands at the boundary of the piece it names. The bridge is not one of these isos and is **not built** (see Open).

Generator and fold/unfold:

- **Two-pass:** shape first, then populate (user). `gen:shape` draws pure nesting; `gen:populate` turns each `()` into an atom.
- **The smallest shape is `()`** (user) — a leaf is just the empty-list base of the list recursion, so the shrinker treats leaf and frame uniformly. A populated frame is its bare list of children (`(() ())` → `("a" "b")`); the old `(list kids seps)` rep and its separator sublist are gone.
- **Whitespace uniform single-space, baked into the pieces.** Makes `render` injective (so `tree ↔ text` is an iso) at the cost of dropping varied/zero-width/multiline cuts from random generation — those stay corpus-only. Rejected: a separate per-junction whitespace pass (keeps the varied cuts, costs another structure to thread).
- **`tree->indexes`/`indexes->tree` are a cata/ana pair** (Haskell `Data.Tree` `foldTree`/`unfoldForest`). The unfold is the labelless rose-tree anamorphism `(map (unfold coalg) (coalg seed))`; the reconstruction is a trie — a path `(1 2)` is `(head . rest)`, so "group by car, map to cdr" (`map-group-by`) is the descent. The structural iso needs no end slots — a frame's children are exactly its head-groups; end slots only matter for the text split, the bridge's concern.

Naming: the state vocabulary settled as **shape / tree / text / spines / pieces**. The address list is **spines** (nomenclature: a spine is the per-level slot list, an index the position it names) — recorded against the earlier "seams → indexes" rename.

## Status

- **Complete and landed:** all three threads — the summary/spine cleanups, the rename/comparator/command vocabulary, the three isos + two-pass generator + `iso-law?`/`check-iso-laws` kit. 1218 green. Working tree only — **not committed**.
- `check-iso-laws` kept pure (no rackcheck) so `helper-algebras.rkt` stays dependency-clean like `rope-core.rkt`; the random property-driving lives in `sexp-edit.rkt`'s test module.
- **Settled:** the head/tail symbols; the tokenize fold; `sand-spines`; whitespace-binds-left; the conventional single comparator; the `pure`/`move`/`spread`/`chain` vocabulary with editing via `focus` and anchoring via `cover`; the free-iso decomposition and its naming.

## Open / parked

- **The bridge is unbuilt — the whole point.** The three isos are free scaffolding; none touches `sand-spines`/`slot-guide`. The actual "indexing works" test (spines, via the guides, cut the text at the piece boundaries, checked against A and B/C) is the next step. End slots return there.
- **Index comparison** (thread B) returns only if clamping does — in canonicalize-then-compare form.
- **Nomenclature `frontier`/`seam` entries** still list the old "atom/form seam flags" (`starts/ends-atom?`/`form?`) — stale after the head/tail change, not yet updated.
- **`spine` vs `index`** as the working term for the address list — flagged, not settled.
- **`check-iso-laws` as a full rackcheck battery** in its own `iso-laws.rkt` (the `summary-laws.rkt` parallel) — vs the pure helper it is now; would need a `CLAUDE.md` import-list entry.
- **`chain` trail variant** (scanl of intermediate zippers, for undo/inspection) — parked.
