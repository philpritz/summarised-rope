# Discussion — 2026-07-09 — the spl: a splitting pair under the isos — with Claude

A **mixed session**; the design substance is the `spl` pair, found on the way to
"make the lines-zip put", plus the store-shaped reconstruction of the opt it
prompted. The session's many other landings (surface trims, `memoize`, combinator
extensions) are routine — the commits are their record. The pre-change generation
is saved in `toolbox/old/`; 2569 tests green.

**The spl — a splitting:**

- `zipper ↔ lines-zip` is not an iso: the conversion must *expand* the focus
  outward to whole lines, and it forgets the guide identity — a row frame cannot
  recover a sexp cursor. But it is lawful one-sidedly: a **section–retraction
  pair**, `to` embedding with no loss, `from` projecting with loss, law
  `(from ∘ to) = id` only. Landed as `(struct spl (to from))`, callable as `to`.
- **iso is now a substruct of spl** — same fields, one more law. Generality is
  fewer laws, not more data: every spl combinator, wearing, and battery takes an
  iso unchanged (`iso?` gates only the ops needing the second law); the batteries
  nest the same way.
- `e = to ∘ from` (`spl-normalize`) is the **split idempotent** — everything the
  pair forgets, as a map. For `lines-spl` (landed in `lines-zip.rkt`) it is
  exactly the line expansion: a mid-line edge rounds out to its line (newline
  included), a point cursor covers its containing line; idempotent, identity on
  the section's image.
- The spl law holds on a **quotient**: `lines-zip=?` compares window + frame only —
  the slack buffer is cache, and a round trip may recenter it. The pattern to
  remember: each layer's iso is exact only *up to something operational* (buffer
  placement here, closure identity for the opt's laws below).
- **Named `spl`** on the stated condition that "a splitting" be precise for exactly
  this — verified: a pair with the one-sided law is exactly a splitting of its own
  idempotent, and every split idempotent yields such a pair. (`ret` rejected —
  reads as *return*; `epi` rejected — names one arrow of the two and drops
  "split", which carries the content.)

**The reconstructed opt — `(struct opt (peek view))`, briefly:**

- The generalization that closed the loop: a lawful lens **is** an iso
  `w ≅ (put · foci)` — GetPut is `apply ∘ peek = id`, PutGet + context-stability
  the other side. So the peek became *the struct*; `opt-get`/`opt-set` are
  projections, `opt-update` a derived command, the stored transform `f` dropped
  (its composition rule vanished with nothing replacing it). A bare run means
  **recompose**: identity for a lawful optic, `e` for a worn spl —
  `spl->opt`/`iso->opt` are one-line wearings, opposite orientations.
- The second field is the **viewer channel**: pure world-renders under a monoid
  (`no-view`/`view-join`), never in the put path. Context that downstream optics
  must consume stays as extra *foci* instead (lisp-edit's widened optics) — the
  channel deliberately never flows into an inner world.
