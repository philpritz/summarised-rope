# Discussion — 2026-06-11 — with Claude

An **implementation session**: the anchor-flipping thread taken from design into the
files. Three landings — flip-as-data in `sexp-edit`, a reworked `zipper-core`
surface, and re-anchoring (`cover`) — plus a stress run and a fresh `README`.
Suite green (**455**).

## Re-basing (built)

`moduli base-left base-right flip edge-moduli` in `sexp-edit`. The uniformity bar
(front − back = N+2 at every level and cut) means the per-level differences ARE the
flip data: an index re-bases by a componentwise shift, the ½ heads carrying for
free; `flip` is an involution. Scope: an index flips with the moduli **of its own
cut** — moduli are per-frame-path, exactly what one index alone cannot know. Spine
next/prev/parent remain undesigned (they would wrap on these same moduli).

## The survivability reading

Each family reads one side of its cut, so a seg cursor's sign pair declares which
edits it is invariant under: **(front, back) reads only the outside of the seg** →
stable under interior edits, covering whatever replaces the focus; (front, front)
only under edits after it; (back, back) only before; (back, front) under none
(content anchoring = the parked identity pin). The user's directive from this:
flip the **second** guide's anchor so "the guides are stable under replace delete
and insert, they remain covering the focus expression when we edit."

## zipper-core rebuilt (user-led)

Surface now **five names**: `start guide replace to-root peek`.

- **`replace` is the one verb** — it "does the work of delete and insert" (user):
  delete = `(replace "")`, insert = replace at a gap. *Supersedes 06-10/1's
  "insert kept (the more basic function)".*
- **The zipper stores its guides**; `guide` is a **three-faced accessor** (read /
  install a 2-vector / install `(f current)`) — get and set "at the same time as a
  lens" (user). Chosen over the van Laarhoven form (shown in chat; derivable in
  userland over the accessor): vL buys composition by functor instantiation, but
  the faces already compose by hand — modify nests by plain `compose`, read nests
  the other way round.
- **Every write re-navigates** (user: "both of these things renavigate to where
  the guide points after making their change"): one internal `renavigate`, both
  writes funnel through it. Composed accessors reach the zipper only through
  `guide`, so a composite write re-navigates exactly once, at the outermost face.
  `renavigate` unexported (one door). `to-root` does not re-navigate; guides
  survive it.

## Re-anchoring (built)

`(re-anchor z i side)` — re-derive edge i's guide from the chosen side's anchor,
install through the modify face; `(cover z)` = `(re-anchor z 1 'back)`. Derives by
**reading** (`anchors` at the edge), not the arithmetic flip — at a cursor the read
is direct and always current; both routes stay tested. Demonstrated end-to-end:
a covered gap at `^q` edited through grow/shrink chains, the right anchor fixed.

## Stress run (temp harness, not kept)

~640 random **balanced** replacements (the user's constraint: unbalanced insertions
would mess up the focus; any number of balanced ones) through chained covered
cursors — flat / nested / depth-8 / top-level / heavy — asserting per step:
focus = content, doc = prefix·content·suffix, both anchor spines re-read unchanged.
All clean in-domain; a negative control confirmed the uncovered drift. The
**end-slot** scenario (gap before `)`) fails at the *first navigate*, before any
flip: the cut's fractional front read (5/2) equals the mid-atom read of the
preceding atom, the guide's 0 is a plateau, `multisect` lands leftmost → carves
mid-atom; front and back even land at different cuts. A concrete reproduction of
the parked end-slot/lean items — a domain boundary, not a covering failure.

## Status

Built & green: whole suite **455**. Landed: re-basing + re-anchoring in
`sexp-edit`, the five-name `zipper-core`, `README.md` rewritten fresh (its example
lines mirror suite checks). The stress harness was a throwaway.

## Open / parked

- **Landing menu** for the post-edit cursor — (s,s) before / (s,e) select /
  (e,e) after, over the surviving pair — explored, not landed.
- **First-class accessor composition** (a combinator needs the outer's subject
  predicate) — hand-nesting by faces suffices for now.
- **Struct guides** (`prop:procedure`, index readable off the guide; 06-07/1
  Part E) — would enable the composed chain zipper → pair → guide → spine and
  dissolve the set/modify ambiguity for procedure-valued accessors.
- **Remote marks** (positions away from the edit): rebase-by-visit vs carried
  moduli (they go stale) vs both-families-at-creation — the patch-vs-identity
  fork again.
- **End slot / end-based anchors** — now with a reproduction (above).
- Carried: the flip restamp short-circuit (re-navigation stays uniform); spine
  next/prev/parent.
