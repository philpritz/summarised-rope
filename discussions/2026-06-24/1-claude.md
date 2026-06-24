# Discussion — 2026-06-24 — guide representation: vector vs cons vs list

Migrated the guide pair to a **list** `(list gs ge)` (storage: two `gs`/`ge` fields); the landing
is in this session's commits (git log). A list makes the lenses simpler — it's the shape
`list-of`/`lref` operate on, so the cons lenses (`pair-car/cdr/diag`) and the vector lenses
(`vref`/`vdiag`) all drop out.

- **vector** `#(gs ge)` — rejected: homogeneous, `vref 0/1`-indexed.
- **cons** `(gs . ge)` — dropped (mid-session intermediate): needed bespoke pair lenses.
