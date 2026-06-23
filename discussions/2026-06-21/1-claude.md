# Discussion — 2026-06-21 — renderer shape + summary-combine perf — with Claude

A **mostly exploratory session**: the general shape of the line renderer, then a perf pass. One change landed; the renderer is scratch and the details aren't settled.

**Renderer shape:**
- The viewport is pure geometry — top line + row count — separate from the document/zipper.
- A screen line is read as a **head** `before · focus · after`: focus is the line's text, `before`/`after` the summaries flanking it (off the zipper's edge accessors).
- **Syntax is read off `before`, not rescanned** — the cached summary at a line's start carries the entering context (in-string?, paren depth, keyword position), which `analyze-head` turns into spans. The payoff of caching a summary at every node: per-line context is a read, not a re-lex.
- **The cursor highlight is the zipper's own `before⟦focus⟧after`** — one contiguous segment, one `⟦…⟧` pair — split on newlines, not a marker per line.
- **`paint` is a pure read `(spec, z) -> lines`**, kept out of the edit pipeline (the Elm split); a `tap` or Writer-style form derives from it if a pipeline op is wanted.

**Landed:**
- `rope-core.rkt` — `make-summary`'s `smr` is now a `case-lambda` with fast unary/binary paths (the variadic `foldl`/`map`/`coerce` was the bulk of combine cost); valid by the identity law `(combine id x) = x`. Tests pass. ~2.9× on combine-heavy paths.

**Perf findings:**
- Per-line render is ~98% navigation, not syntax.
- Futures give no real speedup (the cost is allocation, not parallelism).
- The bundle being a hash is the next bottleneck (build + per-slot extraction).

**Directions (not done):** vector-backed bundle; reconstruct a screen from one block navigation instead of per-line; regex-free leaf scanners.

**Scratch:** `scratch/render-highlight/` — renderer + frozen highlighter snapshot + cost harnesses. Not promoted.
