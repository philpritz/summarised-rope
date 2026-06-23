# Function Gallery

Deprecated functions preserved for their shape.

## `toward` — the gap-only descent step

    (define ((toward g smr) h)
      (match-let*-values ([((head b t a)) h]
                          [(mt)      (empty smr)]
                          [(refocus) (lens smr)]
                          [(probe)   (lambda (l r) (g (smr b l) (smr r a)))]
                          [(lt rt)   (bisect t)])
        (match* ((probe mt t) (probe lt rt) (probe t mt))   ; left edge | seam | right edge
          [(0 _  _) (refocus (edge-l mt)       h)]   ; gap at the left edge
          [(_ _  0) (refocus (edge-r mt)       h)]   ; gap at the right edge
          [(_ 0  _) (refocus (seam   lt rt mt) h)]   ; gap at the seam
          [(1 -1 _) (refocus (half-l lt rt mt) h)]   ; into the left half  (left edge inward)
          [(_ 1 -1) (refocus (half-r lt rt mt) h)]   ; into the right half (right edge inward)
          [(-1 _ _) (error 'toward "target precedes the focus -- ascend further")]
          [(_ _  1) (error 'toward "target follows the focus -- ascend further")])))

One step of the zipper's descent to a gap, driven by a single guide
(`zipper-core.rkt`, commit `1ce4de2`). Superseded by the gap/seg unification
(`discussions/2026-06-07`), which split descent into a chop-only walk plus a
separate `carve`.

## `fixed` — the arity-generic fixpoint loop

    (define ((fixed improve [same? equal?] [key list]) . xs)
      (let loop ([xs xs])
        (define ys (apply (compose list improve) xs))   ; improve's values, listed
        (if (same? (apply key xs) (apply key ys)) (apply values ys) (loop ys))))

Iterate a values-in / values-out `improve` to a fixed point, halting when a
projection (`key`, under `same?`) stops changing (`helper-algebras.rkt`, commit
`c3898a0`). One loop for every arity — the tuple carried as the list `xs`, and
`(compose list improve)` reifying improve's multiple return values back into it
each step. Superseded by an arity-specialized `case-lambda`: clauses 1..4, built
by an internal `fixed-case` macro, loop on named variables with no per-step
`apply`/`list`, and this list form survives only as the past-4 tail (`rest-loop`).
The named-variable loop measured ~3× faster on the 2-value path (navigate's
`ascend`/`descend`); the list shape is kept here for its one-loop-fits-all
generality.
