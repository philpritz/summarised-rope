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
