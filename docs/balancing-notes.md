# Balancing Notes

Need: keep edits persistent/local, but prevent bad tree shape from making split, guide search, and zipper navigation linear.

## Eager balancing on concat

Batch concat of `n` existing roots can be `Θ(n)` by building a balanced tree over the roots.

Rejected as the normal edit policy because per-edit balancing is too eager: it can spend work on structure the zipper may never touch.

## Total rebalance

Flatten leaves and rebuild.

Cost:

```text
Θ(m)
```

for `m` leaves/chunks, assuming leaves and summaries are reused.

Rejected as the normal edit policy because it destroys locality if done after ordinary edits. Keep it as repair/maintenance.

## Deferred local rotations

Let local concat create temporary imbalance, then repair when the zipper exposes it:

```text
[L [A B]] -> [[L A] B]
[[A B] R] -> [A [B R]]
```

One rotation is `O(1)`. If the heavy side is already reasonable, local repair is logarithmic in the weight ratio. If the heavy side is a dirty spine, repair is `O(height)`, worst-case `O(m)`.

This fits the zipper well: touched regions heal, untouched regions need not be perfect. But rotations alone do not cap pathological debt.

## Policy

Use the hybrid:

```text
ordinary zipper movement:
  bounded local rotations

large batch concat:
  balanced build, Θ(n)

pathological subtree:
  full rebuild, Θ(m)
```

Track `summary`, `weight`, and `height` on branches. Rebuild when height is too large for weight, e.g.

```text
height(t) > C * log2(weight(t) + 1) + K
```

So the active region self-repairs locally, while very bad subtrees are detected and rebuilt before shape debt becomes unbounded.
