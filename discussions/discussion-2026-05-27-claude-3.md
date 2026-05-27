# Discussion — 2026-05-27 (third) — with Claude

Very short, partial note. Parked idea. The framing here is
editor-conceptual, not algebra-mechanical — the structural collapse it
implies (one head shape, one guide, one navigate) is downstream of the
real claim, which is about how the user interacts with the text.

## The idea: the cursor is two marks, always

Most editors model the cursor as a single mark (a position between
characters). Selection is a *mode* you enter — you hold shift, you drag,
you press a key — and the editor temporarily tracks a second mark
until you collapse back.

The shift: drop the single-mark cursor entirely. The user's cursor is
*always* two marks. A "point cursor" is just the degenerate case where
the two marks sit at the same position.

```text
left ^mark1^ middle ^mark2^ right
```

There is no mode. There is no "enter selection." The two marks are
always there; sometimes they coincide, sometimes they don't.

## What this gives the user

Editing operations stop being modal. Every edit acts on the region
between the two marks:

- **Insert** = fill the region with content. If the marks coincide, the
  content lands between them (classical insert). If they don't, the
  selected text is replaced. No distinction between "insert" and
  "type-over-selection" — same operation.
- **Delete** = empty the region. If the marks coincide, nothing
  happens (no character is selected). To delete-left, you first extend
  one mark leftward; then delete is uniform.
- **Replace** = same as insert. There is no separate verb.
- **Move** = move both marks together.
- **Extend / shrink** = move one mark while leaving the other fixed.

Every "command" the user issues is one of: move-both, move-one, or
fill-the-region. There is no separate selection state to track or
toggle. The mental model is uniform.

## Why this feels seamless

The traditional editor has two states (point vs selection) and a set of
operations that behave differently in each. The user has to know which
state they're in to predict the outcome of a keypress. Insert in point
state = insert; insert in selection state = replace. Same key, two
behaviours.

Under always-two-marks there is one state and one set of operations.
The behaviour the user predicts is the *same shape* in every case; only
the width of the region between the marks varies. Type-over-selection
is just insert-with-a-non-empty-region. Backspace-when-selecting is
just delete-when-non-empty. The seams between the modes go away because
the modes go away.

## Status

Parked. Idea is open-ended and conceptual; not the present direction.
The implementation collapse it implies (no head discriminator, one
navigate, one relative, etc.) is large but mechanical — it follows
from the conceptual shift, not the other way around.

Worth picking up later as: *if* this is how the editor should feel,
*then* what does the algebra look like to support it cleanly?

## Where this came from

Came up while encoding seg addresses for relative navigation. Once a
seg-address is just a pair of gap addresses, point-navigation and
selection-navigation start to look like the same operation with the
endpoints coinciding or not. That mechanical observation gestured at
the conceptual one: maybe the editor itself shouldn't distinguish.
