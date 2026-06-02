# 001 — Mark/span heads, guides, and editing algebra

> **Deprecated — kept for history only.** This is the project's earliest design
> sketch, in the "mark/span" vocabulary and written for an earlier (Codex)
> workflow. The design has moved on substantially since — current direction
> lives in the `discussions/` notes (see the most recent dates). Read this only
> as background, not as the live design.

~~These notes capture the current design conversation for continuing work in Codex.~~ They are intentionally provisional.

This file lives under `design-notes/` so future sessions can add separate numbered notes and later synthesize them.

## Current framing

The zipper should distinguish two head shapes:

```text
mark-head:
  left ^ right

span-head:
  left ^ span ^ right
```

A **mark** is a single boundary. A **span** is an editable interval between two boundaries. The word `span` is still slightly provisional, but it is the current working term.

The main design direction is that editing operations should be built from a small algebra over these head shapes.

## Core editing algebra

### Mark to span

```text
expand-mark : mark-head -> span-head
```

`expand-mark` is the general primitive for turning a two-part mark partition into a three-part span partition.

Conceptually:

```text
left ^ right
=> left' ^ span ^ right'
```

It should be powerful enough to implement insertion, selection, and other operations that produce a span.

### Insert

```text
insert : mark-head, rope -> span-head
```

Insertion should operate on a mark head and produce a span head whose middle is exactly the inserted rope:

```text
left ^ right
=> left ^ inserted ^ right
```

This is deliberately span-producing. Normal typing is then:

```text
insert, then settle-right
```

while raw `insert` leaves the inserted material selected/highlighted.

### Span to mark

```text
contract-span : span-head -> mark-head
```

The refined meaning of `contract-span` is text-preserving: divide the span into two pieces and adjoin them to the two sides.

```text
left ^ span ^ right
span = span-left ++ span-right
=> left+span-left ^ span-right+right
```

This means `contract-span` chooses a mark somewhere inside the span. It is not deletion.

### Settle

`settle-left` and `settle-right` are named contractions.

```text
settle-left:
  left ^ span ^ right
  => left ^ span+right

settle-right:
  left ^ span ^ right
  => left+span ^ right
```

The word `settle` feels right: a span is a temporary/editing state, and settling chooses the final mark position.

### Delete

```text
delete : span-head -> mark-head
```

Delete is not a normal contraction. It drops the span:

```text
left ^ span ^ right
=> left ^ right
```

### Replace

```text
replace : span-head, rope -> span-head
```

Replace operates on a span head and swaps the middle:

```text
left ^ old-span ^ right
=> left ^ replacement ^ right
```

Normal type-over-selection behavior is:

```text
replace, then settle-right
```

## Guide model

Ordinary guides classify a mark relative to one boundary:

```text
-1 = before target boundary
 0 = at target boundary
 1 = after target boundary
```

Internally, ordinary guide results can be normalized using `signum`, so guide computations may return any signed number:

```racket
(define (signum n)
  (cond
    [(negative? n) -1]
    [(zero? n)      0]
    [else           1]))
```

The guide interpreter should use `signum` before dispatching.

## Span guides

A span guide should be more than two independent boundary guides. It should classify a mark relative to an interval.

Working scale:

```text
-2 = before span
-1 = at span start
 0 = inside span
 1 = at span end
 2 = after span
```

Names under consideration / current working names:

```text
span-guide-position
span-guide-start
span-guide-end
```

`span-guide-position` returns the five-state span position.

`span-guide-start` projects a span guide to an ordinary guide for the start boundary.

`span-guide-end` projects a span guide to an ordinary guide for the end boundary.

Using `signum`, the projections are elegant:

```text
span-guide-start = signum(position + 1)
span-guide-end   = signum(position - 1)
```

Reason: in the span scale, the start boundary is `-1` and the end boundary is `1`.

## Rope splitting

The real `split-rope` should be context-aware.

A guide is fundamentally about the summaries around a gap:

```text
left-total ^ right-total
```

So splitting a rope should provide the ambient summaries before and after that rope.

Working shape:

```text
split-rope : before-summary, rope, after-summary, guide -> left, right
```

Conceptually:

```text
before-summary | rope | after-summary
```

`split-rope` descends through `rope`, asking the guide about candidate gaps using full contextual summaries.

The context-free version should be a convenience wrapper:

```text
split-just-rope : rope, guide -> left, right
```

where both ambient summaries are empty.

## Three-way splitting by span guide

We also want a rope operation that splits a rope into three pieces using a span guide.

Working name:

```text
split-span-rope
```

Shape:

```text
split-span-rope : before-summary, rope, after-summary, span-guide
               -> before-span, span, after-span
```

Conceptually:

```text
before-summary | before-span ^ span ^ after-span | after-summary
```

This can be implemented using two ordinary `split-rope` calls:

1. Split at `span-guide-start`.
2. Split the remainder at `span-guide-end`, using updated context.

The operation isolates a target interval from a rope once the larger containing rope is in view.

## Exposure / bringing a span into view

We changed direction here.

Earlier idea: expose a span by producing a mark head such that the target span straddles the mark.

Newer idea: the operation that brings a span into view should rise/open/rearrange until the desired span is contained inside the **middle rope** of a larger span-shaped view.

Conceptual exposure shape:

```text
left ^ middle ^ right
```

where the desired span is somewhere inside `middle`.

Then `split-span-rope` can split `middle` into:

```text
before-target ++ target ++ after-target
```

and rebuild the final span head:

```text
left+before-target ^ target ^ after-target+right
```

This separates two responsibilities:

```text
expose = bring a containing region into local view
isolate = split that region to extract the exact target span
```

The exact naming for the exposure operation is still unsettled. Earlier candidates included:

```text
expose-straddle-mark
expose-span
expose-range
```

But after the rethink, it should probably not mention `mark`, because it does not merely return a mark head. It brings a containing middle rope into view.

## Relationship to navigate

`navigate` with an ordinary guide moves/rearranges the zipper until the target boundary is at the mark:

```text
navigate guide -> mark-head
```

The analogous span operation should bring a region containing the target interval into view, then use `split-span-rope` to isolate it.

Possible high-level pipeline:

```text
span-guide
=> expose containing middle rope
=> split-span-rope
=> span-head with target in the middle
```

## Example editing sequence: rename an S-expression

Starting text:

```text
(define square
  (lambda (x)
    (* x x)))
```

Goal: rename `square` to `cube`.

Conceptual sequence:

```text
1. navigate/expose using a span guide for the S-expression `square`.
2. isolate the target into a span head.

   (define ^square^
     (lambda (x)
       (* x x)))

3. replace with rope "cube".

   (define ^cube^
     (lambda (x)
       (* x x)))

4. settle-right.

   (define cube^
     (lambda (x)
       (* x x)))
```

## Example: select word containing current mark

This is the motivating example for scans / relative span targets.

Starting text:

```text
hello wor^ld today
```

Desired result:

```text
hello ^world^ today
```

The goal is to model this through span-guide-like machinery rather than ad hoc word functions.

The eventual operation should discover or construct a span guide for the word containing the current mark, expose a containing region, split out the span, then return a span head.

## Current working vocabulary

```text
mark-head
span-head

guide
span-guide-position
span-guide-start
span-guide-end
signum

split-rope
split-just-rope
split-span-rope

expand-mark
contract-span

insert
delete
replace

settle-left
settle-right
```

Open naming question:

```text
What should we call the operation that brings a containing middle rope into view for a span guide?
```

Open conceptual question:

```text
Should `span` remain the word, or should it become `range`, `region`, `segment`, or something else?
```
