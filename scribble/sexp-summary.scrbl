#lang scribble/manual

@(require scribble/example
          racket/sandbox
          racket/runtime-path
          (for-label racket
                     "../summaries/sexp-summary.rkt"))

@; A trusted sandbox so the example evaluator may require the local module
@; (the default sandbox blocks reading files outside collects).
@(define-runtime-path sexp-summary-path "../summaries/sexp-summary.rkt")
@(define ev
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (define e (make-base-eval))
      (e `(require (file ,(path->string sexp-summary-path))))
      e)))

@title{sexp-summary}

The s-expression structure cached at every rope node: @racket[sexp-smr] (the
monoid), @racket[malformed?], @racket[sand-spines] (the navigation read), and the
string-aware seeds @racket[str-smr] and @racket[strsexp-smr]. Built on rope-core's
@racket[make-summary]; the plain-text metrics and the @tt{buffer-smr} product that
combines this instance with them live in @tt{summaries.rkt}.

@section{sexp-smr}

@defproc[(sexp-smr [part any/c] ...) (or/c #f sexp-val? (quote malformed))]{
Folds the s-expression algebra over its parts. The value is one of three states:
@racket[#f] is the empty/identity (@racket[(sexp-smr "")]); @racket['malformed] is the
absorbing clash; otherwise a @tt{sexp-val}, a struct
@racket[(sexp-val head closes forms opens tail)] whose @racket[head]/@racket[tail]
are the char-classes (@racket['open] | @racket['close] | @racket['ws] |
@racket['atom]) of the fragment's first and last chars, flanking three measure
fields described under @secref["the-algebra"].

Brackets @tt{( ) [ ] @"{" @"}"} are matched strictly by kind: a closer closes only
the innermost open of its own shape. A wrong-kind innermost open is a clash that
collapses the whole value to @racket['malformed].

@examples[#:eval ev
  (code:line (sexp-smr "(a b)")    (code:comment "one complete form: opens/closes empty"))
  (code:line (sexp-smr "(aa ")     (code:comment "an open frame survives in opens"))
  (code:line (malformed? (sexp-smr "(a]"))     (code:comment "wrong-kind closer"))
  (code:line (malformed? (sexp-smr "(a [b] c)")) (code:comment "well-formed multi-kind is fine"))]

See also @racket[sand-spines].
}

@section{sand-spines}

@defproc[(sand-spines [L any/c] [R any/c]) any]{
The summary's read interface for navigation. At a cut between summaries @racket[L]
(everything left) and @racket[R] (everything right), returns two values --- the
all-left @racket[front] spine and the all-right @racket[back] spine, each
innermost-first, the slot at each level being a form index.

@racket[front] slots are 0-based (the stored @racket[+1] drops at the read);
@racket[back] slots are as stored (@racket[-1] = after the last form). A @racket[½]
refinement leans the head slot to place a cut that falls @emph{inside} a token ---
see @secref["the-cut"].

@examples[#:eval ev
  (sand-spines (sexp-smr "(aa ") (sexp-smr "bb cc)"))]

The spine algebra that compares against these spines lives in @tt{sexp-edit.rkt}.
}

@section{str-smr and strsexp-smr}

@defproc[(str-smr [part any/c] ...) exact-nonnegative-integer?]{
The seed of syntax highlighting: in-string state as a raw count of @tt{"}
characters. At a cut, @racket[(str-smr L)] is the quote count to the left, and its
parity is whether the cut sits inside a string.

Naive on purpose --- it counts @emph{every} @tt{"}, so escapes (@tt{\"}), quotes in
comments, @tt{#\"} char literals, and @tt{|...|} symbols are not yet discounted.
}

@defproc[(strsexp-smr [part any/c] ...) any]{
The s-expression algebra gated by string state: a @tt{"} toggles in/out of a string,
and tokens inside a string are inert --- the whole string collapses to one form, an
atom with an opaque interior. @racket[strsexp-in-string?] reads the in-string flag at
a cut; @racket[strsexp-spines] reads the cut as spines, like @racket[sand-spines].

@examples[#:eval ev
  (code:line (strsexp-in-string? (strsexp-smr "(a \""))   (code:comment "open quote: inside"))
  (code:line (strsexp-in-string? (strsexp-smr "(a \"x\" ")) (code:comment "closed: outside"))]

See also @racket[sand-spines].
}

@section[#:tag "the-algebra"]{The algebra}

How a fragment's summary is computed, and why the encoding makes it a monoid.

@subsection{Signed stacks}

A @tt{sexp-val} carries @racket[opens] and @racket[closes] as stacks of
@racket[(bracket . count)] entries, innermost-first, but the counts are
@emph{signed}: an @racket[opens] entry is @tt{(bracket . +(k+1))}, a
@racket[closes] entry is @tt{(bracket . -(k+1))}. So a value reads
left-to-right like the fragment itself --- @racket[(sexp-val head (negatives) forms
(positives) tail)] --- and a slot index reads @emph{directly} off a stack head:
@racket[front] is the count at @racket[(car opens)] of the before-summary,
@racket[back] is the count at @racket[(car closes)] of the after-summary. No count
is ever @racket[0] (zero stays the frame's own boundary).

The point of the sign is that the slot offset is itself @emph{storable}: once both
stacks carry it symmetrically and the combine compensates, no separate index needs
threading through. The associativity battery (@tt{summary-laws.rkt}) is the proof
that it stays consistent under chunking.

@subsection{Completion counting}

A frame counts on the enclosing level at its @bold{closer}, not its opener (atoms
still count at their first char). The pop bumps what it exposes. This is what makes
spine comparison naively lexicographic: an open frame's interior leads with the same
value as the frame's own start slot --- a prefix @emph{extension} of it --- instead
of colliding with the next sibling's start.

Mirrored on the right: a dangling closer seeds the level above with the frame it
closed (@racket[forms := 1]), so @racket[closes] entries count frames whose close is
ahead. Every completion is counted exactly once, by whichever side saw the closer ---
a leaf pop bumps; the merge's cancellation does not (the right chunk already counted
it via its @racket[forms := 1] seed).

@subsection{Kind-strict, malformed-absorbing}

On well-formed input, strict by-kind matching agrees with bracket-blind nesting, so a
single kind (@tt{( )} only) can never clash and stays fully associative on @emph{any}
input. A multi-kind clash goes to @racket['malformed], and because @racket['malformed]
is absorbing, the combine stays associative even on malformed fragments. These two
facts are what license @racket[sexp+] as a lawful monoid operation.

@subsection{The combine}

@racket[sexp+] cancels the right operand's closers against the left's opens,
kind-strict: a matching innermost pops (no bump --- the right chunk already counted
that form); a wrong kind is a clash. The right's leftover forms and opens then nest
into the left's innermost surviving open. One boundary case: if the left ends
mid-atom and the right starts mid-atom, the right's leading atom is a
@emph{continuation}, so the form it began is undone (@tt{drop-start}).
@racket['malformed] absorbs; @racket[#f] is the identity.

@subsection[#:tag "the-cut"]{The ½ refinement at a cut}

@racket[sand-spines] classifies a cut by the two char-classes touching it --- the
tail of @racket[L] and the head of @racket[R]:

@itemlist[
  @item{@bold{start} --- a form begins here (atom or opener): flush, no lean;}
  @item{@bold{end} --- right before a closer or the document end: flush, no lean;}
  @item{@bold{mid} --- straddling an atom: @racket[front] @racket[-½], @racket[back] @racket[+½];}
  @item{@bold{lean} --- whitespace, binding to the previous form: @racket[front] @racket[-½], @racket[back] @racket[-½].}
]

The refinement lands on the @bold{head} slot only. An atom's interior is the one
structurally invisible position --- a frame's interior shows up as depth in the
spine, but an atom's does not --- so a mid-atom cut is pushed half-way into the atom
to give it a distinct position between the atom's start and the next form's start.

@subsection{Gated string state}

@racket[strsexp-smr] reuses @racket[sexp+], gated by string state. The difficulty: a
fragment cannot know whether it @emph{begins} inside a string, since that depends on
everything to its left. So the value carries the @tt{sexp-val} parsed under
@emph{each} entry mode --- entered in code, and entered mid-string --- plus the quote
count, whose parity is the gate. Each @tt{sexp-val} is built by transforming the text
to its code-equivalent: every string becomes a single placeholder atom (@tt{~}) with
its interior removed, so the existing tokenizer, fold, and @racket[sand-spines] treat
a string exactly like an atom. The combine is @racket[sexp+]; the only new logic is
selecting which of the right operand's two @tt{sexp-val}s to splice, by the left's
parity.
