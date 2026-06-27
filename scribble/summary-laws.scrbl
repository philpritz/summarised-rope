#lang scribble/manual

@(require scribble/example
          racket/sandbox
          racket/runtime-path
          (for-label racket
                     "../summaries/summary-laws.rkt"))

@; A trusted sandbox so the example evaluator may require the local modules
@; (the default sandbox blocks reading files outside collects). The laws kit
@; depends only on a summary value, so the examples build one with make-summary.
@(define-runtime-path laws-path "../summaries/summary-laws.rkt")
@(define-runtime-path core-path "../rope-core.rkt")
@(define ev
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (define e (make-base-eval))
      (e `(require (file ,(path->string laws-path))
                   (file ,(path->string core-path))))
      e)))

@title{summary-laws}

An optional conformance kit for summary writers: @racket[check-summary-laws] runs the
battery, the @racket[law:identity] / @racket[law:associativity] / @racket[law:homomorphism]
properties run one law each, and @racket[identity-law?] / @racket[associativity-law?] /
@racket[homomorphism-law?] are the plain predicates underneath. Built against the
@racket[smr] value a writer gets from rope-core's @racket[make-summary] --- never against
rope-core itself, so the obligations cross the module wall (see @secref["role"]).

@section{check-summary-laws}

@defproc[(check-summary-laws [smr procedure?] [gs gen?]
                             [#:corpus corpus (listof string?) (quote ())]
                             [#:config c config? (make-config)])
         void?]{
Runs the full battery on @racket[smr] (a summary fn from @racket[make-summary]) with
@racket[gs] a generator of domain strings. First sweeps @racket[corpus] deterministically
--- every entry identity-checked, and homomorphism-checked at @emph{every} single cut ---
then property-tests all three laws over a random stream into which the corpus is mixed 1:3.
A failure reports the shrunk minimal counterexample and a replay seed. The corpus doubles as
regression memory: append a shrunk counterexample and it is re-checked deterministically
forever after.

See also @racket[law:homomorphism] and @racket[homomorphism-law?].
}

@section{The laws as predicates}

The runnable form of the obligations rope-core's caching, fusing, and rebalancing rely on.
Each is a plain predicate over concrete values --- usable in any test --- and is wrapped by a
same-named @racket[law:…] @racket[property] for the random layer. Values are compared with
@racket[equal?], so a summary's values need a sensible @racket[equal?].

@defproc[(identity-law? [smr procedure?] [x any/c]) boolean?]{
The unit behaves: folding a lone summary value from the unit returns it unchanged
(@racket[(smr x)] = @racket[x]), and folding the unit after it changes nothing
(@racket[(smr x (smr))] = @racket[x]).

@examples[#:eval ev
  (define len (make-summary string-length +))
  (code:line (identity-law? len (len "ab"))   (code:comment "lawful: + has unit 0"))
  (define minus (make-summary string-length -))
  (code:line (identity-law? minus (minus "ab")) (code:comment "- is not a monoid combine"))]
}

@defproc[(associativity-law? [smr procedure?] [x any/c] [y any/c] [z any/c]) boolean?]{
Grouping of combines does not matter: @racket[(smr (smr x y) z)] = @racket[(smr x (smr y z))].
@racket[x], @racket[y], @racket[z] are summary values. This is the license rope-core needs to
reassociate branches.

@examples[#:eval ev
  (define len (make-summary string-length +))
  (associativity-law? len (len "a") (len "bc") (len "d"))]
}

@defproc[(homomorphism-law? [smr procedure?] [s string?] [cuts (listof exact-nonnegative-integer?)])
         boolean?]{
The measure respects concatenation: measuring the whole equals folding the measures of any
split. @racket[cuts] are sorted offsets into @racket[s]; the substrings between consecutive
cuts (with @racket[0] and @racket[(string-length s)] bracketing implicitly) are measured and
folded. Repeated cuts yield an empty chunk, which exercises the unit in context.

@examples[#:eval ev
  (define len (make-summary string-length +))
  (code:line (homomorphism-law? len "abcde" '(2 4)) (code:comment "fold of the parts = whole"))
  (define mx (make-summary string-length max))
  (code:line (homomorphism-law? mx "ab" '(1))       (code:comment "max of parts /= measure of whole"))]
}

@section[#:tag "role"]{Why a separate kit, and why these laws}

@subsection{The kit's role}

@racket[make-summary] accepts any @racket[string-summary] / @racket[combine] pair that
typechecks, but rope-core's caching, fusing, and rebalancing are sound only if the summary
obeys laws no signature or per-call check can see --- associativity quantifies over
@emph{triples} of values, not one boundary crossing. The battery is those laws made runnable,
offered to a writer @emph{while} a summary is being implemented.

It is optional by necessity: a library cannot gate a writer's code, only make checking it
cheap. And it is how the laws reach the writer at all --- a summary writer never reads
rope-core, so the kit carries the obligations across the module wall. That is why it depends
@bold{only} on the @racket[smr] value (plus rackcheck and rackunit), never on rope-core.

@subsection{Two groups, three laws --- the group is the diagnosis}

The split is not just an ordering. A @bold{summary}-group failure (identity, associativity)
means the @racket[combine] / unit algebra is broken; a @bold{string}-group failure
(homomorphism) means the measure does not respect concatenation. They are @emph{independent},
so the battery needs both groups and is not offered à la carte:

@itemlist[
  @item{The homomorphism check folds left against the whole and never regroups; associativity
        is precisely the regrouping license. So @racket[combine] @racket[=] @racket[-] passes
        the fold yet fails regrouping --- @bold{only} the summary group fails.}
  @item{@racket[combine] @racket[=] @racket[max] is a fine monoid (associative, with unit
        @racket[0]) but the max of the parts is not the measure of the whole --- @bold{only}
        the string group fails.}
]

These two mutants live in the kit's own tests, each law group catching exactly its own. The
rope needs both: homomorphism makes leaves sound, associativity lets branches reassociate.

@subsection{Choices behind the phrasing}

@itemlist[
  @item{@bold{Homomorphism quantifies over any n-way split, not a single cut.} A monoid
        homomorphism preserves arbitrary finite products, so the n-way statement @emph{is}
        the law; a single-cut check is merely its n=2 case. Repeated cuts give empty chunks,
        testing the unit in context for free.}
  @item{@bold{Identity is phrased over summary values}, @racket[(smr x)] @racket[=]
        @racket[x], rather than over strings (@racket[(smr "" s)] @racket[=] @racket[(smr s)]).
        The value form is strictly stronger --- @racket[combine] @racket[=] @racket[-]
        satisfies the string form but fails the value form --- and writers may feed raw
        summary values, which @racket[smr] accepts.}
  @item{@bold{No rope-integration law in the battery.} A check that a built tree's cached
        summary equals the flat measure is, given these three laws, a @emph{theorem} --- it
        can only fail when rope-core is at fault, the wrong subject for a battery whose
        verdicts are about the summary. Excluding it also keeps the kit free of any rope-core
        dependency. The same blame logic covers crashes: a summary that raises is honest
        nonconformance, since the laws claim totality.}
  @item{@bold{Predicates first, properties second.} One statement of each law, shared by the
        random layer, the corpus sweep, and the mutant tests.}
  @item{@bold{Engine: rackcheck.} Integrated shrink trees give minimal counterexamples with
        no shrinker code; failures carry a replay seed; @racket[label!] distributions are the
        evidence the random layer hits the seams.}
]

The design session is @tt{discussions/2026-06-11/3-claude.md}.
