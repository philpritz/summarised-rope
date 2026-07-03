#lang scribble/manual

@(require scribble/example
          racket/sandbox
          racket/runtime-path
          (for-label racket
                     "../summaries/sexp-summary.rkt"
                     "../summaries/lisp-summary.rkt"))

@; A trusted sandbox so the example evaluator may require the local module
@; (the default sandbox blocks reading files outside collects).
@(define-runtime-path lisp-summary-path "../summaries/lisp-summary.rkt")
@(define ev
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (define e (make-base-eval))
      (e `(require (file ,(path->string lisp-summary-path))))
      e)))

@title{lisp-summary}

The sexp structure of real Lisp source: @racket[lisp-smr] gates the sexp algebra
by Lisp lexical syntax --- string literals (backslash escapes honoured), @tt{;}
line comments, non-nesting @tt{#| |#} block comments, and @tt{#\} char literals.
Brackets and quotes inside any of them are inert.

@section{The mode machine and the value}

A fragment cannot know its entry mode --- that depends on text to its left --- so
the value answers for @emph{every} entry:

@verbatim{
  value : hasheq mode -> (arm exit n first tailc pend val)
  mode  : code | string | escape | comment | hash | charlit | block | block-pipe
}

@racket[exit] is the mode after the fragment; @racket[val] the sexp measure of its
code-equivalent text under that entry (strings and char literals read as the
placeholder atom @tt{~}, comment interiors vanish). Combine threads the left's
@racket[exit] into the right's entry --- function composition on the mode set,
@tt{sexp+} on the values --- @tt{strsexp}'s two-mode parity trick generalized to a
transition table.

The memory modes are match counters for the two-char delimiters: @racket['hash] =
"1 char of @tt{#|}-or-@tt{#\} matched", @racket['block-pipe] = "1 of @tt{|#}",
@racket['escape] = "1 of the @tt{\c} pair", @racket['charlit] = "2 of @tt{#\},
payload pending". The count lives only on the right edge (threaded state); the
left edge of the next fragment supplies raw chars and combine does the matching.

The ceiling, documented at @tt{step}: block comments do not nest (unbounded depth
is not finite-mode state); @tt{#;} is structural, not lexical; a char literal
consumes exactly one char (@tt{#\space} reads as @tt{~ pace}, inert either way);
@tt{|piped symbols|} are ordinary atoms.

@examples[#:eval ev
  (lisp-state (lisp-smr "(a \""))
  (lisp-state (lisp-smr "(a \"x\\"))
  (lisp-state (lisp-smr "(a #| x |"))
  (lisp-in-comment? (lisp-smr "#| c"))]

@section{Run classes}

Each char has a render class derived from the mode pair around it,

@verbatim{
  class : mode-before x mode-after -> code | string | comment | block | charlit | hash
}

so delimiters class with their construct: both quotes inside the string, the
@tt{;} inside the comment, and a line comment runs @emph{through} its terminating
newline (a comment ends at end-of-line, inclusive). @racket['hash] is glue --- a
held @tt{#} adheres to the run of whatever follows (@tt{#|}, @tt{#\}, @tt{#"},
or plain atom text).

A run is a maximal span of one class. The boundary rule:

@verbatim{
  boundary? : prev-class x class x gap-mode -> boolean
}

a boundary at a class change, @emph{or} at the same class when the gap's mode sits
outside the construct --- which is what keeps @tt{"a""b"} two runs (the gap is in
code mode) and @tt{#\a#\b} two char literals.

Per entry mode the arm carries @racket[n] (boundaries strictly inside),
@racket[first]/@racket[tailc] (the run classes presented at each edge ---
@tt{sexp-val}'s head/tail pattern, one level up), and @racket[pend] (trailing
glue, decision deferred rightward).

@section{Cut reads}

@defproc[(lisp-state [L any/c]) symbol?]{The entry mode at a cut: the all-left
value's exit from @racket['code]. @racket[lisp-in-string?] and
@racket[lisp-in-comment?] are its predicates (@racket['block] and
@racket['block-pipe] count as comment).}

@defproc[(lisp-spines [L any/c] [R any/c]) any]{@racket[sand-spines] on the
mode-selected sexp values: L's code-entry value against R's value under L's exit.}

@defproc[(class-sides [L any/c] [R any/c]) any]{A cut as the pair of run classes
touching it --- @racket[sand-spines]' shape for the lexical layer:
@verbatim{  class-sides : L R -> (values left-class right-class)}
Left is @racket[L]'s resolved tail (@racket['hash] when trailing glue defers);
right is @racket[R]'s presented first class, selected by the cut's mode.

@examples[#:eval ev
  (call-with-values (lambda () (class-sides (lisp-smr "(a ") (lisp-smr "\"x\" b)"))) list)
  (call-with-values (lambda () (class-sides (lisp-smr "(a \"x") (lisp-smr "y\" b)"))) list)]}

@section{The runs guide (experimental)}

@tt{lisp-runs-guide*}, in the @tt{experimental} submodule: @tt{multisect*} splits
a @racket[lisp-smr] rope at every run boundary --- strings, comments, block
comments, and char literals come out as whole pieces (glue travelling with its
construct), code runs stay maximal. Pruned by the cached @racket[n], so cost
tracks the number of runs, not the document. A pending-glue boundary is never cut
at a seam (that would split a delimiter); the guide resolves the glue through the
right context and reports the cut inside the child, before the @tt{#} group.
