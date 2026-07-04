#lang scribble/manual

@title{Scrolling the lines: the lines-zip}

@margin-note{A design-and-cost note for @filepath{text-edit/lines-zip.rkt}. Records
the options weighed and the rough benchmarks behind them; the figures are
single-run, order-of-magnitude, not calibrated measurements.}

A @deftech{lines-zip} is a scrolling line viewport over a summarised document:
given a document in a zipper, it presents @tt{h} visible lines at a row and moves
up and down cheaply. This note records how the design was chosen --- the options
considered, why the obvious ones fail, and where the cost actually goes.

@section{The problem, and the shape of the answer}

The document is a summarised rope under a zipper. The zipper already maintains
@tt{before · focus · after} with cached summaries on both flanks --- structurally a
viewport (off-screen above · screen · off-screen below). So a viewport is some
disciplined use of the cursor: navigate the focus to a band of lines, split it into
rows, and move by re-navigating. The whole design question is how to make ``move by
one line'' cheap when the document is large.

@section{Options considered}

@subsection{A zipper ↔ flat-zipper iso (rejected)}

The first idea: make the lines-zip an isomorphic re-presentation of the zipper ---
@tt{before-rope · deque-of-lines · after-rope} --- with a lawful @racket[iso] back
to the machine zipper, so nothing is lost.

It is not an iso once movement enters. The viewport frame is inherently a
@emph{linecol} frame (scroll is defined in rows), so the forward map must express
whatever the cursor's guides were as row boundaries --- a @bold{forgetful} map, not
invertible: two different cursors that happen to carve the same band collapse to one
flat, and a cursor not aligned to a row boundary cannot be recovered. Carrying the
original guides verbatim gives a strict iso only for a @emph{frozen} frame, which is
moot --- the whole point is to move. So the honest structure is a @emph{retraction}
onto the linecol-framed zippers, not an iso, and the cursor (exact, anchored) and
the display frame (linecol, coarse) are two different concerns that must not be
conflated.

@subsection{Re-navigation overlay (adopted)}

Drop the separate structure. The lines-zip is a @bold{view over the zipper},
parametrised by a linecol frame @tt{(n, m)}: the band is derived by navigating the
zipper to those rows and splitting; scrolling just moves the frame and re-guides.
The guide-reframing problem vanishes --- the frame @emph{is} @tt{(n, m)} integers,
and the guides are rebuilt fresh from them each move, never carried or lost.
Navigation reuses the machine's own rise/toward, so an adjacent move climbs a crumb
or two rather than re-descending from the root.

@subsection{A deque cache and smart reframe}

Re-splitting all @tt{h} lines every scroll is wasteful: a one-row scroll shares
@tt{h-1} lines with the previous window. So the window is held as a deque, and
@racket[reframe] reuses the overlap --- it fetches only the rows @emph{not} already
in the deque:

@racketblock[
(define (reframe w n* m*)
  (define lo (max n n*)) (define hi (min m m*))    (code:comment "the overlap")
  (cond
    [(>= lo hi)                                     (code:comment "disjoint: fetch it all")
     (fetch-all n* m*)]
    [else                                           (code:comment "reuse [lo,hi), fetch the gaps")
     (splice (fetch n* lo) (reuse lo hi) (fetch hi m*))]))
]

Local scroll fetches 1 and reuses @tt{h-1}; a far jump fetches all @tt{h} but in a
@emph{single} navigation, so its cost is independent of jump distance.

@subsection{A slack buffer (adopted)}

Even a one-line fetch pays a navigation. So the navigated band is a @bold{slack
buffer} @tt{[N, M)} holding the window @tt{[n, m)} plus overscan @tt{s} on each
side. Scrolling @emph{within} the slack is pure index math --- no navigation at
all; only crossing the buffer edge triggers a refill that re-navigates and
re-centres. Navigation drops from every scroll to one per @tt{s} scrolls.

@subsection{Loose (tolerant) frame guides}

A refill's buffer only has to @emph{contain} the window+slack, not sit on exact line
boundaries. A @bold{loose guide} accepts any cut whose line-count is in a band, so
the descent halts at a coarse rope boundary without drilling to an exact newline:

@racketblock[
(define ((loose lo hi) L R)
  (define ln (linecol-lines (linecol-smr L)))
  (cond [(< ln lo) 1] [(> ln hi) -1] [else 0]))    (code:comment "0 over a range")
]

This is not new machinery: @racket[within-ratio] --- the balance comparator that
@racket[bisect] already uses by default --- is the same tolerant guide on subtree
size. An exact guide is the width-0 case. Under a loose guide, @tt{ascend}
guarantees the focus contains the @emph{inner} band (the core it must cover) and
@tt{descend} tightens it to the @emph{outer} band, so the focus is squeezed between
the two interpretations and lands at whatever coarse boundary is cheapest.

@section{Where the cost goes}

@subsection{Navigation dominates; splitting is nearly free}

Decomposing one scroll (4000 lines, @tt{h=50}):

@tabular[#:sep @hspace[2]
(list (list @bold{algebra}       @bold{navigate+split} @bold{navigate only} @bold{split})
      (list "view-smr (lisp+lc)" "1676 µs"             "1641 µs"            "~35 µs")
      (list "linecol only"       "71 µs"               "69 µs"              "~2 µs"))]

Splitting a line off the focus costs almost nothing. Navigation is the cost --- and
it is @bold{24× more expensive under the lisp bundle} than under linecol, because
every rope-join during the descent recomputes the node summary, and @racket[lisp-smr]
rebuilds an 8-entry per-mode hash on each combine. The navigation @emph{structure}
is fast (69 µs); the @emph{algebra} carried through it is what's slow.

The design consequence: keep @racket[lisp-smr] off the scroll path. Scroll on
linecol; compute the syntax classes lazily, per visible line, at paint time ---
which is why @filepath{lisp-view.rkt} colours the lines the lines-zip hands back
rather than riding every navigation join.

@subsection{An adjacent scroll is O(1), not O(log n)}

Navigation-only, linecol, across document sizes:

@tabular[#:sep @hspace[2]
(list (list @bold{lines} @bold{scroll-down} @bold{scroll-up})
      (list "2 000"      "72 µs"            "80 µs")
      (list "8 000"      "69 µs"            "60 µs")
      (list "32 000"     "71 µs"            "75 µs")
      (list "128 000"    "68 µs"            "79 µs"))]

The document grows 64× and the per-scroll cost stays flat: an adjacent move climbs
to the nearest common ancestor (a crumb or two) and carves back down, never
re-descending from the root. The crumb stack makes locality free.

@section{Rough benchmark comparisons}

@subsection{Smart vs dumb reframe (split reuse)}

Single-row scroll, bundle algebra; smart reuses @tt{h-1} lines, dumb re-splits all
@tt{h}:

@tabular[#:sep @hspace[2]
(list (list @bold{window h} @bold{smart} @bold{dumb}  @bold{ratio})
      (list "10"            "1669 µs"     "2470 µs"    "1.5×")
      (list "25"            "1669 µs"     "3174 µs"    "1.9×")
      (list "50"            "1693 µs"     "3783 µs"    "2.2×")
      (list "100"           "1708 µs"     "4148 µs"    "2.4×"))]

Smart is flat in @tt{h} (always one line split); dumb grows linearly. The shared
floor is the one navigation both pay.

@subsection{Slack vs strict focus}

@tt{h=50}, slack @tt{s=50}, 100 000 lines, linecol:

@tabular[#:sep @hspace[2]
(list (list @bold{direction} @bold{strict} @bold{slack} @bold{ratio})
      (list "scroll down"    "160 µs"       "21 µs"      "7.5×")
      (list "scroll up"      "188 µs"       "23 µs"      "8.2×"))]

Strict re-navigates every scroll; slack navigates once per @tt{s}, so the ~70 µs
navigation amortises to ~1.4 µs/scroll. The remaining slack cost is the buffer
slice, not navigation.

@subsection{Loose vs exact refill}

Window moves 40 rows, 2000 refills:

@tabular[#:sep @hspace[2]
(list (list @bold{cut} @bold{µs/refill})
      (list "exact"    "298 µs")
      (list "loose"    "134 µs"))]

The loose guide halts at a coarse boundary instead of drilling to the newline ---
~2× cheaper, and it overshoots slightly (more free slack, so refills are also
rarer).

@section{The chosen design, and what it gives up}

The lines-zip is @tt{(z, buf, N, M, n, m, s, total)}: the zipper is the document
and its lazy line-access, @tt{(n, m)} the window, @tt{[N, M)} the slack buffer,
@tt{buf} the cached lines. Scrolling within the slack is O(1) index math; crossing
the edge refills once per @tt{s} scrolls; the scroll path stays on linecol.

Accepted trade-offs:

@itemlist[
@item{@bold{Not an editing surface.} It is a read view; edits go through the zipper
and re-open the frame. The put exists structurally but is not exposed.}
@item{@bold{Loose buffers are representation-dependent.} A loose cut lands at
whatever coarse boundary the tree offers, so the buffer edge is not a pure function
of content --- fine for a covering buffer, wrong for an anchored cursor.}
@item{@bold{The slice is O(buffer).} @racket[view-lines] re-slices the buffer each
scroll; a visible-window deque would make it O(1), deferred until profiling asks.}
]
