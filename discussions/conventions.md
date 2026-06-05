# Discussion File Conventions

This folder keeps compact notes from design sessions.

## Naming

Discussion files should be named so chronological order is visible from the
path, even when different assistants or collaborators are involved.

Use a date folder, then put the chronological number before the source:

```text
discussions/YYYY-MM-DD/N-<source>.md
```

where:

- `YYYY-MM-DD` is the session date.
- `N` is the chronological session number for that date.
- `<source>` is the main collaborator/source, for example `claude` or
  `chatgpt`.

Examples:

```text
discussions/2026-05-27/1-claude.md
discussions/2026-05-27/2-claude.md
discussions/2026-05-27/3-claude.md
discussions/2026-05-27/4-chatgpt.md
```

If an older file is missing a number or uses a flat filename, prefer moving it
into the date folder when convenient, rather than leaving chronology implicit.

## Collaboration style

Keep project discussions tight and user-led:

- Let the user propose ideas first.
- Do not go off designing or thinking ahead independently.
- Avoid fluff.
- Keep responses compact unless more detail is requested.

## Proposals are exploratory by default

When the user proposes an idea, it is opening a discussion, not a go-ahead to
implement. The default is to explore it together and get a feel for it:

- Discuss the shape, trade-offs, and alternatives first.
- Trying code is welcome, but show it inline in chat as a sketch. Do not write
  it into project files, commit, or push.
- Stay in this exploratory mode, iterating, until the user explicitly signs off
  on adding it.
- Only after sign-off: make the change in the project, then commit/push per the
  rules below.

A proposal is an invitation to think together, not a task to rush to done.

## Questions are questions, not instructions

When the user asks a question, answer the question. Do not infer a plan, a
design decision, or a go-ahead from it.

- A question like "do we need X?" or "is Y used anywhere?" is a request for
  information, not a request to remove X or change Y. Answer what was asked and
  surface the findings. (If instead the question probes a choice in your own
  work — say code you drafted in chat — feel free to revise it if you wish.)
- **Above all, do not close off design avenues on your own.** In these
  discussions the user decides which possibilities to rule out; your job is to
  keep them open — lay out options and trade-offs, don't prune them.
- When unsure whether a question is also a request to act, ask before acting.

This pairs with the exploratory-by-default rule above: both keep the work from
racing ahead of the user's lead.

## "Draft" means draft in chat

When the user asks to *draft* something, produce it inline in chat — not in a
project file. Drafting is collaborative: show the text, revise it together, and
write it into a file (conventions, notes, code) only after the user signs off.

- "draft X" / "show X" → show it in chat first; iterate until sign-off, then
  write.
- A direct instruction to add concrete content already in front of you ("put
  this in …", "add this") is a go-ahead to write it straight away — no draft
  round needed.
- Sign-off must be explicit and affirmative ("write it", "add it"). A request to
  *change* the draft is a new revision round, not approval — after applying the
  edit, re-show the result and wait. When in doubt, stay in chat.
- A tentative "maybe X" depends on state: while still drafting in chat it means
  keep revising (never a write); once the content is already in the file, apply
  it by default unless you think of a serious objection, in which case we'll
  discuss. If the "maybe" carries a question ("…what do you think?"), that's
  soliciting an opinion (see *Questions are questions*), don't apply.
- "Draft" means in chat.

So drafting is the collaborative mode; an explicit "add this" is not.

## When to record

Do not be eager to write discussion notes. Recording happens when the user
decides to finish the session, not mid-thread. Keep designing until then;
write the note only when the user calls the session done.

## Content style

Keep notes compact and durable:

- Record decisions and open questions, not full transcripts.
- Mark parked ideas explicitly as parked.
- Mark draft code clearly as draft code.
- Include small code snippets only when they capture the shape of the design.
- Mention the concrete files affected when useful.

## Record the alternatives, not only the choice

A decision is half-recorded if the note says what was chosen but not what it was
chosen *over*. The reasoning — the options weighed, why each was set aside, why
the survivor won — is the most valuable and least recoverable part of a session;
capture it alongside the decision, not as an afterthought.

For each decision that mattered:

- list the **options considered**, including ones that were attractive and only
  narrowly lost;
- for each one set aside, give the **specific reason** it lost — the concrete
  cost or flaw, not just "we preferred the other";
- say why the **chosen** option won — what it buys, and what it gives up.

Distinguish **parked** (a live option merely deferred) from **rejected**
(considered and ruled out, with a reason); both belong in the note, only their
status differs.

This is what lets a later session build on the work instead of re-deriving it:
settled questions stay settled because the rationale is right there, and a wrong
turn is cheap to undo — the runners-up and their reasons are already written, so
you resume from the fork, not from scratch. Keep it proportionate, though: a
decision turned over once needs a line or two per option, not a transcript. The
test is whether someone arriving at the fork cold could see why it went the way
it did.

## Write for a reader who wasn't there

A discussion note is a **standalone artifact** — decipherable from the repo alone,
without the conversation that produced it. The transcript evaporates; the note is
what survives, so nothing in it may depend on the transcript to be understood.

- **Design the examples — don't transcribe the chat's.** Examples raised live are
  chosen for the back-and-forth, not the page: ad-hoc, half-stated, tangled with
  context, and rarely the clearest teaching case. You are positively *encouraged*
  to rework them or invent fresh ones — pick whatever conveys the idea most cleanly
  and elegantly. Fidelity is to the *idea*, not to the example that happened to
  come up; a cleaner illustration you build yourself is the better record.
- **Build on the repo, not the conversation.** Leaning on an earlier dated note or
  the code is fine — they're durable and a reader can follow the pointer. Leaning
  on what "we just said" is not.

## Refine a draft with agents

When a request to write or draft a note mentions **agents** — "write the draft and
refine it with agents", "spawn agents to see if the ideas get across" — don't just
write it: write it, then *test it on fresh readers and revise from what they miss*.
The reader is a future session, exactly the audience; this is the runnable form of
*Write for a reader who wasn't there*.

The loop:

- **Spawn a fresh agent** — one with no access to the current conversation; that's
  the point. Hand it the draft, the core files it describes, and any prior notes it
  points to. Read-only.
- **Probe answer-free.** Ask it to reconstruct the decision and answer specific
  questions, but never reveal the answers in the prompt. Require it to **attribute**
  each answer to the *note*, the *code*, or its own *inference* — that split
  separates "the note conveyed it" from "a capable reader filled it in."
- **Read its report** (it returns to you, not the user); the gaps are wherever it
  leaned on the code or inference, or flagged something asserted, unclear, or
  over-claimed.
- **Revise, then re-test with a new fresh agent** — a clean slate each round, no
  carryover. Two or three rounds usually converge; stop when a fresh reader clears
  it end-to-end.
- Keep the revisions in the working tree until sign-off (the draft rule holds).

## Status sections

When a discussion produces code, include a short status section that says what
is complete, what is not complete, and whether the work is ready to promote or
only a draft artifact.

## Merging to master

Two distinct actions, not to be conflated:

- **Push the feature branch.** Committing and pushing to the session's feature
  branch is routine — it just syncs the branch to the remote. Plain "push" /
  "upload to GitHub" requests mean this, and it is *not* a merge to master.
- **Merge to master.** Only when the request explicitly contains the word
  "merge" or "master" — e.g. "merge to master", "push to master", "merge this".
  Plain "push" / "upload" never triggers it; without one of those words, the
  feature branch is never folded into `master`.

## Amendments inherit the last destination

A follow-up amendment is handled the same way as the most recent comparable
action, without the user restating how. The disposition changes only when the
user redirects.

- If the last change was **pushed to master**, subsequent amendments are also
  committed and pushed to master — no need to re-ask.
- If the last change was **pushed to a feature branch**, subsequent amendments
  go to that same branch.
- If the current mode is **drafting in chat**, follow-up tweaks are likewise
  drafted in chat (still subject to explicit sign-off before they land).

This refines *Merging to master*: the word "merge"/"master" is still needed to
first send work to master, but amendments to the same work then stay there by
default.

## Verify the user's premises, hedged or assumed

When the user supplies a premise — hedged ("unless I'm mistaken," "if I'm
right," "check me") or carried in a conditional ("if nothing else calls this
…") — check it before acting on it or building on it:

- **Premise holds** → proceed, and confirm they were right.
- **Premise is wrong** → do *not* proceed on it. Point out what they
  overlooked, so the mistaken step never lands.

A confident assumption with no hedge word is still a premise to check, not a
fact to take on faith. When the premise carries an instruction, that
instruction is a go-ahead conditioned on it; when it's only stated, confirm or
correct it before relying on it.

Unlike a bare question (see *Questions are questions*), a premise is something
to verify and act on — just conditioned on its being true.

## One branch per session

A session's pushed work goes on a single feature branch by default, not a fresh
branch per sub-task. The session's first push creates it
(`claude/<date>/<topic>`); everything after lands on that same branch —
including changes that feel separate, like a conventions tweak made alongside
code.

Don't open a second branch for a side change mid-session. If a change genuinely
belongs on its own branch, the user will say so; the default is together.

Pairs with *Amendments inherit the last destination*.

## When a convention is ambiguous

Feel free to ask when it's genuinely unclear which convention applies, or how to
apply it.
