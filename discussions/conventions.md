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
