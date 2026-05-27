# Discussion File Conventions

This folder keeps compact notes from design sessions.

## Naming

Discussion files should be named so chronological order is visible from the
filename, even when different assistants or collaborators are involved.

Use:

```text
discussion-YYYY-MM-DD-<source>-N.md
```

where:

- `YYYY-MM-DD` is the session date.
- `<source>` is the main collaborator/source, for example `claude` or
  `chatgpt`.
- `N` is the chronological session number for that date.

Examples:

```text
discussion-2026-05-27-claude-1.md
discussion-2026-05-27-claude-2.md
discussion-2026-05-27-claude-3.md
discussion-2026-05-27-chatgpt-4.md
```

If an older file is missing a number, prefer adding a numbered replacement or
renaming it when convenient, rather than leaving chronology implicit.

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
