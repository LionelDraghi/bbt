# Instructions for coding agents

This file is the entry point for coding agents working on bbt.

## Non-negotiable rules

- Never stage, commit, push, tag, create a release or publish to Alire without
  the owner's explicit consent.
- `git add`, `git commit` and `git push` are forbidden until the owner gives
  the corresponding go-ahead.
- If a build, test, documentation, cleanup or publication step fails, stop and
  report the failure.
- Do not include unrelated changes, temporary files or unwanted test artefacts.
- Check repository cleanliness on the file system, not only with `git status`.
- The repository uses LF line endings on every platform, including Windows.

## Choose the right procedure

- Development work, bug fixes, doc updates, tests, cleanup, review: use
  `docs/dev/development_workflow.md`
- Release versioning, tagging, GitHub release, Alire publication: use
  `docs/dev/release_procedure.md`

## Build pointers

- `make build` builds bbt and project tools.
- `make tools` builds `sut` and `rpl`, and refreshes the test links in `tests/`.
- `make clean` removes test artefacts while keeping built binaries and links.
- `make distclean` removes everything that can be rebuilt.

## Task triage

When the user asks “what should we do now?” or “what is the priority right now?”,
check the project backlog and tracked work before proposing new work:

- `docs/dev/issues_index.md` for tracked issues;
- `docs/proposed_features` for candidate ideas not yet decided;
- `docs/dev/project.md` for the project TDL and current directions;
- `docs/dev/fixme_index.md` for actionable fixes and cleanup items;
- `docs/dev/design_discussions.md` for decisions that constrain the next step.

Use these sources to ground the answer in the repository’s existing work. Do not
invent a new task before checking whether the need is already tracked elsewhere.

## Useful references

- `docs/bbt-skill`
- `docs/dev/developer_guide.md`
- `docs/dev/design_discussions.md`
- `docs/dev/fixme_index.md`
- `docs/proposed_features`

bbt is tested mostly with bbt.