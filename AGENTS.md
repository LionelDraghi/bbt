# Instructions for coding agents

This file is the entry point for coding agents working on bbt.

Keep changes focused, follow the existing project conventions, and use the
appropriate procedure below for each task.

## Mandatory rules

- Never stage, commit, push, tag, create a release or publish to Alire without
  the owner's explicit consent.
- `git add` is as forbidden as `git commit` and `git push` until the owner gives
  the corresponding go-ahead.
- If a build, test, documentation, cleanup or publication step fails, stop and
  report the failure. Do not continue with later steps.
- Do not include unrelated changes, temporary files or unwanted test
  artefacts.
- Check repository cleanliness on the file system, not only with `git status`:
  Git-ignored files remain invisible.
- The repository uses LF line endings on every platform, including Windows.
  This is enforced by `.gitattributes`; do not override it locally.

## Development tasks

For source changes, bug fixes, documentation changes, tests and other normal
development work, follow:

```text
docs/dev/development_workflow.md
```

This procedure covers:

- the normal development and test loop;
- documentation, changelog and Fixme updates;
- source review;
- cleanup;
- owner approval;
- final validation;
- commit and push.

## Release tasks

For version selection, release preparation, Git tagging, GitHub releases and
Alire publication, follow:

```text
docs/dev/release_procedure.md
```

Do not simply remove the `-dev` suffix. The release version must be proposed
according to SemVer and the contents of the current changelog section, then
confirmed by the owner.

## Build pointers

- `make build` builds bbt in validation mode and the project tools.
- `make tools` builds `sut` and `rpl`, and creates the `sut`, `bbt` and `gcc`
  links in `tests/`.
- Run `make tools` after a fresh clone or when these links are missing.
- On Windows, these links are copies. They are refreshed by the tests setup
  target and by `make build`, through the `refresh_bbt` target after linking.
- `make clean` removes test-run artefacts while keeping built binaries and
  test links usable.
- `make distclean` removes everything that can be rebuilt, including binaries
  and links.

## Project documentation

- Understanding bbt: `docs/bbt-skill`
- Design and tests: `docs/dev/developer_guide.md`
- Design discussions: `docs/dev/design_discussions.md`
- Development workflow: `docs/dev/development_workflow.md`
- Release procedure: `docs/dev/release_procedure.md`
- Proposed features: `docs/proposed_features`
- Fixme index: `docs/dev/fixme_index.md`
- Project to-do list: the TDL chapter of `docs/dev/project.md`

bbt is tested mostly with bbt.