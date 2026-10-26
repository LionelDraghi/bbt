# Instructions for coding agents

This file is the entry point for coding agents working on bbt.

**Before any other action, read the shared template referenced below
and follow it** -- local copy at `../ada-project-template/AGENTS.template.md`
when present, otherwise fetch the GitHub URL.

The shared rules for Lionel's Ada projects live in the [reference
repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/AGENTS.template.md)
and apply here in full:
non-negotiable git rules (in brief: no `git add`, commit, push, tag,
release or Alire publish without the owner's explicit, post-review
consent), procedure choice, task triage, reference
policy, skills, Ada tooling (Serena MCP first), and the definition of
done. This file keeps only what
is specific to bbt. The repository uses LF line endings on every
platform, including Windows.

## Build pointers

The shared build pointers apply ([reference repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/AGENTS.template.md)); `make help` is the documentation of record. bbt specifics:

- `make build` builds bbt in validation mode; the tools are built by `make tools`, not by `make build`.
- `make tools` builds `sut` and `rpl`, and refreshes the test links in `tests/`.
- `make doc` regenerates the generated docs (help reference files, fixme and issues indexes, badges) and checks the links with mlc.
- `make install` copies bbt to `~/bin`.
- Scenario sources live in `docs/features/` and `docs/examples/`; `tests/` is the run directory, and also hosts the unit and non-regression suites (`expect_test`, `file_utilities_tests`, `non_reg_tests`, `spawn_test`, `utilada_test`).
- Deviation on `clean`: `make clean` removes the test-run artefacts only, keeping the built binaries and links usable; `make distclean` matches the shared full clean (everything that can be rebuilt).

## Test procedure

- To run a specific test: `make A300` (where `A300` is the id that starts
  the test file name). It runs the scenario with the right options for
  the platform and prints the exact command line; grab that line to
  rerun or tweak it, e.g. replace `./bbt` with `./bbt explain`, or add
  `-d` for debug traces.
- To run sanity checks (rapid): `cd tests && ./bbt ../docs/examples`
- To run functional tests: `cd tests && ./bbt ../docs/features`
- Add the option `--exclude Windows_Only` when you are on Linux/macOS.
- Add the option `--exclude Unix_Only` when you are on Windows.

## Conditional readings

Read a document only when the task requires it; do not open them all up front.

- To understand and use bbt: `docs/bbt-skill` — this is the bbt agent
  skill, published from this repository and installed from GitHub
  (`npx skills add LionelDraghi/bbt --skill bbt-skill`)
- To prepare or run a release: the [common release procedure on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/release_procedure.md), with the bbt parameters and additions in `docs/dev/release_procedure.md`
- To choose the next task or find already-tracked work:
  `docs/dev/issues_index.md`, `docs/dev/project.md`,
  `docs/dev/fixme_index.md`, `docs/proposed_features`
- To review or challenge the design: `docs/dev/design_discussions.md`
- To follow the developer workflow: `docs/dev/developer_guide.md`

bbt is tested mostly with bbt.
