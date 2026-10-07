# Instructions for coding agents

## Commit discipline

- never stage, commit or push without the owner's explicit consent:
  `git add` is as forbidden as `git commit` and `git push`; prepare the
  change, run a full `make all` (build, sut, check, doc), a `make clean` to check
  that there is no remaining unwanted file, and report the result;
  wait for the go-ahead before touching the index or the history
- once the owner gives the go-ahead, run the whole sequence in one go:
  `git add` the whole generated state (results, badges,
  indexes...), commit, and push. Committing in the middle of the chain
  (e.g. after features only) freezes inconsistent artifacts, such as
  a badge.url still holding the bbt placeholder, or a stale badge.svg

## Build

- `make build` to build bbt and tools

- `make tools` to build sut and rpl, and to create the sut, bbt and gcc
  links in tests/; run it after a fresh clone, or when those links are missing
- on Windows, those links are in fact copies, refreshed by the tests
  `setup` target (a prerequisite of every suite) and by `make build`,
  that refreshes tests/bbt via the `refresh_bbt` target after linking

## Test procedure

- to run a specific test       : `cd tests && ./bbt <test_file>`
- to run sanity checks (rapid) : `cd tests && ./bbt ../docs/examples`
- to run functional tests      : `cd tests && ./bbt ../docs/features`
- to run one suite             : `cd tests && make features | examples | non_reg | unit_testing`
- add the `--exclude Windows_Only` option when on Linux/MacOS
- add the `--exclude Unix_Only` option when on Windows
- `make clean` removes the test run artifacts only, keeping the built
  binaries and links usable; `make distclean` removes everything
  that can be rebuilt, including binaries and links
- after `make clean` or `make distclean`, verify cleanliness on the file
  system, not only with git status: git ignored files
  (input.*, expected_*, cp...) remain invisible
- files created by `When I run` steps (e.g. binaries compiled by gcc) are
  not tracked by --cleanup, which only tracks files created in `Given` steps

## Changing a feature or an error message format

- feature files in docs/features are the specification (TDD first)
- avoid snapshot testing (cf. bbt-skill), especially for generated files
- add a line in docs/changelog.md under the current -dev version
- keep the changelog entries short: one line announcing the change, with a
  reference to the feature file or the issue for the details
- `Fixme:` comments in docs/ and src/ are indexed in docs/dev/fixme_index.md by `make doc`
- examples and generated docs must not depend on the machine or on the locale:
  no hard version number (use a regexp or a fixed behavior), and set
  `Given the environment variable LC_ALL is set to C` when a tool message is checked
- files in docs/help/ are embedded in bbt at build time (External_Initialization):
  rebuild before testing `bbt help`, and regenerate the reference files
  (docs/examples/gcc_hello_world.md)
  after a change, otherwise B130 fails
- to show a whole step between backticks in the docs, with the inner
  backticks visible (e.g. When I type `Y`), use a double backtick code span,
  with a space before the closing delimiter: ``When I type `Y` ``;
- align the tables in Markdown files with spaces, so that they stay readable
  in plain text form; generated files (docs/grammar.md, docs/keywords.md...)
  are exempt, their format is up to the generator
- on any functional evolution, do not forget the possible update of the
  tutorial and of the example in help (docs/help/tutorial.md,
  docs/help/example.md and docs/examples/gcc_hello_world.md)

## Design discussions

- design subjects are discussed in docs/dev/design_discussions.md, one
  entry per subject, with its status: under discussion, or arbitrated; an
  arbitrated entry states the decision, the rejected alternatives and
  why, and references the PR or discussion where the subject was
  elaborated
- the document header holds a table of the entries, sorted by status,
  the entries under discussion first
- a reference to an entry from the code is welcome when it helps the
  understanding
- the developer guide keeps only the operational descriptions: when a
  discussion of alternatives ends up there, move it to the design
  discussions
- an idea not yet decided goes to docs/proposed_features, with an entry
  in the TDL chapter of docs/dev/project.md; it can be short, just enough
  to capture the need, or already polished as a docs/features file,
  scenarios included, ready to be moved there when the decision is
  made (cf. docs/proposed_features/timeouts.md)

## Alire and version numbering

- the pre-release suffix must be dash separated (0.3.1-dev), not dot (0.3.1.dev),
  otherwise alr cannot load the workspace
- after a version change in alire.toml, `alr update` regenerates Crate_Version;
  `alr build` alone does not
- the full release procedure is in docs/dev/release_procedure.md

## Pointers

- to understand bbt: docs/bbt-skill
- design and tests: docs/dev/developer_guide.md
- design discussions: docs/dev/design_discussions.md
- to do list: docs/proposed_features, docs/dev/fixme_index.md,
  chapter TDL in docs/dev/project.md
- bbt is tested mostly with bbt
