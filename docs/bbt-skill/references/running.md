# Running bbt

Use this reference to install bbt, run existing scenarios, debug a failing run, or integrate bbt into CI/CD.

## Install

```bash
alr version       # is Alire (the Ada package manager) installed?
alr install bbt   # recommended installation method
bbt help          # check that bbt is in the PATH
```

For other installation methods (AppImage, building from sources), see the [repository](https://github.com/LionelDraghi/bbt#installation).

## Running

- Discover commands and options with `bbt help`; do not assume them from memory.
- Prefer the simplest command that does the job; add options (`--recursive`, `--select`, `--exclude`, ...) only when the task requires them.
- If some scenarios are specific to a platform, or are executed differently on it, mark them with a tag such as `Unix_Only` or `Windows_Only`, and run only the scenarios relevant to the current platform (see `bbt help filtering`).

## Debugging

First determine what is being debugged: the program under test, or the scenario.

- If the request is about the **program**: run the scenario, diagnose the program, fix the program. Never modify the scenario to make it pass - the scenario is the specification.
- If a **manifest scenario error** appears (wrong keyword, typo, stale expected value), do not fix it silently: propose the modification to the user, and apply it only with their agreement.
- To debug a scenario itself: `bbt explain file.md`, then `bbt --verbose file.md`; run the commands manually; inspect the files created during the run (see `bbt help debug`).

## CI/CD

Keep it minimal: install bbt, run the scenario files. A starter GitHub Actions workflow is provided in [assets/github-actions-bbt.yml](../assets/github-actions-bbt.yml); adapt it to the target OS and to the filtering needs.
