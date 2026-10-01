Use this reference when the user wants to install bbt, run existing scenarios, explain command-line usage, or integrate bbt into CI/CD.

# Installation and Setup

Check if bbt is already installed

```bash
bbt help
```

If not, install it.

## Installing bbt using the Alire package manager (Recommended method)

check if Alire is installed:

```bash
alr version
```
If not, refer to https://alire.ada.dev/docs/#installation for installing

Then:

```bash
alr install bbt
```

## Installing bbt using other installation methods

For other installation methods (AppImage for Linux or compiling from sources), see the [bbt GitHub repository](https://github.com/LionelDraghi/bbt#installation).


If bbt is still not available, ensure that it is in the PATH.
When installed with Alire, it should be in:
- Linux/macOS : ~/.alire/bin/
- Windows : %APPDATA%\alire\bin\ (soit C:\Users\<utilisateur>\AppData\Roaming\alire\bin\)

> Note for macOS (Darwin): On older versions, setting `GNAT_FILE_NAME_CASE_SENSITIVE=1` to avoid case sensitivity issues may be required.


## Execution principles

When running or proposing bbt commands:

1. Prefer the simplest valid command first.
2. Use selection, exclusion, recursive mode, matching mode, or cleanup options only when the task requires them.
3. For CI/CD, keep the workflow minimal and make installation steps explicit.
4. Do not assume the target operating system unless the user or repository context indicates it.

## Basic commands

Run scenarios in a Markdown file:

```bash
bbt README.md
```

Run scenarios recursively in a directory:

```bash
bbt --recursive .
```

Continue after failures:

```bash
bbt --keep_going README.md
```

Remove files created during the run when supported by the scenarios:

```bash
bbt --cleanup README.md
```

Run non-interactively when prompts may appear:

```bash
bbt --yes README.md
```

Explain what bbt understands before running:

```bash
bbt explain README.md
```

## Selecting scenarios

Run only scenarios whose title matches a string:

```bash
bbt README.md --select "Sanity Check"
```

Exclude scenarios whose title matches a string:

```bash
bbt README.md --exclude "Windows_Only"
```

## Matching modes

Human-oriented matching, usually the default, tolerates casing, whitespace, and empty-line differences:

```bash
bbt --human_match README.md
```

Exact matching should be used only when formatting is part of the expected behaviour:

```bash
bbt --exact_match README.md
```

## CI/CD guidance

Keep CI/CD integration small:

1. Install bbt.
2. Check the bbt version or help output.
3. Run the relevant Markdown files.
4. Use recursive mode only when the repository intentionally stores scenarios across several directories.

For a GitHub Actions template, see [github-actions-bbt.yml](../assets/github-actions-bbt.yml).
