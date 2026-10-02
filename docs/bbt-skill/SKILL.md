---
name: bbt-skill
description: Create, convert, validate, run, and debug bbt scenarios - executable black-box CLI tests embedded in Markdown - and integrate them in CI/CD. Use when the user mentions bbt, wants to make documentation or examples executable, asks for CLI test scenarios, or needs to run or fix bbt tests.
license: CC-BY-NC-SA-4.0
compatibility: bbt must be installed for running, debugging, and CI/CD tasks; authoring requires no runtime dependency.
metadata:
  author: Lionel Draghi
  version: "1.0.0"
allowed-tools:
  - read
  - grep
  - bash
  - edit
  - write_file
  - ask_user_question
---

# bbt

`bbt` is both:
1. a format embedding test scenarios, written in almost natural English, inside Markdown documents;
2. a command-line tool that runs those scenarios against CLI programs.

## Route the task

Read only the reference needed by the user's intent. Do not load both by default.

### Authoring - read [references/authoring.md](references/authoring.md) when the user wants to

- create, write, improve, fix, refactor, or validate bbt scenarios;
- convert existing documentation, requirements, README instructions, user stories, or Gherkin scenarios into bbt scenarios;
- make documentation or examples executable.

Typical requests: "make my README runnable", "create a bbt test suite from this documentation", "convert this guide into bbt scenarios", "fix this bbt scenario".

### Running - read [references/running.md](references/running.md) when the user wants to

- install bbt;
- run or check existing bbt scenarios, understand command-line usage;
- debug a failing run;
- integrate bbt into CI/CD.

Typical requests: "run my README.md", "why does this bbt run fail?", "create a GitHub Actions workflow for bbt".

If the request involves both conversion and execution, read `running.md` first, then `authoring.md`.

## Get information at the source

Options, grammar, and examples evolve with each bbt release: never rely on copied documentation. Query bbt itself, or the online reference:

| Need | Command or link |
|------|-----------------|
| Syntax and file structure | `bbt help tutorial` |
| A complete starter scenario | `bbt help example` |
| Full step grammar, with examples | `bbt help grammar` |
| Step keywords | `bbt help keywords` |
| Commands and options | `bbt help`, then `bbt help <topic>` with topic in `filtering`, `matching`, `other`, `debug`, or `bbt help on_all` |
| What bbt understands from a file (dry run) | `bbt explain file.md` |
| Installation, issues, documentation | https://github.com/LionelDraghi/bbt |
