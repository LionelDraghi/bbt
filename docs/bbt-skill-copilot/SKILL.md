---
name: bbt-user
description: Create, improve, convert, validate, run, and integrate bbt scenarios for black-box CLI testing. Use when the user mentions bbt, behaviour scenarios, executable documentation, CLI test automation, tests scenario creation, or CI/CD integration for bbt.
license: CC-BY-NC-SA-4.0
compatibility: Requires bbt to be installed for execution tasks; authoring tasks require no runtime dependency.
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

# bbt skill for agents

Use this skill when the user wants to work with **bbt** scenarios: write them, convert existing documentation into them, check their syntax, run them locally, or integrate them in CI/CD.

bbt is both:

- a lightweight scenario format for CLI programs embedded in Markdown documentation;
- a command-line tool that executes those scenarios.

## Route the task

Choose the smallest relevant reference file. Do not load all references by default. Use progressive discovery: start from this file, then read only the reference needed by the user's intent.

### Read [authoring](references/authoring.md) when the user wants to

- create, write, improve, fix, refactor, or validate bbt scenarios;
- convert existing documentation, examples, README files, test notes, tutorials, or Gherkin scenarios into bbt-compatible scenarios;
- make documentation examples executable or checkable by bbt.

Typical requests:

- "make my README.md runnable"
- "make tests.md bbt compatible"
- "create a bbt tests suite from this documentation"
- "make examples provided in user_guide.md checkable by bbt"
- "turn examples from my documentation into executable tests"
- "convert this guide into bbt scenarios"
- "fix this bbt scenario"

### Read [execution](references/execution.md) when the user wants to

- run existing bbt scenarios
- check whether a Markdown file executes correctly with bbt
- understand or troubleshoot bbt command-line usage
- integrate bbt into CI/CD

Typical requests:

- "run my README.md"
- "fix my tests"
- "how do I run these bbt tests?"
- "create a GitHub Actions workflow for bbt"
- "why does this bbt run fail?"

### Inspect [examples](https://github.com/LionelDraghi/bbt/tree/main/docs/examples) when the user asks for complete examples

Use this link only when the user needs realistic, copyable, complete scenario files or when examples are useful to guide authoring.

If the request involves both conversion and execution, read `authoring.md` first, then `execution.md`.
