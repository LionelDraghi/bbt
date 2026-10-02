# Authoring bbt scenarios

Use this reference to create, improve, validate, or convert bbt scenarios.

## Workflow

1. Identify in the source material (requirement, README, user story, Gherkin, manual procedure): the actions, the inputs, the expected results, the preconditions.
2. Write one scenario per behavior: setup with `Given`, action with `When`, expected result with `Then`, continuations with `And` or `But`. Reuse the source wording as much as possible.
3. Validate what bbt understands: `bbt explain file.md`.
4. Run: `bbt file.md`, fix, rerun.

## Principles

- One scenario = one behavior. Group related scenarios under a `Feature`, and factor common preconditions into a `Background`.
- Do not invent expected results. If the source does not state the expected output, insert a clearly marked placeholder and ask the user to confirm it.
- Prefer stable checks over volatile values (timestamps, absolute paths, environment-specific versions).
- Add filtering tags for platform-specific scenarios (e.g. `Windows_Only`, `Unix_Only`); see `bbt help filtering`.

## Expected results and golden files

- Expected results must be explicit and readable: keep them in clear text in the scenario file, as an inline string or a fenced code block.
- Only when the expected result is too long to stay readable, move it to an external expected-result file - a "golden file" - and check it with the bbt file-to-file comparison steps (exact syntax: `bbt help grammar`).
- Never modify a golden file without the user's explicit agreement. A mismatch means either a regression in the program under test, or a deliberate behavior change: only the user can decide.

## Frequent pitfalls

Most structural mistakes are visible in `bbt explain` output. Check that:
- steps use the `-` list marker, not `*` or `+`, and start with `Given`, `When` or `Then`, never `And` or `But`;
- commands and short parameters are between backticks, multiline parameters in a fenced code block;
- a code block immediately follows its step, with no blank line in between;
- no bbt step keyword appears in free text outside scenarios.
