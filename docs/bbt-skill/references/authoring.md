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

A precise, focused expectation says what the test tests: prefer it to a
comparison with a full reference output, where the whole output is
asserted at once - including details nobody deliberately specified - and
where a change in an unrelated part of the output breaks the test.
Comparing the whole output against a reference file - a "golden file" -
is the technique called snapshot testing: a last resort, then. Keep
expected results explicit and readable:

- keep them in clear text in the scenario file, as an inline string or a fenced code block;
- check the specific part you care about with `is`, `contains` or `matches`, not the whole output;
- only when the expected result is too long to stay readable, move it to an external expected-result file - a "golden file" - and check it with the bbt file-to-file comparison steps (exact syntax: `bbt help grammar`);
- never modify a golden file without the user's explicit agreement. A mismatch means either a regression in the program under test, or a deliberate behavior change: only the user can decide.

## Frequent pitfalls

Most structural mistakes are visible in `bbt explain` output. Check that:
- steps use the `-` list marker, not `*` or `+`, and start with `Given`, `When` or `Then`, never `And` or `But`;
- commands and short parameters are between backticks, multiline parameters in a fenced code block;
- a parameter between backticks cannot itself contain a backtick: the first one closes the parameter, and the rest of the line is misparsed, with possibly no visible error; when a parameter must contain a backtick character, use a fenced code block instead of inline backticks;
- the same trap corrupts the rendering of the prose around scenarios: quoting a bbt step in single backticks while the step itself contains backticks (most do) leaves an unclosed code span that swallows the document until the next backtick, often far away; wrap such quotes in double backticks, as in ``When I run `cmd` ``, and use `` ``` `` for a literal fence mark in prose;
- a code block immediately follows its step, with no blank line in between;
- no bbt step keyword appears in free text outside scenarios.
