# Authoring bbt scenarios

Use this reference to create, improve, validate, or convert bbt scenarios.

## Authoring principles

When creating or transforming scenarios:

1. Preserve the user's intent and existing wording where possible.
2. Prefer simple Markdown scenarios with clear Given / When / Then structure.
3. Make commands executable, explicit, and deterministic.
4. Avoid inventing expected outputs that are not provided by the user or source document.
5. If expected output is unknown, add a clearly marked placeholder or ask the user to confirm it after producing the best effort.

## Output expectations

For authoring tasks, return valid Markdown that can be saved and executed by bbt.

For conversion tasks, include only the converted scenario unless the user requests commentary.

## Scenario shape

A bbt scenario is written in Markdown and normally follows this pattern:

```markdown
## Scenario: short scenario title

- Given some initial context
- When I run `command --option value`
- Then the output contains `expected text`
```

The title should describe the behaviour being checked, not the implementation detail.

## Organising scenarios

When several scenarios describe a coherent capability, organise them like Gherkin features by grouping them under a Markdown feature heading.

Use a `Background` section when multiple scenarios share the same setup, preconditions, input files, or context. Keep the background short and only include information that is genuinely common to all scenarios in the feature.

Example structure:

```markdown
# Feature: command-line help

## Background

- Given the application is available as `mytool`

## Scenario: help lists available commands

- When I run `mytool --help`
- Then the output contains `commands`

## Scenario: version option prints a version

- When I run `mytool --version`
- Then the output contains `mytool`
```

Prefer this structure only when it improves readability. For one-off checks, a single scenario is enough.

## Writing rules

- Keep one behaviour per scenario.
- Use explicit commands in backticks.
- Prefer stable outputs over volatile values such as timestamps, absolute paths, random IDs, or environment-specific versions.
- If a command creates files, make the file names explicit.
- If a scenario depends on input files, describe or create them before the command is run.
- When exact output is required, use exact matching only when whitespace, casing, and blank lines are intentionally meaningful.

## Conversion from existing text

When converting a README, user guide, Gherkin feature, test note, or manual procedure:

1. Identify candidate behaviours to check. Good candidates include sections named `example`, `scenario`, `usage`, or `test`; quoted command lines; fenced shell blocks; command/output pairs; and explicit descriptions of expected results.
2. Extract the command the user should run.
3. Extract the observable result.
4. Rewrite the result into a bbt-compatible Then step.
5. Keep explanations outside the scenario unless they are needed as setup.

## Do not fabricate assertions

If the source document says "the command displays the result" but does not state the result, do not invent it. Use a placeholder such as:

```markdown
- Then the output contains `<expected text to confirm>`
```

## Common transformations

Natural language:

```text
Run mytool --help. It should mention the --verbose option.
```

bbt scenario:

```markdown
## Scenario: help lists verbose option

- When I run `mytool --help`
- Then the output contains `--verbose`
```

Gherkin:

```gherkin
Scenario: show version
  When I run app --version
  Then I see 1.2.3
```

bbt scenario:

```markdown
## Scenario: show version

- When I run `app --version`
- Then the output contains `1.2.3`
```
