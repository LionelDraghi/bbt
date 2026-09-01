# Authoring bbt scenarios

Use this reference to create, improve, validate, or convert bbt scenarios.

## Authoring principles

When creating or transforming scenarios:

1. Preserve the user's intent and existing wording where possible.
2. Prefer simple Markdown scenarios with clear Given / When / Then structure.
3. Make commands executable, explicit, and deterministic.
4. Avoid inventing expected outputs that are not provided by the user or source document.
5. If expected output is unknown, add a clearly marked placeholder or ask the user to confirm it after producing the best effort.
6. Reuse the source text as much as possible; only modify to fit `bbt` syntax requirements.

## Creating a `bbt` scenario

The output is either a new Markdown file (or files) when asked to create a `bbt` scenario, or a modified source file.
The source can be any text file: README, user guide, Gherkin feature, test note, manual procedure, or other text-based document.
Unless the user specifies otherwise, modifications should be limited to what is necessary to execute the scenario.
Unless the user specifies otherwise, modifications are made in place within the original file.

1. **Identify candidate behaviors to check**:
   Good candidates include sections named `example`, `scenario`, `usage`, or `test`; or containing quoted command lines, fenced shell blocks, command/output pairs, and explicit descriptions of expected results.

2. **Analyze the source** for:
   - Key actions (commands to run)
   - Inputs (files, parameters)
   - Expected outputs (results, files, messages)
   - Preconditions (setup required)

3. **Identify the scenario structure**:
   - One scenario per **behavior** or **test case**
   - Group related scenarios under a **Feature** header

4. **Map to bbt syntax**:
   - **Setup/Precondition** → `Given`
   - **Action/Execution** → `When`
   - **Verification/Result** → `Then`
   - **Continuation** → `And` or `But`
   
   Followed by:
   - **Command execution** → Use `run` or `successfully run`
   - **File operation** → Use file-related keywords
   - **Output verification** → Use `get`, `contains`, `matches`
   - **Error handling** → Use `get error`, `I get an error`

   And parameters:
   - **Short text (single line)** → Use inline backticks
   - **Multiline content** → Use a code block after the step
   - **File reference** → Use filename in backticks

5. **Add context**:
   - Use descriptive scenario names.
   - Add tags for platform-specific tests.
   - Include comments (ignored by `bbt`) for clarity.
   - Keep the original text that is not converted to `bbt` syntax as a comment.

6. **Check created or modified files**:
   - Run `bbt explain file.md` and check `bbt` understanding of the tests.
 
   
## Writing rules

- Keep one behavior per scenario.
- Group scenarios under a Feature header when relevant.
- Factor out common preconditions for multiple scenarios using Background.
- Use explicit commands in backticks.
- Prefer stable outputs over volatile values such as timestamps, absolute paths, random IDs, or environment-specific versions.
- If a command creates files, make the file names explicit.
- If a scenario depends on input files, describe or create them before the command is run.
- When exact output is required, use exact matching only when whitespace, casing, and blank lines are intentionally meaningful.
- Do not fabricate assertions: if the source document says "the command displays the result" but does not state the result, do not invent it. Use a placeholder such as:   
```markdown
- Then the output contains `<expected text to confirm>`
```
- Do not check the same feature twice: if the source mentions the same feature twice, notify the user and explain how you factored it.
- Do not use `bbt` keywords in the free text part of the steps.


## Common transformation examples

### From Natural language:

```text
Run mytool --help. It should mention the --verbose option.
```

bbt scenario:

```markdown
## Scenario: help command should expose the verbose option

- When I run `mytool --help`
- Then the output contains `--verbose`
```

### From Gherkin:

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


## 📚 Transformation Guides

According to the context, refer if relevant to the following detailed instructions:

| **Source** | **Description** | **File** |
|------------|----------------|----------|
| **Requirements** | Transform formal requirements to bbt | [from-requirements.md](./from-requirements.md) |
| **README Instructions** | Convert README documentation to bbt | [from-readme.md](./from-readme.md) |
| **User Stories** | Transform user stories to bbt scenarios | [from-user-stories.md](./from-user-stories.md) |


## Scenario syntax

The structure and syntax of a `bbt` scenario are described in the [tutorial](https://github.com/LionelDraghi/bbt/blob/main/docs/help/tutorial.md).

**Code Block Nesting Rules:**

When writing scenarios that contain code blocks, follow these rules:

1. **Outer code block**: Use `~~~` with language specifier for markdown examples.
2. **Inner code blocks**: Use \`\`\` with language specifier for actual `bbt` code blocks.
3. **Maximum nesting**: Never exceed 2 levels.

## Common Mistakes to Avoid

1. **Starting with `And` or `But`**
   - ❌ `- And given there is a file`
   - ✅ `- Given there is a file`

2. **Missing backticks**
   - ❌ `- When I run gcc --version`
   - ✅ `- When I run `gcc --version`

3. **Incorrect code block nesting**
   - ❌ Adding blank lines between step and code block.
   - ✅ Code block immediately follows step.

4. **Using bbt keywords in decorative text**
   - ❌ `This test Given a file...`
   - ✅ `This test requires a file...`



