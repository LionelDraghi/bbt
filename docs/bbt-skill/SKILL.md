---
name: bbt-user
description: |
  Reference guide for LLM to generate valid bbt scenarios from natural language with proper syntax and structure, and to run them.
  Includes installation, CLI usage, debugging, and CI/CD integration.
license: CC-BY-NC-SA-4.0
metadata:
  author: lionel-draghi
allowed-tools:
  - read
  - grep
  - bash
  - edit
  - write_file
  - ask_user_question
---

# Purpose
`bbt` is both:
1. a format for embedding test scenarios in almost natural english within Markdown documentation;
2. a tool to run those tests.

---

# Installation and Setup

## Prerequisites

Nore if the [Alire](https://alire.ada.dev/) (the Ada package manager) is installed.

> Note for macOS (Darwin): On older versions, set `GNAT_FILE_NAME_CASE_SENSITIVE=1` to avoid case sensitivity issues.

## Installation Methods

Recommended method: Install bbt using the Alire package manager:
```bash
alr install bbt
bbt --version
```
For other installation methods (AppImage for Linux or compiling from sources), see the [bbt GitHub repository](https://github.com/LionelDraghi/bbt#installation).


---

# Running Tests with bbt

## Basic Commands

| Command | Description | Example |
|---------|-------------|---------|
| `bbt` | Run tests in file/directory | `bbt README.md` |
| `bbt explain` | Dry run bbt against a scenario | `bbt explain my_test.md` |
| `bbt help` | Display general help | `bbt help` |
| `bbt help grammar` | Display complete grammar | `bbt help grammar` |
| `bbt help example` | Generate an example scenario | `bbt help example > test.md` |
| `bbt help tutorial` | Generate a comprehensive tutorial | `bbt help tutorial` |

## Common Options

| Option | Description | Example |
|--------|-------------|---------|
| `--recursive` | Run tests recursively | `bbt -r .` |
| `--exclude <tag>` | Exclude scenarios with a tag | `bbt tests/ --exclude Windows_Only` |
| `--include <tag>` | Include only scenarios with a tag | `bbt tests/ --include Smoke` |
| `--verbose` | Verbose mode | `bbt --verbose my_test.md` |
| `--stop-on-error` | Stop at first failure | `bbt --stop-on-error tests/` |
| `--keep_going` | Continue after errors | `bbt --keep_going tests/` |
| `--cleanup` | Remove temporary files after test | `bbt --cleanup tests/` |
| `--yes` | Batch mode (auto-answer yes) | `bbt --yes tests/` |
| `--select <name>` | Run only a specific scenario | `bbt README.md --select 'Sanity Check'` |
| `--human_match` | Ignore case and whitespace (default) | `bbt --human_match tests/` |
| `--exact_match` | Require exact output match | `bbt --exact_match tests/` |

## Typical Project Structure
```
my_project/
├── README.md
├── docs/
│   └── scenarios.md
└── tests/
    └── features.md
```

## Execution Examples
```bash
# Test a specific file
bbt README.md

# Test all files in a directory
bbt docs/
bbt tests/

# Test recursively
bbt -r .

# Exclude tags (for CI on Windows systems)
bbt tests/ --exclude Unix_Only

# Include only specific tags
bbt tests/ --include Smoke --include Regression

# Run a specific scenario
bbt README.md --select 'Version Check'
```

# Debugging bbt Tests

- Verify that `bbt` correctly identify the scenarios and steps in your file
  ```bash
  bbt explain my_test.md
  ```
  If a step doesn't appear, verify:
  - Dash `-` is used (not `*` or `+`)
  - Keyword (`Given/When/Then/And/But`) is present
  - Parameters are in backticks or code blocks

- Run in Verbose Mode

- Check the Temporary Files (run without `--cleanup` to keep them) 

## Frequent Errors

### Error 1: Missing Backticks
```markdown
# Wrong
- When I run gcc --version

# Correct
- When I run `gcc --version`
```

### Error 2: Incorrect Code Block Markers
~~~markdown
# Wrong (using 1 or 2 backticks)
- Then the output contains `
hello
`

# Correct (using 3 backticks)
- Then the output contains
  ```
  hello
  ```
~~~

### Error 3: Wrong List Marker
```markdown
# Wrong
* When I run `ls`

# Correct
- When I run `ls`
```

---

# CI/CD Integration

## GitHub Actions
```yaml
- name: Install Alire
  run: |
    curl -fsSL https://alire.ada.dev/download | bash
    echo "$HOME/.alire/bin" >> $GITHUB_PATH
- name: Install bbt
  run: alr install bbt
- name: Run bbt tests
  run: bbt docs/features/
```

## GitLab CI
```yaml
test:
  script:
    - alr install bbt
    - bbt docs/features/ --exclude Windows_Only
```

---

# Writing Tests with bbt Format

`bbt` scenarios are characterized by a Gherkin structure, embedded within structured text file, mainly Markdown, but also restructured text and Asciidoc.

`bbt help tutorial` generates a comprehensive description of the file structure, also available at https://github.com/LionelDraghi/bbt/blob/main/docs/tutorial.md

Here is a file example, with comments starting with "-->" at the end of each line:

~~~markdown
# gcc simple sanity tests  --> ignored by bbt

## Scenario: gcc version?  --> scenario header

- When I run `gcc -v` --> Step with a parameter between backticks

* on Linux or Windows, the output is something like: --> ignored by bbt
  > gcc version 14.2.0 (Debian 14.2.0-16)  --> ignored by bbt

- Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*` --> Step with regexp

## Scenario: compiling and executing an hello word --> start another scenario

Sanity check of a complete compile / link / run sequence: --> ignored by bbt

- Given the new file `main.c` containing --> Step with parameter + code block
  ```c
  #include <stdio.h>
  int main() {
  printf("Hello, World!");
  return 0;
  }
  ```
- And given there is no `./main` file --> Step
- When I successfully run `gcc main.c -o main` --> step
- And I run `./main` --> Step
- Then the output is `Hello, World!` --> Step
~~~

## Parameter Formatting

**Inline parameters (backticks):**
- Commands: `gcc --version`
- Filenames: `output.txt`
- Short text: `success`

**Multiline parameters (code blocks):**
- File contents
- Expected multiline output
- Command scripts

Example:
~~~markdown
- Given the file `script.sh`
```bash
#!/bin/bash
echo "Hello"
```
~~~

Common step patterns:
- Command execution: `- When I run `command` `
- File creation: `- Given the file `name` containing `content` `
- Output verification: `- Then output contains `expected` `
- Error checking: `- Then I get an error`

---

# Grammar Reference Table

For a complete grammar reference with exemples, run:

```bash
bbt help grammar
```

---

# Common Scenario Patterns

## Command Execution

**Basic command:**
```markdown
- When I run `command --args`
- Then I get no error
```

**Command with expected output:**
```markdown
- When I run `gcc --version`
- Then output contains `14.2.0`
```

**Successful execution:**
```markdown
- When I successfully run `make`
- Then there is a file `program.exe`
```

## File Operations

**File creation with content:**
```markdown
- Given the file `input.txt` containing `test data`
```

**File creation with multiline content:**
~~~markdown
- Given the file `config.ini`
```ini
[key]=value
[section]
option=setting
```
~~~

**File existence check:**
```markdown
- Given there is a file `data.txt`
- Then file `data.txt` contains `expected content`
```

**File comparison:**
```markdown
- Then file `output.txt` is equal to file `expected.txt`
```

## Output Verification

**Exact output match:**
```markdown
- Then I get
```
Expected output line 1
Expected output line 2
```
```

**Partial output match:**
```markdown
- Then output contains `success`
```

**Output from file:**
```markdown
- Then output contains file `expected_pattern.txt`
```

**Regex matching:**
```markdown
- Then output matches `Error: .*`
```

## Error Handling

**Error detection:**
```markdown
- When I run `invalid_command`
- Then I get error
- And output contains `File not found`
```

**No error verification:**
```markdown
- When I successfully run `valid_command`
```

---

# Natural Language Transformation

When asked to "make a file runable" or "make a file bbt compatible", check within the file 
- section with "Scenario" or "Example" in the title
- command line
- description of input and output
And then try to transform the content into a bbt scenario, using the grammar and patterns described above.

When asked to make a bbt test from a requirement, a README instruction, or a user story :
- try to identify the key actions, inputs, and expected outputs, 
- structure them into a bbt scenario format. 
- Organize the scenarios in features if needed, 
- identify common prerequisite and use Backgrouns as needed
- use tags to indicate platform specific test
- ensure that the steps are clear, concise, and follow the bbt syntax
- reuse the provided wording as much as possible, while ensuring that the resulting scenario is valid and executable by bbt.

## From Requirements to bbt

**Requirement:** "System shall display version when --version flag is used"

**Transformation:**
```markdown
## Scenario: Version flag displays correct version

- When I run `program --version`
- Then output contains `1.0.0`
- And I get no error
```

## From README to bbt

**README instruction:** "To compile: 1. Create source file, 2. Run compiler, 3. Verify output"

**Transformation:**
~~~markdown
## Scenario: Basic compilation workflow

- Given the file `program.adb` containing
```ada
with Ada.Text_IO;
procedure Program is
begin
   null;
end Program;
```
- When I successfully run `gcc -c program.adb`
- Then there is a file `program.o`
- And I get no error
~~~

## From User Stories to bbt

**User Story:** "As a developer, I want to validate my JSON files so that I can catch syntax errors early"

**Transformation:**
~~~markdown
## Scenario: Valid JSON file passes validation

- Given the file `valid.json` containing
```json
{
  "name": "test",
  "value": 42
}
```
- When I run `json_validator valid.json`
- Then I get no error
~~~

## Scenario: Invalid JSON file fails validation

~~~
- Given the file `invalid.json` containing `{"name": "test",}`
- When I run `json_validator invalid.json`
- Then I get an error
- And output contains `Syntax error`
~~~
---

# Best Practices

## Writing Maintainable Tests

1. **One scenario = one behavior** - Avoid scenarios with more than 5-6 steps
2. **Use descriptive names**
   - Bad: `### Scenario: Test 1`
   - Good: `### Scenario: Login with valid credentials`
3. **Group related tests** - Use `# Feature: Authentication` to group scenarios
4. **Document context** - Add explanations outside sections recognized by bbt
5. **Use tags wisely** - e.g., `@Smoke`, `@Regression`, `@Windows_Only`, `@Unix_Only`

## File Organization
```
my_project/
├── README.md                    # Basic examples
├── docs/
│   ├── user_guide.md            # Documentation + tests
│   └── examples/                # Advanced examples
└── tests/
    ├── features/                # Functional tests
    │   ├── auth.md
    │   ├── processing.md
    │   └── errors.md
    └── regression.md             # Regression tests
```

---

# Checklist Before Committing a Test

- [ ] Scenario has a **descriptive name**
- [ ] Steps use **valid keywords** (`Given/When/Then/And/But`)
- [ ] Arguments are **in backticks** (simple) or **fenced code blocks** (multiline)
- [ ] File paths are **relative to the `.md` file**
- [ ] Test passes locally (`bbt my_test.md`)
- [ ] Test passes with `--verbose`
- [ ] Tags are added if needed (`[Unix_Only]`, `[Smoke]`)
- [ ] File is **valid Markdown**

---

# Recommended Workflow

## 1. Write a New Test
1. Create a `.md` file (e.g., `tests/my_feature.md`)
2. Write the scenario in natural English
3. Verify syntax: `bbt explain tests/my_feature.md`

## 2. Debug the Test
1. Run in verbose mode: `bbt --verbose tests/my_feature.md`
2. If it fails:
   - Test commands manually
   - Fix the scenario

## 3. Integrate into Project
1. Add the file to the Git repository
2. Run all tests: `bbt tests/`
3. (Optional) Add a Git hook to run bbt before commit

## 4. Maintain Tests
1. Update scenarios when behavior changes
2. Add tags for specific test types
3. Regularly verify with: `bbt tests/ --include Regression`

---

# Practical Exercises

## Scenarios to Write

1. **Basic test**: Verify that `echo "Hello"` returns `Hello`

2. **File test**:
   - Create a file `test.txt` with content `"Ada"`
   - Verify that `cat test.txt` returns `"Ada"`

3. **Error test**: Verify that `ls nonexistent_file` returns an error code

4. **Multi-step test**:
   - Create a file `input.csv`
   - Run a script that processes it
   - Verify that `output.csv` is created with the correct content

---

# Command Summary

| Action | Command |
|--------|---------|
| Install bbt | `alr install bbt` |
| Build from source | `alr build` or `make build` |
| Verify installation | `bbt --version` |
| Run a test | `bbt my_test.md` |
| Show test structure | `bbt explain my_test.md` |
| Run in verbose mode | `bbt --verbose my_test.md` |
| Generate an example | `bbt help example > test.md` |
| View grammar | `bbt help grammar` |
| Exclude tags | `bbt tests/ --exclude Windows_Only` |
| Include tags | `bbt tests/ --include Smoke` |

---

# Advanced Topics

## Background Usage

**Document-level background (applies to all scenarios):**
```markdown
## Background: Common test setup

- Given the directory `test_data`
- And the file `config.ini` containing `default=value`
```

**Feature-level background (applies to feature scenarios only):**
```markdown
# Feature: File processing

## Background: File processing setup
- Given the file `input.dat` containing `test data`
```

## Complex File Operations

**File content verification:**
```markdown
- Then file `output.txt` contains
```
Line 1 of expected content
Line 2 of expected content
Line 3 of expected content
```
```

**File content exclusion:**
```markdown
- Then file `log.txt` does not contain `error`
```

## Advanced Matching

**Unordered content matching:**
```markdown
- Then output contains unordered
```
item1
item2
item3
```
```

**Regex patterns:**
```markdown
- Then output matches `^Success: .* completed`
- Then output does not match `^Error: .*`
```

## Filtering and Tags

**Scenario filtering:**
```markdown
## Scenario: Windows-specific test, Windows_Only

- Given the Windows environment
```

**Feature filtering:**
```markdown
# Feature: Linux file system tests, Linux_Only
```

---

# Common Mistakes to Avoid

## Keyword Conflicts

**Wrong:**
```markdown
- - given there is no `config` file in the current directory
```

**Correct:**
```markdown
- - given there is no `config` file
```

## Parameter Formatting

**Wrong:**
```markdown
- When I run gcc --version
```

**Correct:**
```markdown
- When I run `gcc --version`
```

## Incorrect Negation

**Wrong:** (bbt ignores "never")
```markdown
- then the output never contains `Error`
```

**Correct:**
```markdown
- then the output doesn't contains `Error`
```

---

# Complete Examples

## Simple Command Test

**Input:** "Test that gcc compiler is installed"

**Output:**
```markdown
## Scenario: GCC compiler installation check

- When I run `gcc --version`
- Then output contains `gcc`
- And I get no error
```

## File Processing Workflow

**Input:** "Convert input.txt to output.txt using converter tool and verify result"

**Output:**
```markdown
## Scenario: File conversion workflow

- Given the file `input.txt` containing `raw data`
- When I run `converter input.txt output.txt`
- Then there is a file `output.txt`
- And file `output.txt` contains `processed data`
- And I get no error
```

## Complex Build System

**Input:** "Test complete build process: create source, compile, link, and verify executable"

**Output:**
```markdown
## Scenario: Complete build process

- Given the file `main.c` containing
```c
#include <stdio.h>
int main() {
    printf("Hello\n");
    return 0;
}
```
- When I successfully run `gcc -c main.c`
- Then there is a file `main.o`
- When I successfully run `gcc -o program main.o`
- Then there is a file `program`
- When I run `./program`
- Then output contains `Hello`
- And I get no error
```

## Error Handling Test

**Input:** "Verify that invalid input produces appropriate error message"

**Output:**
```markdown
## Scenario: Invalid input error handling

- Given the file `invalid.txt` containing `corrupted data`
- When I run `validator invalid.txt`
- Then I get an error
- And output contains `Invalid format`
- And output contains `Line 1: Syntax error`
```

---

# Decision Tree for Scenario Generation

## Step 1: Identify Test Objective
- Command execution: Use `run` or `successfully run`
- File operation: Use file-related keywords
- Output verification: Use `get`, `contains`, `matches`
- Error handling: Use `get error`, `I get an error`

## Step 2: Determine Parameters
- Short text (single line): Use inline backticks
- Multiline content: Use a code block after the step
- File reference: Use filename in backticks

## Step 3: Choose Step Type
- Setup/Precondition: `Given`
- Action/Execution: `When`
- Verification/Result: `Then`
- Continuation: `And` or `But`

## Step 4: Add Natural Decoration
- Keep it simple and focused
- Avoid bbt keywords in free text
- Use natural language that humans would understand

## Step 5: Validate Structure
- Scenarios must start with Given, When, or Then
- And/But can only follow another step
- Code blocks must immediately follow their step
- All parameters must be properly formatted

---

# Writing Guidelines

## Generation Priorities
1. Correct syntax first (must be parseable by bbt)
2. Natural language second (must be readable by humans)
3. Completeness third (cover the test objective)

## Steps must be natural English sentences
- Too short: `- Then file output.txt is file expected.txt`
- Too long: `- Then the file output.txt has the same content as file expected.txt, this checks the UTF8 vs LATIN-1 conversion discussed in #234`
- OK: `- Then the file output.txt has the same content as file expected.txt, fixes #234`
- Comments should be moved to following lines, except:
  - Filtering tags must stay on the line of the item to filter
  - Issue numbers may stay on the same line

## When in Doubt
- Use the most common pattern from the Quick Start Guide
- Prefer simple, direct language
- Use meaningful filenames
- Focus on the core test objective

## Handling Ambiguity
- Missing parameters: Use placeholders like `value`, `content`
- Unclear expectations: Use `contains` rather than exact matches
- Complex workflows: Break into multiple simple scenarios

## Avoid Snapshot Testing
Avoid test results that provide a full reference output if the test is focused on a specific part. Otherwise, all tests are impacted when the output format changes.
Use `matches` or `contains` instead of `is`.

Don't do:
```md
# Scenario 1: testing the complete --version message
- When I run `gcc --version`
- Then the output is 
~~~
gcc (Debian 14.2.0-19) 14.2.0
Copyright (C) 2024 Free Software Foundation, Inc.
~~~
```

Prefer:
```md
# Scenario 1 : testing the Copyright
- When I run `gcc --version`
- Then the output contains 
~~~
Copyright (C) 2024 Free Software Foundation, Inc.
~~~

# Scenario 2: testing the version format
- When I run `gcc --version`
- Then the output matches `(.*version [0-9]+\.[0-9]+\.[0-9]+ .*`
```

## Code Block Nesting Rules

**Critical Rule for LLM**: When documenting scenarios that contain code blocks, follow these rules:
1. Outer code block: Use `~~~` with language specifier
2. Inner code blocks: Use ``` with language specifier
3. Maximum nesting: Never exceed 2 levels

**Correct Example**:
```markdown
~~~markdown
## Scenario: File creation example

- Given the file `script.sh`
```bash
#!/bin/bash
echo "Hello"
```
~~~
```

## Optimization Tips
- Group related tests in the same feature
- Use Background for common setup across scenarios
- Keep scenarios focused on single test objectives
- Use descriptive scenario names

---

# Reference Summary

## Key Rules Checklist
- Scenarios start with Given/When/Then (not And/But)
- Parameters in backticks or code blocks
- Code blocks immediately follow their step
- No bbt keywords in decorative text
- File operations specify filenames
- Commands are executable strings

## Common Keywords
- Actions: run, successfully run, is, is no, contains, does not contain, get, matches
- Subjects: file, output, error, dir, directory
- Modifiers: new, no, not, unordered

## Parameter Style Guide
- Inline: Single line, short text, commands, filenames
- Code block: Multiline content, file contents, expected output
- File reference: Use `file` keyword + filename in backticks

This reference guide provides LLM agents with the essential information needed to generate valid, effective bbt scenarios that remain readable as natural language for humans.
