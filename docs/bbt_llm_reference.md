# *bbt* LLM Reference Guide

**Purpose**: Enable LLM agents to generate valid *bbt* scenarios from natural language with proper syntax and structure.

**Target Audience**: LLM agents generating *bbt* test scenarios

- [*bbt* LLM Reference Guide](#bbt-llm-reference-guide)
  - [QUICK START GUIDE (For LLM)](#quick-start-guide-for-llm)
    - [80% Rule: Most Common Pattern](#80-rule-most-common-pattern)
  - [CORE SYNTAX RULES](#core-syntax-rules)
    - [Grammar Reference Table](#grammar-reference-table)
    - [Step Keywords and Structure](#step-keywords-and-structure)
    - [Parameter Formatting](#parameter-formatting)
    - [Document Structure](#document-structure)
  - [COMMON SCENARIO PATTERNS](#common-scenario-patterns)
    - [Command Execution](#command-execution)
    - [File Operations](#file-operations)
    - [Output Verification](#output-verification)
    - [Error Handling](#error-handling)
  - [NATURAL LANGUAGE TRANSFORMATION](#natural-language-transformation)
    - [From Requirements to *bbt*](#from-requirements-to-bbt)
    - [From README to *bbt*](#from-readme-to-bbt)
  - [Scenario: Invalid JSON file fails validation](#scenario-invalid-json-file-fails-validation)
    - [Complex File Operations](#complex-file-operations)
    - [Advanced Matching](#advanced-matching)
    - [Filtering and Tags](#filtering-and-tags)
  - [COMMON MISTAKES TO AVOID](#common-mistakes-to-avoid)
    - [Keyword Conflicts](#keyword-conflicts)
    - [Parameter Formatting](#parameter-formatting-1)
    - [The sentence is interpreted in an opposite way by `bbt`](#the-sentence-is-interpreted-in-an-opposite-way-by-bbt)
  - [COMPLETE EXAMPLES](#complete-examples)
    - [Simple Command Test](#simple-command-test)
    - [File Processing Workflow](#file-processing-workflow)
    - [Complex Build System](#complex-build-system)
  - [DECISION TREE FOR SCENARIO GENERATION](#decision-tree-for-scenario-generation)
    - [Step 1: Identify Test Objective](#step-1-identify-test-objective)
    - [Step 2: Determine Parameters](#step-2-determine-parameters)
    - [Step 3: Choose Step Type](#step-3-choose-step-type)
    - [Step 4: Add Natural Decoration](#step-4-add-natural-decoration)
    - [Step 5: Validate Structure](#step-5-validate-structure)
  - [WRITING GUIDELINES](#writing-guidelines)
    - [Generation Priorities](#generation-priorities)
    - [Steps must be natural english sentences](#steps-must-be-natural-english-sentences)
    - [When in Doubt](#when-in-doubt)
    - [Handling Ambiguity](#handling-ambiguity)
    - [Avoid snapshot testing](#avoid-snapshot-testing)
    - [Code Block Nesting Rules](#code-block-nesting-rules)
    - [Optimization Tips](#optimization-tips)
  - [REFERENCE SUMMARY](#reference-summary)
    - [Key Rules Checklist](#key-rules-checklist)
    - [Common Keywords](#common-keywords)
    - [Parameter Style Guide](#parameter-style-guide)


## QUICK START GUIDE (For LLM)

### 80% Rule: Most Common Pattern

```markdown
## Scenario: [Brief description]

- Given [setup condition]
- When [action to perform]  
- Then [expected result]
```

**Parameter formatting:**
- Short text: `` `inline parameter` ``
- Multiline content: code block immediately after step
- File references: `` `filename` ``

**Common step patterns:**
- Command execution: `- When I run `command` `
- File creation: `- Given the file `name` containing `content` `
- Output verification: `- Then output contains `expected` `
- Error checking: `- Then I get an error`

## CORE SYNTAX RULES

### Grammar Reference Table

This table provides a comprehensive reference of all *bbt* syntax patterns:

Step kind  |         |Subject |       Verb       | Object |         Action          | Code block expected after the step |
|-------|---------|--------|------------------|--------|-------------------------|------------|
| Given |         |        | run              | `text` | RUN_CMD                 |            |
| Given |         |        | successfully run | `text` | RUN_WITHOUT_ERROR       |            |
| Given |         |        | is               | `file` | CHECK_FILE_EXISTENCE    |            |
| Given |         |        | is               | `dir`  | CHECK_DIR_EXISTENCE     |            |
| Given |         |        | is no            | `file` | SETUP_NO_FILE           |            |
| Given |         |        | is no            | `dir`  | SETUP_NO_DIR            |            |
| Given |         | `dir`  |                  |        | CREATE_IF_NONE          |            |
| Given |         | `file` |                  |        | ERASE_AND_CREATE        |     X      |
| Given |         | `file` | containing       |        | CREATE_IF_NONE          |     X      |
| Given |         | `file` | containing       | `text` | CREATE_IF_NONE          |            |
| Given | new     | `dir`  |                  |        | ERASE_AND_CREATE        |            |
| Given | new     | `file` |                  |        | ERASE_AND_CREATE        |     X      |
| Given | new     | `file` | containing       |        | ERASE_AND_CREATE        |     X      |
| Given | new     | `file` | containing       | `text` | ERASE_AND_CREATE        |            |
| When  |         |        | run              | `text` | RUN_CMD                 |            |
| When  |         |        | run              | `cmd`  | RUN_CMD                 |            |
| When  |         |        | successfully run | `text` | RUN_WITHOUT_ERROR       |            |
| When  |         |        | successfully run | `cmd`  | RUN_WITHOUT_ERROR       |            |
| Then  |         |        | get              |        | OUTPUT_IS               |     X      |
| Then  |         |        | get              | `file` | OUTPUT_IS               |            |
| Then  |         |        | get              | `text` | OUTPUT_IS               |            |
| Then  |         |        | get              | error  | ERROR_RETURN_CODE       |            |
| Then  |         |        | get no           | output | NO_OUTPUT               |            |
| Then  |         |        | get no           | error  | NO_ERROR_RETURN_CODE    |            |
| Then  |         |        | is               | `file` | CHECK_FILE_EXISTENCE    |            |
| Then  |         |        | is               | `dir`  | CHECK_DIR_EXISTENCE     |            |
| Then  |         |        | is               | error  | ERROR_RETURN_CODE       |            |
| Then  |         |        | is no            | output | NO_OUTPUT               |            |
| Then  |         |        | is no            | `file` | CHECK_NO_FILE           |            |
| Then  |         |        | is no            | `dir`  | CHECK_NO_DIR            |            |
| Then  |         |        | is no            | error  | NO_ERROR_RETURN_CODE    |            |
| Then  |         | `file` | does not contain |        | FILE_DOES_NOT_CONTAIN   |     X      |
| Then  |         | `file` | does not contain | `file` | FILE_DOES_NOT_CONTAIN   |            |
| Then  |         | `file` | does not contain | `text` | FILE_DOES_NOT_CONTAIN   |            |
| Then  |         | `file` | contains         |        | FILE_CONTAINS           |     X      |
| Then  |         | `file` | contains         | `file` | FILE_CONTAINS           |            |
| Then  |         | `file` | contains         | `text` | FILE_CONTAINS           |            |
| Then  |         | `file` | is               |        | FILE_IS                 |     X      |
| Then  |         | `file` | is               | `file` | FILE_IS                 |            |
| Then  |         | `file` | is               | `text` | FILE_IS                 |            |
| Then  |         | `file` | is no            | `file` | FILE_IS_NOT             |            |
| Then  |         | output | does not contain |        | OUTPUT_DOES_NOT_CONTAIN |     X      |
| Then  |         | output | does not contain | `file` | OUTPUT_DOES_NOT_CONTAIN |            |
| Then  |         | output | does not contain | `text` | OUTPUT_DOES_NOT_CONTAIN |            |
| Then  |         | output | contains         |        | OUTPUT_CONTAINS         |     X      |
| Then  |         | output | contains         | `file` | OUTPUT_CONTAINS         |            |
| Then  |         | output | contains         | `text` | OUTPUT_CONTAINS         |            |
| Then  |         | output | matches          | `text` | OUTPUT_MATCHES          |            |
| Then  |         | output | does not match   | `text` | OUTPUT_DOES_NOT_MATCH   |            |
| Then  |         | output | is               |        | OUTPUT_IS               |     X      |
| Then  |         | output | is               | `file` | OUTPUT_IS               |            |
| Then  |         | output | is               | `text` | OUTPUT_IS               |            |

### Step Keywords and Structure

**Valid step starters:** `Given`, `When`, `Then`
**Continuation keywords:** `And`, `But` (must follow a Given/When/Then)

**Step structure:**
```markdown
- [Given|When|Then] <step_content>
- [And|But] <step_content>  # Continuation only
```

### Parameter Formatting

**Inline parameters (backticks):**
- Commands: `` `gcc --version` ``
- Filenames: `` `output.txt` ``
- Short text: `` `success` ``

**Multiline parameters (code blocks):**
- File contents
- Expected multiline output
- Command scripts

**Example:**
~~~markdown
- Given the file `script.sh`
```bash
#!/bin/bash
echo "Hello"
```
~~~

### Document Structure

```markdown
# Feature: [Optional feature name]

## Background: [Optional common setup]
- Given [background step]
- And [background step]

## Scenario: [Test case name]
- Given [setup]
- When [action]
- Then [result]
```

A document may contain several features.
"
A feature may contain several scenarios.


## COMMON SCENARIO PATTERNS

### Command Execution

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

### File Operations

**File creation with content:**
```markdown
- Given the file `input.txt` containing `test data`
```

**File creation with multiline content:**
```markdown
- Given the file `config.ini`
```ini
[key]=value
[section]
option=setting
```
```

**File existence check:**
```markdown
- Given there is a file `data.txt`
- Then file `data.txt` contains `expected content`
```

**File comparison:**
```markdown
- Then file `output.txt` is equal to file `expected.txt`
```

### Output Verification

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

### Error Handling

**Error detection:**
```markdown
- When I run `invalid_command`
- Then I get an error
```

**Specific error code:**
```markdown
- Then I get error
- And output contains `File not found`
```

**No error verification:**
```markdown
- When I successfully run `valid_command`
- Then I get no error
```

## NATURAL LANGUAGE TRANSFORMATION

### From Requirements to *bbt*

**Requirement:** "System shall display version when --version flag is used"

**Transformation:**
```markdown
## Scenario: Version flag displays correct version

- When I run `program --version`
- Then output contains `1.0.0`
- And I get no error
```

### From README to *bbt*

**README instruction:** "To compile: 1. Create source file, 2. Run compiler, 3. Verify output"

**Transformation:**
```markdown
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
```

### From User Stories to *bbt*

**User Story:** "As a developer, I want to validate my JSON files so that I can catch syntax errors early"

**Transformation:**
```markdown
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

## Scenario: Invalid JSON file fails validation

- Given the file `invalid.json` containing `{"name": "test",}`
- When I run `json_validator invalid.json`
- Then I get an error
- And output contains `Syntax error`
```

## ADVANCED TOPICS

### Background Usage

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

### Complex File Operations

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

### Advanced Matching

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

### Filtering and Tags

**Scenario filtering:**
```markdown
## Scenario: Windows-specific test, Windows_Only

- Given the Windows environment
```

**Feature filtering:**
```markdown
# Feature: Linux file system tests, Linux_Only
```

## COMMON MISTAKES TO AVOID

### Keyword Conflicts

**❌ Keyword in free text:**
```markdown
- - given there is no `config` file in the current directory  # ❌ There is both file and directory keywords in the object phrase
```

**✅ Correct:**
```markdown
- - given there is no `config` file
```

### Parameter Formatting

**❌ Missing backticks:**
```markdown
- When I run gcc --version  # ❌ Command not in backticks
```

**✅ Correct:**
```markdown
- When I run `gcc --version`
```

### The sentence is interpreted in an opposite way by `bbt`

**❌ Incorrect:** 
```markdown
- then the output never contains `Error`
```
`bbt` ignore "never" (not a keyword) and will check that the output *contains* `Error`

**✅ Correct:**
```markdown
- then the output doesn't contains `Error`
```

## COMPLETE EXAMPLES

### Simple Command Test

**Input:** "Test that gcc compiler is installed"

**Output:**
```markdown
## Scenario: GCC compiler installation check

- When I run `gcc --version`
- Then output contains `gcc`
- And I get no error
```

### File Processing Workflow

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

### Complex Build System

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

### Error Handling Test

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

## DECISION TREE FOR SCENARIO GENERATION

### Step 1: Identify Test Objective
- **Command execution** → Use `run` or `successfully run`
- **File operation** → Use file-related keywords
- **Output verification** → Use `get`, `contains`, `matches`
- **Error handling** → Use `get error`, `I get an error`

### Step 2: Determine Parameters
- **Short text** (single line) → Inline backticks `` `text` ``
- **Multiline content** → Code block after step
- **File reference** → `` `filename` ``

### Step 3: Choose Step Type
- **Setup/Precondition** → `Given`
- **Action/Execution** → `When`
- **Verification/Result** → `Then`
- **Continuation** → `And` or `But`

### Step 4: Add Natural Decoration
- Keep it simple and focused
- Avoid *bbt* keywords in free text
- Use natural language that humans would understand

### Step 5: Validate Structure
- Scenarios must start with Given, When, or Then
- And/But can only follow another step
- Code blocks must immediately follow their step
- All parameters must be properly formatted

## WRITING GUIDELINES

### Generation Priorities
1. **Correct syntax** first (must be parseable by *bbt*)
2. **Natural language** second (must be readable by humans)
3. **Completeness** third (cover the test objective)

### Steps must be natural english sentences
- too short, not natural : "- Then file `output.txt` is file `expected.txt`
- too long               : "- Then the file `output.txt` has the same content as file `expected.txt`, this check the UTF8 vs LATIN-1 conversion discussed in #234"
- OK     : "- Then the file `output.txt` has the same content as file `expected.txt`, fixes #234"
- comments should be moved to following lines, with the following exceptions: 
  - filtering tags must be on stay on the line of the item to filter (Feature, Scenario, Step)  
  - Issue number may stay on the same line

### When in Doubt
- Use the most common pattern from the Quick Start Guide
- Prefer simple, direct language
- Use meaningful filenames (`customers.txt`, `output.txt`, `sensor_3_data.json`)
- Focus on the core test objective

### Handling Ambiguity
- **Missing parameters**: Use placeholders like `` `value` ``, `` `content` ``
- **Unclear expectations**: Use `contains` rather than exact matches
- **Complex workflows**: Break into multiple simple scenarios

### Avoid snapshot testing
Avoid test results that provide a full reference output if the test is focused on a specific part. Otherwise, all tests are impacted when the output format changes, not only tests regarding specifically the modified part.
This is achieved by using "matches" or "contains" instead of "is".

Example:
```md
# Scenario 1: testing the complete --version message
- When I run `gcc --version`
- Then the output is 
~~~
gcc (Debian 14.2.0-19) 14.2.0
Copyright (C) 2024 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
~~~

# Scenario 2: testing just that the Copyright is given
- When I run `gcc --version`
- Then the output contains  
~~~
Copyright (C) 2024 Free Software Foundation, Inc.
~~~

# Scenario 3: testing just the format of the version number
- When I run `--version`
- Then the output matches `(.*version [0-9]+\.[0-9]+\.[0-9]+ .*`
```

### Code Block Nesting Rules

**Critical Rule for LLM**: When documenting scenarios that contain code blocks, follow these nesting rules:

1. **Outer code block**: Use `~~~` with language specifier
2. **Inner code blocks**: Use ``` with language specifier
3. **Maximum nesting**: Never exceed 2 levels of nesting

**✅ Correct Example**:
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

### Optimization Tips
- Group related tests in the same feature
- Use Background for common setup across scenarios
- Keep scenarios focused on single test objectives
- Use descriptive scenario names

## REFERENCE SUMMARY

### Key Rules Checklist
- ✅ Scenarios start with Given/When/Then (not And/But)
- ✅ Parameters in backticks or code blocks
- ✅ Code blocks immediately follow their step
- ✅ No *bbt* keywords in decorative text
- ✅ File operations specify filenames
- ✅ Commands are executable strings

### Common Keywords
- **Actions**: run, successfully run, is, is no, contains, does not contain, get, matches
- **Subjects**: file, output, error, dir, directory
- **Modifiers**: new, no, not, unordered

### Parameter Style Guide
- **Inline**: Single line, short text, commands, filenames
- **Code block**: Multiline content, file contents, expected output
- **File reference**: Use `file` keyword + filename in backticks

This optimized reference guide provides LLM agents with the essential information needed to generate valid, effective *bbt* scenarios while maintaining natural language readability for humans.