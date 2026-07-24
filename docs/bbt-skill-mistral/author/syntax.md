# bbt Syntax Reference

`bbt` uses a **Gherkin-like syntax** embedded in Markdown files. Scenarios are structured with natural language steps that bbt can parse and execute.

---

## Grammar Overview

A bbt scenario consists of:
1. A **header** (optional, for documentation)
2. One or more **steps** starting with keywords (`Given`, `When`, `Then`, `And`, `But`)
3. **Parameters** in backticks (inline) or code blocks (multiline)

### Basic Structure
```markdown
## Scenario: <Descriptive Name>

- Given <precondition>    # Setup step
- When <action>           # Execution step
- Then <verification>    # Assertion step
- And <continuation>     # Optional continuation
```

**Critical Rules:**
- Scenarios **must start** with `Given`, `When`, or `Then` (not `And` or `But`)
- `And`/`But` can **only follow** another step (not at the start)
- Steps use **dash `-`** as list marker (not `*` or `+`)
- Text outside steps (comments, explanations) is **ignored by bbt**

---

## Keywords

| **Role** | **Keywords** | **Example** |
|----------|-------------|-------------|
| **Setup/Precondition** | `Given` | `- Given the file `input.txt` containing `data` |
| **Action/Execution** | `When` | `- When I run `gcc --version` |
| **Verification/Result** | `Then` | `- Then output contains `14.2.0` |
| **Continuation** | `And`, `But` | `- And I get no error` |

### Keyword Usage Notes
- **`Given`**: Describes the initial state (files exist, environment setup)
- **`When`**: Describes the action being tested
- **`Then`**: Describes the expected outcome
- **`And`/`But`**: Continues the previous step type (setup, action, or verification)

---

## Parameter Formatting

Parameters can be specified in two ways, depending on their length and complexity.

### ⚡ Inline Parameters (Backticks)

Use **single backticks** for:
- Short commands: `` `gcc --version` ``
- Filenames: `` `output.txt` ``
- Short text: `` `success` ``
- Single-line expected output: `` `Hello, World!` ``

**Examples:**
```markdown
- When I run `gcc --version`
- Then there is a file `output.exe`
- Then output contains `compilation successful`
```

### 📄 Multiline Parameters (Code Blocks)

Use **triple backticks** (code blocks) for:
- File contents
- Multiline expected output
- Command scripts
- Any content spanning multiple lines

**Critical Rule**: Code blocks **must immediately follow** their step (no empty lines in between).

**Examples:**
```markdown
- Given the file `script.sh` containing
```bash
#!/bin/bash
echo "Hello"
```

- Then output contains
```
Line 1 of expected output
Line 2 of expected output
```
```

---

## Step Types Reference

### Command Execution
```markdown
- When I run `command --args`
- When I successfully run `command`
```

### File Operations
```markdown
- Given the file `name` containing `content`
- Given the new file `name` containing
```language
multiline
content
```
- Given there is a file `name`
- Given there is no file `name`
- Then there is a file `name`
- Then file `name` contains `content`
- Then file `output.txt` is equal to file `expected.txt`
- Then file `log.txt` does not contain `error`
```

### Output Verification
```markdown
- Then I get
```
actual output
```
- Then output contains `text`
- Then output contains file `expected.txt`
- Then output matches `regex_pattern`
- Then output does not match `error_pattern`
- Then output contains unordered
```
item1
item2
item3
```
```

### Error Handling
```markdown
- Then I get error
- Then I get an error
- Then I successfully run `command`
```

---

## Complete Grammar Reference

For the **full, authoritative grammar**, run:
```bash
bbt help grammar
```

This provides:
- All recognized keywords
- Complete step patterns
- Parameter parsing rules
- Regular expression details for matching

---

## File Example with Annotations

Here's a complete example with comments (note: comments starting with `-->` are for illustration and ignored by bbt):

~~~markdown
# gcc simple sanity tests  --> ignored by bbt (documentation)

## Scenario: gcc version?  --> scenario header

- When I run `gcc -v` --> Step with a parameter between backticks

* on Linux or Windows, the output is something like: --> ignored by bbt
  > gcc version 14.2.0 (Debian 14.2.0-16)  --> ignored by bbt

- Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*` --> Step with regexp

## Scenario: compiling and executing a hello world --> start another scenario

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

---

## Common Step Patterns

| **Category** | **Pattern** | **Example** |
|--------------|-------------|-------------|
| **Command execution** | `- When I run \`command\`` | `- When I run `gcc --version` |
| **File creation** | `- Given the file \`name\` containing \`content\`` | `- Given the file `config.txt` containing `key=value` |
| **File creation (multiline)** | `- Given the file \`name\`<br>\`\`\`language` | See example above |
| **Output verification** | `- Then output contains \`text\`` | `- Then output contains `success` |
| **Error checking** | `- Then I get error` | `- Then I get error` |
| **File existence** | `- Given there is a file \`name\`` | `- Given there is a file `data.txt` |
| **Regex matching** | `- Then output matches \`regex\`` | `- Then output matches `[0-9]+\.error` |

---

## Code Block Nesting Rules

**Critical for LLM**: When documenting scenarios that contain code blocks, follow these rules:

1. **Outer code block**: Use `~~~` with language specifier for markdown examples
2. **Inner code blocks**: Use \`\`\` with language specifier for actual bbt code blocks
3. **Maximum nesting**: Never exceed 2 levels

**Correct Example:**
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

**Incorrect Example (exceeds nesting):**
```markdown
~~~markdown
~~~bash
# This is WRONG - too many nesting levels
~~~
~~~ 
```
