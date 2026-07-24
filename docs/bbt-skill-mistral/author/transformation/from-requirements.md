# Transforming Requirements to bbt

Convert formal requirements ("System shall...", "The tool must...") into executable bbt scenarios.

---

## Requirement Transformation Guide

### Basic Structure

Most requirements follow this pattern:
```
[Subject] shall/will/must [action] [object] [condition]
```

Map to bbt:
- **Subject/Object** → Setup (`Given`) or context
- **Action** → Execution (`When`)
- **Condition/Result** → Verification (`Then`)

---

## Examples

### Example 1: Simple Command Requirement

**Requirement:**
> "System shall display version when --version flag is used"

**Transformation:**
```markdown
## Scenario: Version flag displays correct version

- When I run `program --version`
- Then output contains `1.0.0`
- And I get no error
```

**Mapping:**
- Action: "display version when --version flag is used" → `When I run program --version`
- Result: "display version" → `Then output contains 1.0.0`

---

### Example 2: File Creation Requirement

**Requirement:**
> "The tool must create a log file named 'output.log' containing the execution results"

**Transformation:**
```markdown
## Scenario: Log file is created with execution results

- When I run `tool execute`
- Then there is a file `output.log`
- And file `output.log` contains `Execution completed`
```

---

### Example 3: Error Handling Requirement

**Requirement:**
> "System shall return an error code when invalid input is provided"

**Transformation:**
```markdown
## Scenario: Invalid input produces error

- Given the file `invalid_input.txt` containing `bad data`
- When I run `program invalid_input.txt`
- Then I get error
- And output contains `Invalid input`
```

---

### Example 4: Complex Workflow Requirement

**Requirement:**
> "The compiler shall compile source files, generate object files, and produce an executable when given valid source code"

**Transformation:**
```markdown
## Scenario: Complete compilation workflow

- Given the file `main.c` containing
```c
#include <stdio.h>
int main() { return 0; }
```
- When I successfully run `gcc -c main.c`
- Then there is a file `main.o`
- When I successfully run `gcc -o program main.o`
- Then there is a file `program`
- And I get no error
```

**Note:** Split into multiple scenarios if the workflow is too complex:
```markdown
## Scenario: Compile source to object file
- Given the file `main.c` containing...
- When I successfully run `gcc -c main.c`
- Then there is a file `main.o`

## Scenario: Link object file to executable
- Given there is a file `main.o`
- When I successfully run `gcc -o program main.o`
- Then there is a file `program`
```

---

### Example 5: Conditional Requirement

**Requirement:**
> "If the input file does not exist, the system shall display an error message"

**Transformation:**
```markdown
## Scenario: Missing input file produces error

- Given there is no file `input.txt`
- When I run `program input.txt`
- Then I get error
- And output contains `File not found`
```

---

## Transformation Patterns

| **Requirement Type** | **bbt Pattern** | **Example** |
|----------------------|-----------------|-------------|
| **Command execution** | `When I run \`command\`` | `When I run program --version` |
| **File creation** | `Then there is a file \`name\`` | `Then there is a file output.log` |
| **File content** | `Then file \`name\` contains \`text\`` | `Then file output.log contains Success` |
| **Error condition** | `Then I get error` | `Then I get error` |
| **Success condition** | `Then I get no error` | `Then I get no error` |
| **Precondition** | `Given \`condition\`` | `Given there is a file input.txt` |

---

## Tips for Requirement Transformation

1. **Identify the verb**: This usually maps to the `When` step
   - "display" → `Then output contains`
   - "create" → `Then there is a file`
   - "return" → `Then I get` or `Then I get error`
   - "execute" → `When I run`

2. **Identify the subject**: This often maps to setup (`Given`) or the command itself

3. **Identify the object**: This maps to parameters (in backticks) or file contents

4. **Identify the result**: This maps to verification (`Then`)

5. **Add context**: Use descriptive scenario names that reflect the requirement

6. **Split complex requirements**: One scenario per behavior/test case

---

## Handling Ambiguity

When requirements are ambiguous:

1. **Missing parameters**: Use placeholders
   ```markdown
   - When I run `command`
   - Then output contains `expected_result`
   ```

2. **Unclear expectations**: Use `contains` rather than exact matches
   ```markdown
   - Then output contains `success`  # Better than exact match
   ```

3. **Complex conditions**: Break into multiple scenarios
   ```markdown
   ## Scenario: Positive case
   - Given valid input
   - When I run `command`
   - Then I get no error

   ## Scenario: Negative case
   - Given invalid input
   - When I run `command`
   - Then I get error
   ```

---

## Complete Example: Full Requirement Document

**Original Requirement Document:**
```
REQ-001: The system shall compile Ada source files with gnatmake.
REQ-002: The system shall produce an executable file when compilation succeeds.
REQ-003: The system shall display compilation errors when source contains syntax errors.
```

**Transformed bbt Scenarios:**
```markdown
# Feature: Ada Compilation

## Scenario: Successful compilation produces executable, REQ-001, REQ-002

- Given the file `main.adb` containing
```ada
with Ada.Text_IO;
procedure Main is
begin
   null;
end Main;
```
- When I successfully run `gnatmake main.adb`
- Then there is a file `main`
- And I get no error

## Scenario: Syntax errors are displayed, REQ-003

- Given the file `broken.adb` containing
```ada
with Ada.Text_IO
procedure Broken is  -- Missing semicolon
begin
   null;
end Broken;
```
- When I run `gnatmake broken.adb`
- Then I get error
- And output contains `syntax error`
```
