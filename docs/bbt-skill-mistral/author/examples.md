# Complete Examples and Practical Exercises

This file contains complete, ready-to-use examples of bbt scenarios, plus exercises for practice.

---

## 📚 Complete Examples

### Example 1: Simple Command Test

**Input:** "Test that gcc compiler is installed"

**Output:**
```markdown
## Scenario: GCC compiler installation check

- When I run `gcc --version`
- Then output contains `gcc`
- And I get no error
```

---

### Example 2: File Processing Workflow

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

---

### Example 3: Complex Build System

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

---

### Example 4: Error Handling Test

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

### Example 5: Background Usage

**Document-level background** (applies to all scenarios in the file):
```markdown
## Background: Common test setup

- Given the directory `test_data`
- And the file `config.ini` containing `default=value`

## Scenario: First test using background
- When I run `command1`
- Then output contains `result1`

## Scenario: Second test using same background
- When I run `command2`
- Then output contains `result2`
```

**Feature-level background** (applies only to scenarios in that feature):
```markdown
# Feature: File processing

## Background: File processing setup
- Given the file `input.dat` containing `test data`

## Scenario: Process file
- When I run `processor input.dat`
- Then there is a file `output.dat`
```

---

### Example 6: Complex File Operations

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

---

### Example 7: Advanced Matching

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

---

### Example 8: Filtering with Tags

**Scenario filtering:**
```markdown
## Scenario: Windows-specific test, Windows_Only

- Given the Windows environment
- When I run `windows_command`
- Then output contains `Windows result`
```

**Feature filtering:**
```markdown
# Feature: Linux file system tests, Linux_Only

## Scenario: Linux file operations
- When I run `ls -la`
- Then output contains `total`
```

Run with:
```bash
# Include only Linux tests
bbt tests/ --include Linux_Only

# Exclude Windows tests
bbt tests/ --exclude Windows_Only
```

---

## 🎯 Practical Exercises

Try writing bbt scenarios for these common testing situations.

### Exercise 1: Basic Command Test
**Task:** Verify that `echo "Hello"` returns `Hello`

**Solution:**
```markdown
## Scenario: Echo command returns input

- When I run `echo "Hello"`
- Then output contains `Hello`
- And I get no error
```

---

### Exercise 2: File Test
**Task:** 
1. Create a file `test.txt` with content `"Ada"`
2. Verify that `cat test.txt` returns `"Ada"`

**Solution:**
```markdown
## Scenario: File creation and content verification

- Given the file `test.txt` containing `Ada`
- When I run `cat test.txt`
- Then output contains `Ada`
- And I get no error
```

---

### Exercise 3: Error Test
**Task:** Verify that `ls nonexistent_file` returns an error code

**Solution:**
```markdown
## Scenario: Listing nonexistent file produces error

- Given there is no file `nonexistent_file`
- When I run `ls nonexistent_file`
- Then I get error
- And output contains `No such file or directory`
```

---

### Exercise 4: Multi-Step Test
**Task:**
1. Create a file `input.csv`
2. Run a script that processes it
3. Verify that `output.csv` is created with the correct content

**Solution:**
```markdown
## Scenario: CSV processing workflow

- Given the file `input.csv` containing
```csv
name,value
item1,100
item2,200
```
- When I successfully run `processor input.csv output.csv`
- Then there is a file `output.csv`
- And file `output.csv` contains
```csv
name,value,processed
item1,100,YES
item2,200,YES
```
- And I get no error
```

---

### Exercise 5: Conditional Test
**Task:** Test that a program behaves differently based on input

**Solution:**
```markdown
## Scenario: Program handles valid input

- Given the file `valid_input.txt` containing `good data`
- When I run `program valid_input.txt`
- Then output contains `Success`
- And I get no error

## Scenario: Program handles invalid input

- Given the file `invalid_input.txt` containing `bad data`
- When I run `program invalid_input.txt`
- Then I get error
- And output contains `Invalid input`
```

---

### Exercise 6: Complete Application Test
**Task:** Test a complete application workflow:
1. Create configuration
2. Initialize database
3. Run application
4. Verify output

**Solution:**
```markdown
## Scenario: Complete application workflow

- Given the file `config.yaml` containing
```yaml
server:
  port: 8080
database:
  url: sqlite:app.db
```
- When I successfully run `init_db`
- And I successfully run `start_server`
- Then output contains `Server started on port 8080`
- And there is a file `app.db`
- And I get no error
```

---

## Decision Tree for Scenario Generation

Use this decision tree when creating scenarios from natural language.

### Step 1: Identify Test Objective
- **Command execution** → Use `run` or `successfully run`
- **File operation** → Use file-related keywords
- **Output verification** → Use `get`, `contains`, `matches`
- **Error handling** → Use `get error`, `I get an error`

### Step 2: Determine Parameters
- **Short text (single line)** → Use inline backticks
- **Multiline content** → Use a code block after the step
- **File reference** → Use filename in backticks

### Step 3: Choose Step Type
- **Setup/Precondition** → `Given`
- **Action/Execution** → `When`
- **Verification/Result** → `Then`
- **Continuation** → `And` or `But`

### Step 4: Add Natural Decoration
- Keep it simple and focused
- Avoid bbt keywords in free text
- Use natural language that humans would understand

### Step 5: Validate Structure
- Scenarios must start with Given, When, or Then
- And/But can only follow another step
- Code blocks must immediately follow their step
- All parameters must be properly formatted

---

## Writing Guidelines

### Generation Priorities
1. **Correct syntax first** (must be parseable by bbt)
2. **Natural language second** (must be readable by humans)
3. **Completeness third** (cover the test objective)

### Steps Must Be Natural English Sentences

| **Quality** | **Example** | **Note** |
|-------------|-------------|----------|
| **Too short** | `- Then file output.txt is file expected.txt` | Missing words |
| **Too long** | `- Then the file output.txt has the same content as file expected.txt, this checks the UTF8 vs LATIN-1 conversion discussed in #234` | Too much detail |
| **OK** | `- Then the file output.txt has the same content as file expected.txt, fixes #234` | Balanced |

**Comments should be moved to following lines**, except:
- Filtering tags must stay on the line of the item to filter
- Issue numbers may stay on the same line

---

### When in Doubt
1. Use the most common pattern from this guide
2. Prefer simple, direct language
3. Use meaningful filenames
4. Focus on the core test objective

---

### Handling Ambiguity
- **Missing parameters**: Use placeholders like `value`, `content`
- **Unclear expectations**: Use `contains` rather than exact matches
- **Complex workflows**: Break into multiple simple scenarios
