# File Operations Patterns

Use these patterns for creating, reading, comparing, and verifying files in bbt scenarios.

---

## File Creation

### Create File with Inline Content
Use for short, single-line file content.

```markdown
- Given the file `filename` containing `file_content`
```

**Example:**
```markdown
## Scenario: Create a simple config file

- Given the file `config.txt` containing `key=value`
- Then there is a file `config.txt`
```

---

### Create File with Multiline Content
Use code blocks for file contents spanning multiple lines.

```markdown
- Given the file `filename` containing
```language
line 1
line 2
line 3
```
```

**Example:**
```markdown
## Scenario: Create a script

- Given the file `script.sh` containing
```bash
#!/bin/bash
echo "Hello, World!"
exit 0
```
- Then there is a file `script.sh`
```

---

### Create New File (Explicit)
Use `new file` to explicitly state the file should be created (as opposed to modified).

```markdown
- Given the new file `filename` containing
```language
content
```
```

**Example:**
```markdown
## Scenario: Create a new source file

- Given the new file `main.c` containing
```c
#include <stdio.h>
int main() { return 0; }
```
```

---

## File Existence

### Check File Exists
Verify that a file exists in the filesystem.

```markdown
- Given there is a file `filename`
```

**Example:**
```markdown
## Scenario: Verify build artifact exists

- When I successfully run `make`
- Then there is a file `program.exe`
```

---

### Check File Does Not Exist
Verify that a file does **not** exist.

```markdown
- Given there is no file `filename`
```

**Example:**
```markdown
## Scenario: Clean build directory

- Given there is no file `output.o`
- When I run `make clean`
- Then there is no file `output.o`
```

---

## File Content Verification

### Verify File Contains Text
Check if a file contains specific text (inline).

```markdown
- Then file `filename` contains `text`
```

**Example:**
```markdown
## Scenario: Check log file for success message

- Given the file `log.txt` containing `Operation completed`
- Then file `log.txt` contains `success`
```

---

### Verify File Contains Multiline Text
Use code blocks for multiline expected content.

```markdown
- Then file `filename` contains
```
line 1
line 2
line 3
```
```

**Example:**
```markdown
## Scenario: Verify generated file content

- Given the file `input.txt` containing `raw data`
- When I run `processor input.txt output.txt`
- Then file `output.txt` contains
```
processed line 1
processed line 2
```
```

---

### Verify File Does Not Contain Text
Check that a file does **not** contain certain text.

```markdown
- Then file `filename` does not contain `text`
```

**Example:**
```markdown
## Scenario: Verify no errors in log

- Given the file `log.txt` containing `Info: Processing...`
- Then file `log.txt` does not contain `ERROR`
```

---

## File Comparison

### Compare Two Files
Verify that two files have identical content.

```markdown
- Then file `file1` is equal to file `file2`
```

**Example:**
```markdown
## Scenario: Verify output matches expected

- Given the file `expected.txt` containing `expected result`
- When I run `generate output.txt`
- Then file `output.txt` is equal to file `expected.txt`
```

---

## Directory Operations

### Create Directory
Create a directory structure.

```markdown
- Given the directory `dirname`
```

**Example:**
```markdown
## Scenario: Setup test environment

- Given the directory `test_data`
- And the file `test_data/input.txt` containing `test`
```

---

## Common Use Cases

| **Use Case** | **Pattern** |
|--------------|-------------|
| Create a simple file | `- Given the file `name` containing `content` |
| Create a file with multiline content | `- Given the file `name` containing`<br>```content``` |
| Create a new file (explicit) | `- Given the new file `name` containing` |
| Check file exists | `- Given there is a file `name` |
| Check file does not exist | `- Given there is no file `name` |
| Verify file contains text | `- Then file `name` contains `text` |
| Verify file does not contain text | `- Then file `name` does not contain `text` |
| Compare two files | `- Then file `file1` is equal to file `file2` |
| Create a directory | `- Given the directory `dirname` |

---

## Tips

1. **File paths are relative** to the directory containing the `.md` file with the scenario.
2. **Code blocks must immediately follow** the step (no empty lines).
3. **Use meaningful filenames** that describe the file's purpose.
4. **For large files**, consider storing the expected content in a separate file and using `Then file `output` is equal to file `expected``.
5. **Clean up**: Use `Given there is no file` to ensure a clean state before tests.

---

## Complete Example: File Processing Workflow

```markdown
## Scenario: Convert input file and verify output

- Given the directory `test_data`
- And the file `test_data/input.csv` containing
```csv
name,age
Alice,30
Bob,25
```
- When I run `converter test_data/input.csv test_data/output.json`
- Then there is a file `test_data/output.json`
- And file `test_data/output.json` contains
```json
[{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]
```
- And I get no error
```
