# Frequent Errors and Solutions

Common issues encountered when writing and running bbt scenarios, with solutions.

---

## Error Category: Syntax Errors

### Error 1: Missing Backticks

**Problem:** Commands or parameters not in backticks are not recognized as parameters.

**Incorrect:**
```markdown
- When I run gcc --version
```

**Correct:**
```markdown
- When I run `gcc --version`
```

**Symptoms:**
- Step appears as plain text instead of being parsed
- `bbt explain` shows the step but doesn't recognize the command

**Solution:** Always use backticks for commands, filenames, and short parameters.

---

### Error 2: Incorrect Code Block Markers

**Problem:** Using wrong number of backticks for code blocks.

**Incorrect (1 or 2 backticks):**
~~~markdown
- Then the output contains `
hello
`
~~~ 

**Incorrect (no backticks):**
```markdown
- Then the output contains
hello
```

**Correct (3 backticks):**
```markdown
- Then the output contains
```
hello
```
```

**Symptoms:**
- Code block content is not properly associated with the step
- Output matching fails

**Solution:** Always use triple backticks (```) for code blocks, and ensure they immediately follow the step.

---

### Error 3: Wrong List Marker

**Problem:** Using `*` or `+` instead of `-` for steps.

**Incorrect:**
```markdown
* When I run `ls`
* Then output contains `file.txt`
```

**Correct:**
```markdown
- When I run `ls`
- Then output contains `file.txt`
```

**Symptoms:**
- Steps are not recognized by bbt
- `bbt explain` shows the text but not as steps

**Solution:** Always use dash (`-`) as the list marker for steps.

---

### Error 4: Steps Starting with And/But

**Problem:** Scenarios cannot start with `And` or `But`.

**Incorrect:**
```markdown
## Scenario: Bad start

- And given there is a file `test.txt`
```

**Correct:**
```markdown
## Scenario: Good start

- Given there is a file `test.txt`
```

**Symptoms:**
- Scenario is not recognized
- `bbt explain` shows the text but not as a scenario

**Solution:** Always start scenarios with `Given`, `When`, or `Then`.

---

## Error Category: Parameter Formatting

### Error 5: Code Block Not Immediately Following Step

**Problem:** Empty lines between step and code block.

**Incorrect:**
```markdown
- Given the file `script.sh` containing

```bash
#!/bin/bash
echo "Hello"
```
```

**Correct:**
```markdown
- Given the file `script.sh` containing
```bash
#!/bin/bash
echo "Hello"
```
```

**Symptoms:**
- Code block is not associated with the step
- File content is not created

**Solution:** Ensure code blocks immediately follow their step with no empty lines.

---

### Error 6: Using bbt Keywords in Decorative Text

**Problem:** Using keywords like `Given`, `When`, `Then` in text outside of steps.

**Incorrect:**
```markdown
# This test Given a file does something

- When I run `command`
```

**Correct:**
```markdown
# This test creates a file and does something

- When I run `command`
```

**Symptoms:**
- May confuse bbt's parser
- Can lead to unexpected behavior

**Solution:** Avoid using bbt keywords in comments, headings, or decorative text.

---

## Error Category: Output Matching

### Error 7: Case Sensitivity in Output Matching

**Problem:** Output matching is case-sensitive by default with `--exact_match`.

**Symptom:** Tests fail when output differs only in case.

**Solution:** Use `--human_match` (default) for case-insensitive matching, or ensure exact case with `--exact_match`.

```bash
# Case-insensitive (default)
bbt my_test.md

# Case-sensitive
bbt --exact_match my_test.md
```

---

### Error 8: Whitespace in Output Matching

**Problem:** Extra whitespace causes output matching to fail.

**Symptom:** Tests fail even though the output looks correct.

**Solution:** Use `--human_match` to ignore whitespace differences, or use `contains` instead of exact matches.

```markdown
# Use contains instead of exact match
- Then output contains `success`

# Or use human match
bbt --human_match my_test.md
```

---

## Error Category: File Operations

### Error 9: File Paths Relative to Wrong Directory

**Problem:** File paths in scenarios are relative to the `.md` file, not the current working directory.

**Symptom:** Files are not found or created in unexpected locations.

**Solution:** 
- Place test files in the same directory as the `.md` file
- Or use absolute paths
- Or run bbt from the correct directory

**Example:**
```
project/
├── tests/
│   ├── test.md          # Scenarios here
│   └── input.txt        # Files referenced by test.md
└── src/
```

Run from `project/`:
```bash
bbt tests/test.md  # Will find tests/input.txt
```

---

### Error 10: File Not Created Before Use

**Problem:** Referencing a file that hasn't been created yet.

**Incorrect:**
```markdown
- When I run `cat input.txt`
- Then output contains `data`
```

**Correct:**
```markdown
- Given the file `input.txt` containing `data`
- When I run `cat input.txt`
- Then output contains `data`
```

**Symptom:** File not found errors.

**Solution:** Always create files with `Given` before using them.

---

## Error Category: Command Execution

### Error 11: Command Not in PATH

**Problem:** Command exists but is not in the system PATH.

**Symptom:** "command not found" errors.

**Solutions:**
1. Use full path to command: `` `C:\path\to\command` ``
2. Add command directory to PATH before running bbt
3. Use relative path from the `.md` file location

---

### Error 12: Command Returns Non-Zero Exit Code

**Problem:** Command fails but you expect success.

**Symptom:** Test fails with "command failed" even though output looks correct.

**Solutions:**
1. Use `When I successfully run` to explicitly require success
2. Use `Then I get no error` to verify success
3. Check if the command actually succeeds when run manually

---

## Error Category: Scenario Structure

### Error 13: Missing Scenario Header

**Problem:** Steps without a scenario header.

**Incorrect:**
```markdown
- When I run `command`
- Then output contains `result`
```

**Correct:**
```markdown
## Scenario: Test command

- When I run `command`
- Then output contains `result`
```

**Symptom:** Steps are not associated with any scenario.

**Solution:** Always include a scenario header (`## Scenario: name`).

---

### Error 14: Multiple Scenarios Without Separation

**Problem:** Scenarios run together without clear separation.

**Incorrect:**
```markdown
## Scenario: First
- When I run `cmd1`
## Scenario: Second
- When I run `cmd2`
```

**Correct:**
```markdown
## Scenario: First

- When I run `cmd1`

## Scenario: Second

- When I run `cmd2`
```

**Symptom:** Steps from different scenarios may be merged.

**Solution:** Add blank lines between scenarios for clarity.

---

## Debugging Tips

1. **Always start with `bbt explain`** to verify bbt recognizes your scenarios and steps
2. **Use `--verbose`** to see exactly what commands are being executed
3. **Use `--no-cleanup`** to inspect temporary files
4. **Manually test commands** to isolate issues
5. **Check file paths** are relative to the `.md` file

---

## Quick Checklist for Common Issues

| **Issue** | **Check** |
|-----------|-----------|
| Steps not recognized | Are keywords (`Given`/`When`/`Then`) used correctly? |
| Parameters not recognized | Are they in backticks or code blocks? |
| Code blocks not working | Do they immediately follow their step? |
| Commands not found | Is the command in PATH? |
| Files not found | Are paths relative to the `.md` file? |
| Output matching fails | Are you using `--human_match`? |
| Wrong list marker | Are you using `-` instead of `*` or `+`? |
| Scenario not recognized | Is there a `## Scenario: name` header? |

---

## See Also

- **Verbose Mode:** [verbose-mode.md](./verbose-mode.md)
- **Temporary Files:** [temporary-files.md](./temporary-files.md)
- **Syntax Reference:** [author/syntax.md](../../author/syntax.md)
