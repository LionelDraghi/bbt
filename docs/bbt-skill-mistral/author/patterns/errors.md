# Error Handling Patterns

Use these patterns to test error conditions and verify error messages in bbt scenarios.

---

## Basic Error Detection

### Check for Any Error
Verify that a command produces an error (non-zero exit code).

```markdown
- Then I get error
```

**Example:**
```markdown
## Scenario: Invalid command produces error

- When I run `nonexistent_command`
- Then I get error
```

---

### Check for Specific Error Message
Verify that the output contains a specific error message.

```markdown
- Then I get error
- And output contains `error_message`
```

**Example:**
```markdown
## Scenario: File not found error

- When I run `cat nonexistent_file.txt`
- Then I get error
- And output contains `No such file or directory`
```

---

## Successful Execution

### Verify No Error
Explicitly verify that a command succeeds (exit code 0).

```markdown
- Then I get no error
```

**Example:**
```markdown
## Scenario: Command succeeds without errors

- When I run `valid_command`
- Then I get no error
```

---

### Successful Run (Explicit)
Use `successfully run` to require the command to succeed.

```markdown
- When I successfully run `command`
```

**Example:**
```markdown
## Scenario: Build succeeds

- When I successfully run `make`
- Then there is a file `program`
```

---

## Advanced Error Patterns

### Multiple Error Checks
Verify multiple aspects of an error.

```markdown
- When I run `command`
- Then I get error
- And output contains `Error: `
- And output contains `Line 10`
- And output matches `[Ee]rror: .*`
```

**Example:**
```markdown
## Scenario: Syntax error with details

- Given the file `invalid.py` containing `def broken(`
- When I run `python invalid.py`
- Then I get error
- And output contains `SyntaxError`
- And output contains `invalid syntax`
```

---

### Negative Error Check
Verify that a command does **not** produce an error.

```markdown
- When I run `command`
- Then I do not get error
```

*Note: This is equivalent to `Then I get no error`.*

---

## Error Message Patterns

### Match Error with Regex
Use regular expressions to match error patterns.

```markdown
- Then I get error
- And output matches `Error: [A-Za-z]+`
```

**Example:**
```markdown
## Scenario: Error code in output

- When I run `processor invalid_input`
- Then I get error
- And output matches `Error [0-9]+: .*`
```

---

### Error Output Does Not Contain
Verify that an error message does **not** contain certain text.

```markdown
- When I run `command`
- Then I get error
- And output does not contain `unexpected_text`
```

---

## Common Use Cases

| **Use Case** | **Pattern** |
|--------------|-------------|
| Check for any error | `- Then I get error` |
| Check for specific error message | `- Then I get error`<br>`- And output contains `message` |
| Verify no error | `- Then I get no error` |
| Require successful execution | `- When I successfully run `command` |
| Match error with regex | `- Then I get error`<br>`- And output matches `pattern` |
| Multiple error checks | `- Then I get error`<br>`- And output contains `text1`<br>`- And output contains `text2` |

---

## Tips

1. **Use `I get error`** for general error detection.
2. **Add `And output contains`** to verify specific error messages.
3. **Use `successfully run`** for commands that must succeed.
4. **Use `I get no error`** to explicitly verify success.
5. **Combine with output checks** for comprehensive error verification.

---

## Complete Examples

### Example 1: Invalid Input
```markdown
## Scenario: Invalid input produces error

- Given the file `invalid.txt` containing `corrupted data`
- When I run `validator invalid.txt`
- Then I get error
- And output contains `Invalid format`
- And output contains `Line 1: Syntax error`
```

### Example 2: Missing Dependency
```markdown
## Scenario: Missing dependency error

- When I run `program --feature`
- Then I get error
- And output contains `Missing dependency`
- And output matches `Error: [A-Za-z]+ not found`
```

### Example 3: Permission Denied
```markdown
## Scenario: Permission denied error

- Given the file `protected.txt`
- When I run `cat protected.txt`
- Then I get error
- And output contains `Permission denied`
```

### Example 4: Successful Error Handling
```markdown
## Scenario: Program handles errors gracefully

- Given the file `invalid.json` containing `{bad json}`
- When I run `parser invalid.json`
- Then I get error
- And output contains `JSON parse error`
- And output matches `Error at line [0-9]+`
- And I get no error  # This would fail - contradiction!
```

*Note: The last line in Example 4 is incorrect. A scenario cannot both get an error and not get an error. This is just to illustrate what **not** to do.*

---

## Best Practices for Error Testing

1. **Test both success and failure cases** for each feature.
2. **Be specific** with error messages to avoid false positives.
3. **Use tags** to categorize error tests (e.g., `[ErrorHandling]`, `[NegativeTest]`).
4. **Keep error tests separate** from happy-path tests for clarity.
5. **Document expected errors** in the scenario description.
