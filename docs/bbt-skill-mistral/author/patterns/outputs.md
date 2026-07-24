# Output Verification Patterns

Use these patterns to verify command output in bbt scenarios.

---

## Basic Output Verification

### Partial Match (Contains)
Verify that output **contains** specific text (most common pattern).

```markdown
- Then output contains `text`
```

**Example:**
```markdown
## Scenario: Check for success message

- When I run `program --status`
- Then output contains `success`
```

---

### Exact Match
Verify the **exact** output of a command.

```markdown
- Then I get
```
Exact output line 1
Exact output line 2
```
```

**Example:**
```markdown
## Scenario: Verify exact version string

- When I run `program --version`
- Then I get
```
Version 2.1.0
```
```

---

## Advanced Output Patterns

### Regex Matching
Use regular expressions for flexible output matching.

```markdown
- Then output matches `regex_pattern`
```

**Example:**
```markdown
## Scenario: Verify version format

- When I run `gcc --version`
- Then output matches `(gcc|clang) version [0-9]+\.[0-9]+\.[0-9]+`
```

**Common Regex Patterns:**
```
# Match a version number
[0-9]+\.[0-9]+\.[0-9]+

# Match a timestamp
\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}

# Match "Success" or "OK"
(Success|OK|Passed)

# Match an error code
Error: [A-Z0-9]+
```

---

### Negative Matching
Verify output does **not** contain certain text.

```markdown
- Then output does not contain `text`
```

**Example:**
```markdown
## Scenario: Verify no errors in output

- When I run `validator input.txt`
- Then output does not contain `ERROR`
```

---

### Regex Negative Matching
Verify output does **not** match a pattern.

```markdown
- Then output does not match `error_pattern`
```

**Example:**
```markdown
## Scenario: Verify no error codes in output

- When I run `processor data.txt`
- Then output does not match `[Ee]rror: .*`
```

---

## Unordered Content Matching

### Verify Output Contains All Items (Any Order)
Use when the order of output lines doesn't matter.

```markdown
- Then output contains unordered
```
item1
item2
item3
```
```

**Example:**
```markdown
## Scenario: Verify all expected files are listed

- When I run `ls output/`
- Then output contains unordered
```
file1.txt
file2.txt
file3.txt
```
```

---

## Output from File

### Compare Output to File Contents
Verify that command output matches the contents of a file.

```markdown
- Then output contains file `filename`
```

**Example:**
```markdown
## Scenario: Verify output matches expected file

- When I run `generate_report`
- Then output contains file `expected_report.txt`
```

---

## Multiple Output Verifications

### Chain Multiple Output Checks
Verify multiple aspects of the output.

```markdown
- When I run `command`
- Then output contains `text1`
- And output contains `text2`
- And output does not contain `error`
```

**Example:**
```markdown
## Scenario: Verify complete build output

- When I run `make`
- Then output contains `Building...`
- And output contains `Linking...`
- And output contains `Done.`
- And I get no error
```

---

## Common Use Cases

| **Use Case** | **Pattern** |
|--------------|-------------|
| Basic text match | `- Then output contains `text` |
| Exact output match | `- Then I get`<br>```output``` |
| Flexible matching | `- Then output matches `regex` |
| Negative match | `- Then output does not contain `text` |
| Negative regex match | `- Then output does not match `pattern` |
| Unordered items | `- Then output contains unordered`<br>```item1\nitem2``` |
| Compare to file | `- Then output contains file `filename` |

---

## Tips

1. **Prefer `contains` over exact matches** to avoid brittle tests that break when output format changes.
2. **Use regex** for version numbers, timestamps, or other dynamic content.
3. **Use `unordered`** when the order of output lines is not guaranteed.
4. **Combine multiple checks** with `And` for comprehensive verification.
5. **Store expected output in files** for large or complex expected outputs.

---

## Complete Examples

### Example 1: Version Check
```markdown
## Scenario: Check program version

- When I run `program --version`
- Then output contains `1.2.3`
- And output matches `Version [0-9]+\.[0-9]+\.[0-9]+`
- And I get no error
```

### Example 2: Build Process Verification
```markdown
## Scenario: Verify build process

- When I run `make`
- Then output contains `Compiling main.c`
- And output contains `Linking...`
- And output contains `Build successful`
- And I get no error
```

### Example 3: Error Detection
```markdown
## Scenario: Verify error is detected

- Given the file `invalid.txt` containing `bad data`
- When I run `validator invalid.txt`
- Then I get error
- And output contains `Invalid format`
- And output matches `Error: .*`
```
