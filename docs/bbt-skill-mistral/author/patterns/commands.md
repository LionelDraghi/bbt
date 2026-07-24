# Command Execution Patterns

Use these patterns for running commands and verifying their execution.

---

## Basic Command Execution

### Simple Command
Run a command and verify it executes without error.

```markdown
- When I run `command --args`
- Then I get no error
```

**Example:**
```markdown
## Scenario: Check if GCC is installed

- When I run `gcc --version`
- Then I get no error
```

---

### Command with Expected Output
Run a command and verify its output contains specific text.

```markdown
- When I run `command`
- Then output contains `expected_text`
```

**Example:**
```markdown
## Scenario: Check GCC version

- When I run `gcc --version`
- Then output contains `14.2.0`
```

---

### Successful Execution
Use `successfully run` to explicitly require the command to succeed (exit code 0).

```markdown
- When I successfully run `command`
```

**Example:**
```markdown
## Scenario: Compile a program

- When I successfully run `make`
- Then there is a file `program.exe`
```

---

## Advanced Command Patterns

### Exact Output Match
Verify the **exact** output of a command.

```markdown
- When I run `command`
- Then I get
```
Expected output line 1
Expected output line 2
```
```

**Example:**
```markdown
## Scenario: Check exact version output

- When I run `program --version`
- Then I get
```
Version 1.0.0
```
```

### Output from File
Compare output against a file's contents.

```markdown
- When I run `command`
- Then output contains file `expected_output.txt`
```

---

### Regex Matching
Use regular expressions for flexible output matching.

```markdown
- When I run `command`
- Then output matches `regex_pattern`
```

**Example:**
```markdown
## Scenario: Check version format

- When I run `gcc --version`
- Then output matches `(gcc|clang) version [0-9]+\.[0-9]+\.[0-9]+`
```

---

### Negative Matching
Verify output does **not** contain certain text.

```markdown
- When I run `command`
- Then output does not contain `error`
```

**Example:**
```markdown
## Scenario: Command should not produce errors

- When I run `validator input.txt`
- Then output does not contain `ERROR`
- And I get no error
```

---

## Chaining Commands

### Sequential Commands
Chain multiple commands in a single scenario.

```markdown
- When I run `command1`
- And I run `command2`
- Then output contains `expected_result`
```

**Example:**
```markdown
## Scenario: Build and test

- When I successfully run `make clean`
- And I successfully run `make`
- Then there is a file `program`
- When I run `./program`
- Then output contains `Success`
```

---

## Common Use Cases

| **Use Case** | **Pattern** |
|--------------|-------------|
| Check if a tool is installed | `- When I run `tool --version`<br>`- Then I get no error` |
| Verify command output | `- When I run `command`<br>`- Then output contains `text` |
| Test error handling | `- When I run `invalid_command`<br>`- Then I get error` |
| Chain commands | `- When I run `cmd1`<br>`- And I run `cmd2` |
| Verify exact output | `- When I run `command`<br>`- Then I get`<br>```exact output``` |
| Use regex for flexible matching | `- Then output matches `pattern` |

---

## Tips

1. **Use `successfully run`** when you want to explicitly fail if the command returns a non-zero exit code.
2. **Use `I get no error`** to verify the command succeeded (implicit check).
3. **Prefer `contains` over `is`** for output verification to avoid brittle tests.
4. **Use regex** when you need to match patterns (e.g., version numbers, timestamps).
5. **Chain commands** with `And I run` for multi-step workflows.
