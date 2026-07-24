# Verbose Mode

Using `--verbose` flag to debug bbt scenarios and understand execution details.

---

## What is Verbose Mode?

The `--verbose` (or `-v`) flag makes bbt output **detailed information** about:
- Which scenarios are being executed
- The exact commands being run
- The actual output from commands
- The expected output for comparisons
- Which assertions pass or fail
- Temporary files created

---

## When to Use Verbose Mode

Use `--verbose` when:
- A test is failing and you need to see **why**
- You want to see the **exact command** being executed
- You need to compare **actual vs expected output**
- You're debugging **file operations**
- You're investigating **temporary files**
- You're learning how bbt interprets your scenarios

---

## Basic Usage

```bash
# Run a single test with verbose output
bbt --verbose my_test.md

# Run all tests in a directory with verbose
bbt --verbose tests/

# Combine with other options
bbt --verbose --no-cleanup my_test.md
bbt -v -r .  # Short form
```

---

## Understanding Verbose Output

### Sample Verbose Output

```
Scenario: Check GCC installation
-----------------------------------
Step 1: When I run `gcc --version`
  Command: gcc --version
  Exit code: 0
  Output:
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    Copyright (C) 2021 Free Software Foundation, Inc.
    ...

Step 2: Then output contains `gcc`
  Expected: gcc
  Actual output: gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
  Result: PASS (output contains "gcc")

Step 3: And I get no error
  Result: PASS (exit code was 0)

Scenario: PASS
```

### Key Elements in Verbose Output

| **Element** | **Description** | **What to Look For** |
|-------------|----------------|----------------------|
| **Scenario name** | Name of the current scenario | Verify it's the right scenario |
| **Step number** | Sequence of steps | Check all steps are executed |
| **Command** | The actual command run | Verify command is correct |
| **Exit code** | Command's exit code | 0 = success, non-zero = error |
| **Output** | Command's stdout/stderr | Compare with expectations |
| **Expected** | What bbt expects | Verify matches your intent |
| **Actual** | What was actually output | Look for differences |
| **Result** | PASS/FAIL for each assertion | Identify which check failed |

---

## Debugging with Verbose Mode

### Debugging Output Matching Failures

**Problem:** Test fails on output matching.

**Solution:**
```bash
bbt --verbose my_test.md
```

Look for:
1. The **actual output** from the command
2. The **expected text** bbt is looking for
3. Any **differences** in case, whitespace, or content

**Example:**
```
Step 2: Then output contains `Success`
  Expected: Success
  Actual output: SUCCESS
  Result: FAIL
```

**Fix:** Either:
- Change expected to `SUCCESS`
- Or use `--human_match` for case-insensitive matching
- Or use regex: `Then output matches `(?i)success`

---

### Debugging Command Execution

**Problem:** Command is not running as expected.

**Solution:**
```bash
bbt --verbose my_test.md
```

Check:
1. The **Command:** line shows the exact command being run
2. The **Exit code:** shows if the command succeeded (0) or failed (non-zero)
3. The **Output:** shows what the command produced

**Example:**
```
Step 1: When I run `my_script.sh`
  Command: my_script.sh
  Exit code: 127
  Output: /bin/sh: my_script.sh: not found
  Result: COMMAND FAILED
```

**Fix:**
- Command is not in PATH or doesn't exist
- Use full path: `` `./my_script.sh` ``
- Or ensure script is in PATH

---

### Debugging File Operations

**Problem:** Files are not being created, read, or compared correctly.

**Solution:**
```bash
bbt --verbose --no-cleanup my_test.md
```

Check:
1. File creation steps show the file path
2. File content verification shows expected vs actual
3. Temporary directory contains the files

**Example:**
```
Step 1: Given the file `test.txt` containing `hello`
  File created: /tmp/bbt_temp_12345/test.txt
  Content: hello

Step 2: Then file `test.txt` contains `hello`
  Expected: hello
  Actual: hello
  Result: PASS
```

**Fix:** If file content doesn't match:
- Check code block formatting (must immediately follow step)
- Verify no extra whitespace or formatting

---

### Debugging Multiple Scenarios

**Problem:** One of many scenarios is failing.

**Solution:**
```bash
# Run all scenarios with verbose
bbt --verbose tests/

# Or run specific scenario
bbt --verbose my_test.md --select 'Scenario Name'
```

Look for:
- Which scenario is failing
- Which specific step is failing
- The reason for failure

---

## Verbose Mode Tips

### Tip 1: Save Verbose Output to File

```bash
bbt --verbose my_test.md > debug.log 2>&1
```

This saves the complete verbose output for later analysis.

### Tip 2: Compare Verbose Output Over Time

Save verbose output before and after changes to see what changed:
```bash
bbt --verbose my_test.md > before.log
# Make changes
bbt --verbose my_test.md > after.log
diff before.log after.log
```

### Tip 3: Use with Explain Mode

First check if bbt recognizes your scenarios correctly:
```bash
bbt explain my_test.md
```

Then run with verbose to see execution:
```bash
bbt --verbose my_test.md
```

### Tip 4: Check Environment Variables

Verbose mode shows environment but not environment variables. If commands depend on env vars:
```bash
# Print environment before running
set || env
bbt --verbose my_test.md
```

### Tip 5: Debug Background Steps

Background steps are shown in verbose output:
```
Background: Common setup
------------------------
Step 1: Given the directory `test_data`
  Command: mkdir test_data
  Exit code: 0
  Result: PASS
```

---

## Understanding Exit Codes in Verbose Output

| **Exit Code** | **Meaning** | **bbt Behavior** |
|---------------|-------------|------------------|
| 0 | Success | Continues, `I get no error` passes |
| 1-255 | Error | `I get error` passes, `I get no error` fails |
| Special: Command not found | 127 | Treated as error |

---

## Common Patterns in Verbose Output

### Successful Command
```
Step 1: When I run `echo hello`
  Command: echo hello
  Exit code: 0
  Output: hello
  Result: COMMAND SUCCESS
```

### Failed Command
```
Step 1: When I run `false`
  Command: false
  Exit code: 1
  Output: (empty)
  Result: COMMAND FAILED
```

### Output Contains Match
```
Step 2: Then output contains `hello`
  Expected: hello
  Actual: hello
  Result: PASS
```

### Output Contains No Match
```
Step 2: Then output contains `world`
  Expected: world
  Actual: hello
  Result: FAIL - output does not contain "world"
```

### File Exists Check
```
Step 1: Then there is a file `test.txt`
  File path: /tmp/bbt_temp_12345/test.txt
  Exists: YES
  Result: PASS
```

### File Does Not Exist
```
Step 1: Then there is a file `missing.txt`
  File path: /tmp/bbt_temp_12345/missing.txt
  Exists: NO
  Result: FAIL
```

---

## See Also

- **Common Errors:** [common-errors.md](./common-errors.md)
- **Temporary Files:** [temporary-files.md](./temporary-files.md)
- **CLI Commands:** [commands.md](../commands.md)
