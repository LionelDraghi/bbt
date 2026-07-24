# Temporary Files in bbt

Understanding and using temporary files for debugging bbt scenarios.

---

## What are Temporary Files?

When bbt runs, it:
1. Creates a **temporary directory** for each test file
2. **Copies** the `.md` file to this directory
3. **Creates** files specified in `Given` steps
4. **Executes** commands in this directory
5. **Compares** outputs and file contents
6. **Cleans up** (by default) after the test completes

---

## Accessing Temporary Files

### Keep Temporary Files with `--no-cleanup`

By default, bbt removes temporary files after execution. To keep them for inspection:

```bash
bbt --no-cleanup my_test.md
bbt --no-cleanup --verbose my_test.md  # Combined with verbose
```

### Find Temporary Files

After running with `--no-cleanup`, bbt will print the temporary directory path:

```
Temporary directory kept at: /tmp/bbt_temp_12345
```

You can also find it in the verbose output:
```
Working directory: /tmp/bbt_temp_12345
```

---

## When to Use Temporary Files

Use `--no-cleanup` when:
- A test is failing and you need to **inspect created files**
- You want to **verify file contents** manually
- You're debugging **file operations**
- You need to **check command outputs** in files
- You're developing new scenarios and want to **see the environment**

---

## Structure of Temporary Directory

The temporary directory contains:

```
/tmp/bbt_temp_12345/
├── my_test.md               # Copy of the original test file
├── bbt_execution.log        # Log of commands executed (if verbose)
├── input.txt                # Files created by Given steps
├── output.txt               # Files created by commands
├── expected.txt             # Files used for comparisons
└── ...                      # Other temporary files
```

---

## Debugging with Temporary Files

### Example: File Content Verification

**Test:**
```markdown
## Scenario: Create and verify file

- Given the file `test.txt` containing
```
line 1
line 2
line 3
```
- Then file `test.txt` contains `line 2`
```

**Debug:**
```bash
bbt --no-cleanup --verbose my_test.md
```

**Inspect:**
```bash
# Navigate to temp directory (path from output)
cd /tmp/bbt_temp_12345

# Check the file was created
ls -la
test.txt

# Verify content
cat test.txt

# Compare with expected
head test.txt  # Should show "line 1"
```

---

### Example: Command Output to File

**Test:**
```markdown
## Scenario: Command writes to file

- When I run `echo "Hello" > output.txt`
- Then file `output.txt` contains `Hello`
```

**Debug:**
```bash
bbt --no-cleanup my_test.md
cd /tmp/bbt_temp_12345
cat output.txt  # Should contain "Hello"
```

---

### Example: File Comparison

**Test:**
```markdown
## Scenario: Files are equal

- Given the file `expected.txt` containing `data`
- And given the file `actual.txt` containing `data`
- Then file `actual.txt` is equal to file `expected.txt`
```

**Debug:**
```bash
bbt --no-cleanup my_test.md
cd /tmp/bbt_temp_12345

# Compare files manually
diff expected.txt actual.txt

# Or check content
cat expected.txt
cat actual.txt
```

---

## Temporary File Locations

| **Platform** | **Default Location** | **Note** |
|-------------|---------------------|----------|
| Linux | `/tmp/bbt_temp_*` | Requires `/tmp` write access |
| macOS | `/tmp/bbt_temp_*` | Same as Linux |
| Windows | `%TEMP%\bbt_temp_*` | Usually `C:\Users\<user>\AppData\Local\Temp\bbt_temp_*` |

---

## Cleanup Behavior

| **Option** | **Behavior** | **When to Use** |
|------------|--------------|-----------------|
| `--cleanup` (default) | Remove temp files after test | Normal execution |
| `--no-cleanup` | Keep temp files | Debugging |

**Manual Cleanup:**
```bash
# Find and remove old temp directories
rm -rf /tmp/bbt_temp_*

# On Windows
rmdir /s /q %TEMP%\bbt_temp_*
```

---

## Tips for Using Temporary Files

### Tip 1: Combine with Verbose Mode
```bash
bbt --verbose --no-cleanup my_test.md
```

This shows:
- The temp directory path
- Commands being executed
- Files being created

### Tip 2: Inspect Files During Development

When writing new scenarios:
1. Create the scenario
2. Run with `--no-cleanup --verbose`
3. Inspect the temp directory
4. Fix any issues
5. Re-run until it passes
6. Remove `--no-cleanup` for final run

### Tip 3: Check Working Directory

Commands are executed in the temp directory, so:
- File paths in scenarios are relative to the `.md` file
- But commands execute in the temp directory
- Use absolute paths if you need to reference files outside the temp dir

### Tip 4: Multiple Test Files

Each test file gets its own temp directory:
```bash
bbt --no-cleanup test1.md test2.md
# Creates: /tmp/bbt_temp_12345 (for test1.md)
# Creates: /tmp/bbt_temp_67890 (for test2.md)
```

### Tip 5: Debug Background Steps

Background steps also create files in the temp directory:
```markdown
## Background: Setup
- Given the file `config.ini` containing `key=value`
```

Will create `config.ini` in the temp directory.

---

## Common Issues with Temporary Files

### Issue 1: Files Not Found

**Problem:** Command can't find a file that should exist.

**Causes:**
- File path is wrong (relative to `.md` file, not temp dir)
- File wasn't created (check `Given` steps)
- Code block doesn't immediately follow step

**Solution:**
```bash
bbt --no-cleanup --verbose my_test.md
cd /tmp/bbt_temp_12345
ls -la  # Check if file exists
```

### Issue 2: File Content Incorrect

**Problem:** File content doesn't match what you specified.

**Causes:**
- Code block has wrong indentation
- Extra blank lines in code block
- Content was modified by a command

**Solution:**
```bash
bbt --no-cleanup my_test.md
cd /tmp/bbt_temp_12345
cat filename.txt  # Inspect actual content
```

### Issue 3: Permission Issues

**Problem:** Can't create or read files in temp directory.

**Causes:**
- Temp directory permissions
- Filesystem is read-only

**Solution:**
- Check permissions: `ls -la /tmp`
- Try a different temp location: `export TMPDIR=/some/writable/dir`

---

## Example: Full Debug Session

**Scenario:**
```markdown
## Scenario: Process data file

- Given the file `input.csv` containing
```csv
name,age
Alice,30
Bob,25
```
- When I run `./processor input.csv output.json`
- Then there is a file `output.json`
- And file `output.json` contains `Alice`
```

**Debug Session:**
```bash
# Run with no cleanup and verbose
$ bbt --no-cleanup --verbose data_test.md
Working directory: /tmp/bbt_temp_54321
Scenario: Process data file
  Step 1: Given the file `input.csv` containing...
    File created: /tmp/bbt_temp_54321/input.csv
  Step 2: When I run `./processor input.csv output.json`
    Command: ./processor input.csv output.json
    Exit code: 0
  Step 3: Then there is a file `output.json`
    File exists: YES
  Step 4: And file `output.json` contains `Alice`
    Expected: Alice
    Actual: {"name":"Alice","age":30}
    Result: PASS

Temporary directory kept at: /tmp/bbt_temp_54321

# Navigate to temp directory
$ cd /tmp/bbt_temp_54321

# List files
$ ls -la
total 8
-rw-r--r-- 1 user user 21 Jun 10 10:00 input.csv
-rw-r--r-- 1 user user 27 Jun 10 10:00 output.json

# Check input
$ cat input.csv
name,age
Alice,30
Bob,25

# Check output
$ cat output.json
{"name":"Alice","age":30}

# Test processor manually
$ ./processor input.csv test_output.json
$ cat test_output.json
```

---

## See Also

- **Verbose Mode:** [verbose-mode.md](./verbose-mode.md)
- **Common Errors:** [common-errors.md](./common-errors.md)
- **CLI Commands:** [commands.md](../commands.md)
