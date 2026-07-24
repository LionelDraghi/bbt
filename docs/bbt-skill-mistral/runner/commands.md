# CLI Commands Reference

Complete reference for bbt command-line interface, options, and usage examples.

---

## Basic Commands

| **Command** | **Description** | **Example** |
|-------------|----------------|-------------|
| `bbt` | Run tests in file/directory | `bbt README.md` |
| `bbt explain` | Dry run (show parsed scenarios without executing) | `bbt explain my_test.md` |
| `bbt help` | Display general help | `bbt help` |
| `bbt help grammar` | Display complete grammar reference | `bbt help grammar` |
| `bbt help example` | Generate an example scenario | `bbt help example > test.md` |
| `bbt help tutorial` | Generate a comprehensive tutorial | `bbt help tutorial` |

---

## Common Options

### Filtering Options

| **Option** | **Description** | **Example** |
|------------|----------------|-------------|
| `--recursive`, `-r` | Run tests recursively in directories | `bbt -r .` |
| `--exclude <tag>` | Exclude scenarios with the specified tag | `bbt tests/ --exclude Windows_Only` |
| `--include <tag>` | Include only scenarios with the specified tag | `bbt tests/ --include Smoke` |

**Multiple tags:**
```bash
# Exclude multiple tags
bbt tests/ --exclude Windows_Only --exclude MacOS_Only

# Include multiple tags (AND logic)
bbt tests/ --include Smoke --include Regression
```

### Execution Options

| **Option** | **Description** | **Example** |
|------------|----------------|-------------|
| `--verbose`, `-v` | Verbose mode (shows detailed execution) | `bbt --verbose my_test.md` |
| `--stop-on-error` | Stop at first failure | `bbt --stop-on-error tests/` |
| `--keep_going` | Continue after errors (default behavior) | `bbt --keep_going tests/` |
| `--cleanup` | Remove temporary files after test (default) | `bbt --cleanup tests/` |
| `--no-cleanup` | Keep temporary files for debugging | `bbt --no-cleanup my_test.md` |

### Selection Options

| **Option** | **Description** | **Example** |
|------------|----------------|-------------|
| `--select <name>` | Run only a specific scenario | `bbt README.md --select 'Sanity Check'` |
| `--yes` | Batch mode (auto-answer yes to prompts) | `bbt --yes tests/` |

### Matching Options

| **Option** | **Description** | **Example** |
|------------|----------------|-------------|
| `--human_match` | Ignore case and whitespace in output (default) | `bbt --human_match tests/` |
| `--exact_match` | Require exact output match | `bbt --exact_match tests/` |

---

## Execution Examples

### Basic Usage
```bash
# Test a specific file
bbt README.md

# Test all files in a directory
bbt docs/
bbt tests/

# Test recursively from current directory
bbt -r .
```

### Filtering Tests
```bash
# Exclude platform-specific tests (for CI on Windows systems)
bbt tests/ --exclude Unix_Only

# Include only specific test types
bbt tests/ --include Smoke --include Regression

# Run only a specific scenario
bbt README.md --select 'Version Check'
```

### Debugging and Verbose Mode
```bash
# Run with verbose output
bbt --verbose my_test.md

# Keep temporary files for inspection
bbt --verbose --no-cleanup my_test.md

# Check what scenarios bbt will run without executing
bbt explain my_test.md
```

### CI/CD Usage
```bash
# Run all tests, stop on first failure
bbt --stop-on-error tests/

# Run smoke tests only
bbt tests/ --include Smoke

# Run regression tests, verbose output
bbt --verbose tests/ --include Regression
```

---

## Typical Project Structure

```
my_project/
├── README.md           # Basic examples
├── docs/
│   └── scenarios.md    # Documentation + tests
└── tests/
    ├── features/       # Functional tests
    │   ├── auth.md
    │   ├── processing.md
    │   └── errors.md
    └── regression.md    # Regression tests
```

---

## Command Summary

| **Action** | **Command** |
|------------|-------------|
| Install bbt | `alr install bbt` |
| Build from source | `alr build` or `make build` |
| Verify installation | `bbt --version` |
| Run a test | `bbt my_test.md` |
| Show test structure | `bbt explain my_test.md` |
| Run in verbose mode | `bbt --verbose my_test.md` |
| Generate an example | `bbt help example > test.md` |
| View grammar | `bbt help grammar` |
| Exclude tags | `bbt tests/ --exclude Windows_Only` |
| Include tags | `bbt tests/ --include Smoke` |
| Run recursively | `bbt -r .` |
| Stop on first error | `bbt --stop-on-error tests/` |

---

## Tips

1. **Start with `bbt explain`** to verify bbt correctly identifies your scenarios and steps.

2. **Use `--verbose`** when debugging to see exactly what commands are being executed and what output is being compared.

3. **Use `--no-cleanup`** to inspect temporary files when debugging.

4. **Use tags** to categorize tests for easy filtering:
   ```bash
   # Run only smoke tests
   bbt tests/ --include Smoke

   # Run all tests except slow ones
   bbt tests/ --exclude Slow
   ```

5. **Use `--select`** to run a specific scenario during development:
   ```bash
   bbt my_test.md --select 'Specific scenario name'
   ```

6. **Combine options** for complex test runs:
   ```bash
   bbt -r tests/ --include Smoke --exclude Windows_Only --verbose
   ```

---

## Common Command Combinations

### Quick Test
```bash
bbt my_test.md
```

### Full Test Suite
```bash
bbt -r .
```

### Debug a Failing Test
```bash
bbt --verbose --no-cleanup failing_test.md
```

### CI Pipeline
```bash
bbt tests/ --stop-on-error --exclude Manual
```

### Nightly Regression
```bash
bbt -r tests/ --include Regression --verbose
```

### Platform-Specific Tests
```bash
# On Linux
bbt tests/ --exclude Windows_Only

# On Windows
bbt tests/ --exclude Unix_Only
```

---

## See Also

- **Installation:** [installation.md](./installation.md)
- **Debugging:** [debugging/](./debugging/)
- **Grammar Reference:** Run `bbt help grammar` or see [author/syntax.md](../author/syntax.md)
