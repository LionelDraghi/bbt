# Best Practices for Writing bbt Scenarios

Follow these guidelines to create maintainable, readable, and effective bbt tests.

---

## Writing Maintainable Tests

### 1. One Scenario = One Behavior
Each scenario should test **a single, specific behavior**. Avoid scenarios with more than 5-6 steps.

**Bad:**
```markdown
## Scenario: Complete system test

- Given the file `input1.txt` containing...
- And the file `input2.txt` containing...
- And the file `config.yaml` containing...
- When I run `step1`
- And I run `step2`
- And I run `step3`
- Then output contains `result1`
- And output contains `result2`
- And there is a file `output1.txt`
- And there is a file `output2.txt`
```

**Good:**
```markdown
## Scenario: Step1 processes input1
- Given the file `input1.txt` containing...
- When I run `step1`
- Then there is a file `output1.txt`

## Scenario: Step2 processes input2
- Given the file `input2.txt` containing...
- When I run `step2`
- Then there is a file `output2.txt`
```

---

### 2. Use Descriptive Names
Scenario names should clearly describe **what is being tested**.

**Bad:**
```markdown
### Scenario: Test 1
### Scenario: Test 2
### Scenario: My test
```

**Good:**
```markdown
### Scenario: Login with valid credentials
### Scenario: File upload with invalid type
### Scenario: Database connection timeout
```

---

### 3. Group Related Tests
Use **Feature** headers to group related scenarios.

```markdown
# Feature: Authentication

## Scenario: Login with valid credentials
- When I run `login user pass`
- Then output contains `Welcome`

## Scenario: Login with invalid credentials
- When I run `login user wrongpass`
- Then I get error

# Feature: File Management

## Scenario: Create new file
- When I run `create file.txt`
- Then there is a file `file.txt`
```

---

### 4. Document Context
Add explanations **outside** the scenario steps (these are ignored by bbt but helpful for humans).

```markdown
## Scenario: Process user data

# This scenario tests the data processing pipeline with valid input.
# Expected behavior: The system should transform CSV to JSON without errors.

- Given the file `users.csv` containing...
- When I run `processor users.csv users.json`
- Then there is a file `users.json`
```

---

### 5. Use Tags Wisely
Add tags to scenarios for filtering and categorization.

**Common Tag Patterns:**
- Platform-specific: `[Windows_Only]`, `[Unix_Only]`, `[MacOS_Only]`
- Test type: `[Smoke]`, `[Regression]`, `[Integration]`, `[Unit]`
- Feature area: `[Authentication]`, `[FileIO]`, `[Performance]`
- Priority: `[HighPriority]`, `[Critical]`

**Example:**
```markdown
## Scenario: Windows-specific feature, Windows_Only, Regression

- When I run `windows_command`
- Then output contains `Windows result`

## Scenario: Cross-platform core functionality, Smoke

- When I run `core_command`
- Then I get no error
```

---

## File Organization

### Recommended Project Structure
```
my_project/
├── README.md                    # Basic examples and documentation
├── docs/
│   ├── user_guide.md            # Documentation with embedded tests
│   └── examples/                # Advanced examples
│       ├── basic.md
│       └── advanced.md
└── tests/
    ├── features/                # Functional tests
    │   ├── auth.md               # Authentication tests
    │   ├── processing.md         # Data processing tests
    │   └── errors.md             # Error handling tests
    └── regression.md             # Regression tests
```

### When to Use Which Location
| **Location** | **Purpose** | **Example** |
|--------------|-------------|-------------|
| `README.md` | Basic examples, quick start | Simple command tests |
| `docs/` | Documentation with tests | Feature demonstrations |
| `tests/features/` | Main test suite | Functional tests |
| `tests/regression.md` | Regression tests | Tests for known bugs |

---

## Checklist Before Committing a Test

Use this checklist to ensure your scenarios are high quality:

- [ ] Scenario has a **descriptive name**
- [ ] Steps use **valid keywords** (`Given`/`When`/`Then`/`And`/`But`)
- [ ] **First step** is `Given`, `When`, or `Then` (not `And`/`But`)
- [ ] Arguments are **in backticks** (simple) or **fenced code blocks** (multiline)
- [ ] File paths are **relative to the `.md` file**
- [ ] Test passes locally (`bbt my_test.md`)
- [ ] Test passes with `--verbose` (no hidden issues)
- [ ] Tags are added if needed (`[Unix_Only]`, `[Smoke]`)
- [ ] File is **valid Markdown** (no syntax errors)
- [ ] **No bbt keywords** in decorative text (outside steps)
- [ ] Code blocks **immediately follow** their step (no empty lines)

---

## Recommended Workflow

### 1. Write a New Test
1. Create a `.md` file (e.g., `tests/my_feature.md`)
2. Write the scenario in natural English
3. Verify syntax: `bbt explain tests/my_feature.md`
4. Check for any unrecognized steps

### 2. Debug the Test
1. Run in verbose mode: `bbt --verbose tests/my_feature.md`
2. If it fails:
   - Manually test the commands
   - Check the temporary files (run without `--cleanup`)
   - Fix the scenario
3. Verify it passes with `--verbose`

### 3. Integrate into Project
1. Add the file to the Git repository
2. Run all tests: `bbt tests/`
3. (Optional) Add a Git hook to run bbt before commit:
   ```bash
   # .git/hooks/pre-commit
   #!/bin/sh
   bbt tests/ || exit 1
   ```

### 4. Maintain Tests
1. Update scenarios when behavior changes
2. Add tags for specific test types
3. Regularly verify with: `bbt tests/ --include Regression`
4. Remove obsolete tests
5. Add new tests for new features

---

## Optimization Tips

### Group Related Tests
Use **Background** for common setup across multiple scenarios.

**Document-level background** (applies to all scenarios in the file):
```markdown
## Background: Common test setup

- Given the directory `test_data`
- And the file `config.ini` containing `default=value`

## Scenario: First test
- When I run `command1`
- Then output contains `result1`

## Scenario: Second test
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

### Keep Scenarios Focused
- One scenario = one test objective
- Split complex workflows into multiple scenarios
- Use descriptive names that explain the purpose

### Use Descriptive Filenames
```
# Good
- tests/authentication/login_tests.md
- tests/file_io/file_operations.md
- tests/api/endpoint_tests.md

# Bad
- tests/test1.md
- tests/stuff.md
- tests/misc.md
```

### Reuse Common Setup
- Use Background for common prerequisites
- Create helper scripts for complex setup
- Store test data in a `test_data/` directory

---

## Code Block Nesting Rules

**Critical for LLM and Documentation:**

1. **Outer code block**: Use `~~~` with language specifier for markdown examples
2. **Inner code blocks**: Use ``` with language specifier for actual bbt code blocks
3. **Maximum nesting**: Never exceed 2 levels

**Correct Example:**
```markdown
~~~markdown
## Scenario: File creation example

- Given the file `script.sh`
```bash
#!/bin/bash
echo "Hello"
```
~~~
```

**Incorrect Example:**
```markdown
~~~markdown
~~~bash
# This is WRONG - too many nesting levels
~~~
~~~
```

---

## Avoid Snapshot Testing

Avoid test results that provide a **full reference output** if the test is focused on a specific part. Otherwise, all tests are impacted when the output format changes.

**Bad (brittle):**
```markdown
## Scenario: testing the complete --version message

- When I run `gcc --version`
- Then the output is 
~~~
gcc (Debian 14.2.0-19) 14.2.0
Copyright (C) 2024 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.
~~~
```

**Good (focused):**
```markdown
## Scenario: testing just the version number

- When I run `gcc --version`
- Then output contains `14.2.0`

## Scenario: testing just the Copyright

- When I run `gcc --version`
- Then output contains `Copyright (C) 2024 Free Software Foundation, Inc.`

## Scenario: testing just the version format

- When I run `gcc --version`
- Then output matches `(gcc|clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`
```

**Use `matches` or `contains` instead of exact matches** for better maintainability.

---

## Reference Summary

### Key Rules Checklist
- [ ] Scenarios start with Given/When/Then (not And/But)
- [ ] Parameters in backticks or code blocks
- [ ] Code blocks immediately follow their step
- [ ] No bbt keywords in decorative text
- [ ] File operations specify filenames
- [ ] Commands are executable strings

### Common Keywords
| **Category** | **Keywords** |
|--------------|-------------|
| **Actions** | run, successfully run, is, is no, contains, does not contain, get, matches |
| **Subjects** | file, output, error, dir, directory |
| **Modifiers** | new, no, not, unordered |

### Parameter Style Guide
| **Type** | **Usage** | **Example** |
|----------|-----------|-------------|
| **Inline** | Single line, short text, commands, filenames | `` `gcc --version` `` |
| **Code block** | Multiline content, file contents, expected output | \`\`\`language...\`\`\` |
| **File reference** | Reference another file | `file \`expected.txt\`` |
