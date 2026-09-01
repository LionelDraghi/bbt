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
Add tags to scenarios for filtering and categorization, according to project practices.

**Common Tag Patterns:**
- Platform-specific: `[Windows_Only]`, `[Unix_Only]`, `[MacOS_Only]`
- Test type: `[Smoke]`, `[Regression]`, `[Integration]`, `[Unit]`
- Feature area: `[Authentication]`, `[FileIO]`, `[Performance]`
- Priority: `[HighPriority]`, `[Critical]`

**Example:**
```markdown
## Scenario: Windows-specific Autentification feature, Windows_Only, Regression

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
