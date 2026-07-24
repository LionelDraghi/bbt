# CI/CD Integration

This directory contains examples and guides for integrating bbt into Continuous Integration/Continuous Deployment pipelines.

---

## 📚 CI/CD Guides

| **Platform** | **Description** | **File** |
|--------------|----------------|----------|
| **GitHub Actions** | Integration with GitHub | [github-actions.md](./github-actions.md) |
| **GitLab CI** | Integration with GitLab | [gitlab-ci.md](./gitlab-ci.md) |

---

## Quick Start

### GitHub Actions

Create a file `.github/workflows/bbt-tests.yml`:

```yaml
name: bbt Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Alire
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
      
      - name: Install bbt
        run: alr install bbt
      
      - name: Run bbt tests
        run: bbt docs/features/
```

---

### GitLab CI

Create a file `.gitlab-ci.yml`:

```yaml
stages:
  - test

test:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt docs/features/
```

---

## Best Practices for CI/CD

### 1. Use Tags for CI Filtering

Tag your scenarios to run specific tests in CI:

```markdown
## Scenario: Core functionality, Smoke, CI

- When I run `core_command`
- Then I get no error

## Scenario: Platform-specific, Windows_Only

- When I run `windows_command`
- Then output contains `Windows result`
```

Then in CI, exclude platform-specific tests:
```bash
# On Linux CI
bbt tests/ --exclude Windows_Only --exclude MacOS_Only

# On Windows CI
bbt tests/ --exclude Unix_Only
```

---

### 2. Stop on First Failure

Use `--stop-on-error` to fail fast in CI:
```bash
bbt --stop-on-error tests/
```

---

### 3. Use Verbose Mode for Debugging

When debugging CI failures, use verbose mode:
```bash
bbt --verbose tests/ --include FailingTest
```

---

### 4. Cache Dependencies

Cache Alire and bbt to speed up CI runs.

**GitHub Actions:**
```yaml
- name: Cache Alire
  uses: actions/cache@v3
  with:
    path: ~/.alire
    key: ${{ runner.os }}-alire-${{ hashFiles('**/alire.lock') }}
    restore-keys: |
      ${{ runner.os }}-alire-
```

---

### 5. Matrix Testing

Test across multiple platforms/versions.

**GitHub Actions Example:**
```yaml
jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
    steps:
      - uses: actions/checkout@v4
      - name: Install Alire
        run: curl -fsSL https://alire.ada.dev/download | bash
      - name: Install bbt
        run: alr install bbt
      - name: Run tests
        run: bbt tests/ --exclude ${{ matrix.os == 'windows-latest' && 'Unix_Only' || 'Windows_Only' }}
```

---

## CI/CD Examples by Platform

### Basic GitHub Actions Workflow

```yaml
name: bbt Tests

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Alire
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
      
      - name: Install bbt
        run: alr install bbt
      
      - name: Verify bbt installation
        run: bbt --version
      
      - name: Run bbt tests
        run: bbt -r .
```

---

### Advanced GitHub Actions with Caching

```yaml
name: bbt Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Cache Alire
        uses: actions/cache@v3
        with:
          path: ~/.alire
          key: ${{ runner.os }}-alire-${{ hashFiles('**/alire.lock') }}
          restore-keys: |
            ${{ runner.os }}-alire-
      
      - name: Install Alire
        if: steps.cache-alire.outputs.cache-hit != 'true'
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
      
      - name: Install bbt
        run: alr install bbt
      
      - name: Run smoke tests
        run: bbt tests/ --include Smoke
      
      - name: Run regression tests
        run: bbt tests/ --include Regression
```

---

### Basic GitLab CI Pipeline

```yaml
stages:
  - test

variables:
  ALIRE_ROOT: "$CI_PROJECT_DIR/.alire"

cache:
  key: alire
  paths:
    - .alire/

.test:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - alr install bbt
    - bbt --version
    - bbt -r .
  tags:
    - linux
```

---

### Advanced GitLab CI with Multiple Jobs

```yaml
stages:
  - setup
  - test
  - deploy

variables:
  ALIRE_ROOT: "$CI_PROJECT_DIR/.alire"

cache:
  key: alire
  paths:
    - .alire/

setup:
  stage: setup
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - alr install bbt
  artifacts:
    paths:
      - .alire/

test:
  stage: test
  script:
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - bbt --version
    - bbt tests/ --stop-on-error
  dependencies:
    - setup

deploy:
  stage: deploy
  script:
    - echo "Deploying..."
  only:
    - main
```

---

## CI/CD Tips

### Tip 1: Start Simple

Begin with a simple CI configuration that runs all tests, then add complexity as needed.

### Tip 2: Use Tags Effectively

Tag your tests to run different sets in different CI jobs:
- `[Smoke]` - Quick tests for every commit
- `[Regression]` - Full regression suite for nightly runs
- `[Integration]` - Integration tests
- `[Manual]` - Tests that require manual intervention

### Tip 3: Fail Fast

Use `--stop-on-error` to stop at the first failure and save CI time.

### Tip 4: Platform-Specific Tests

Use tags to exclude platform-specific tests on incompatible platforms:
```bash
# On Linux
bbt tests/ --exclude Windows_Only --exclude MacOS_Only

# On Windows
bbt tests/ --exclude Unix_Only
```

### Tip 5: Environment Variables

Set environment variables in CI that your tests might need:
```yaml
# GitHub Actions
env:
  MY_VAR: value

# GitLab CI
variables:
  MY_VAR: value
```

### Tip 6: Artifacts

Save test outputs as artifacts for debugging:
```yaml
# GitHub Actions
- name: Upload test outputs
  uses: actions/upload-artifact@v3
  if: failure()
  with:
    name: test-outputs
    path: |
      output/
      logs/
```

---

## Troubleshooting CI/CD

### Issue: Tests pass locally but fail in CI

**Causes:**
- Different environment (PATH, variables, etc.)
- Different tool versions
- Different filesystem behavior
- Missing dependencies

**Solutions:**
1. Check environment differences
2. Use the same tool versions
3. Add setup steps to CI configuration
4. Use absolute paths in tests

### Issue: CI runs are slow

**Solutions:**
- Use caching for dependencies
- Run smoke tests on every commit, full tests nightly
- Use `--stop-on-error` to fail fast
- Parallelize tests if possible

### Issue: Flaky tests in CI

**Solutions:**
- Investigate and fix flaky tests
- Use retry logic in CI
- Mark flaky tests with a `[Flaky]` tag and investigate separately

---

## See Also

- **GitHub Actions:** [github-actions.md](./github-actions.md)
- **GitLab CI:** [gitlab-ci.md](./gitlab-ci.md)
- **CLI Commands:** [commands.md](./commands.md)
- **Debugging:** [debugging/](./debugging/)
