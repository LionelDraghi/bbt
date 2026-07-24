# GitHub Actions Integration

Complete guide to integrating bbt tests into GitHub Actions workflows.

---

## Basic Setup

Create a workflow file at `.github/workflows/bbt-tests.yml`:

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
      
      - name: Verify installation
        run: bbt --version
      
      - name: Run bbt tests
        run: bbt -r .
```

---

## Complete Examples

### Example 1: Simple Workflow

```yaml
name: bbt Tests

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Set up Alire
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
      
      - name: Install bbt
        run: alr install bbt
      
      - name: Run tests
        run: bbt tests/
```

---

### Example 2: With Caching

Cache Alire to speed up subsequent runs:

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
      
      - name: Run full tests
        if: github.ref == 'refs/heads/main'
        run: bbt tests/
```

---

### Example 3: Platform-Specific Tests

Run tests on multiple platforms with matrix strategy:

```yaml
name: bbt Tests

on: [push, pull_request]

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
      
      - name: Add Alire to PATH
        run: echo "$HOME/.alire/bin" >> $GITHUB_PATH
        shell: bash
      
      - name: Install bbt
        run: alr install bbt
      
      - name: Run tests
        run: |
          bbt tests/ \
            --exclude ${{ matrix.os == 'windows-latest' && 'Unix_Only' || '' }} \
            --exclude ${{ matrix.os == 'macos-latest' && 'Windows_Only' || '' }} \
            --exclude ${{ matrix.os == 'ubuntu-latest' && 'MacOS_Only' || '' }}
```

---

### Example 4: With Artifacts

Save test outputs as artifacts for debugging:

```yaml
name: bbt Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Alire and bbt
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
          alr install bbt
      
      - name: Run tests with verbose
        run: bbt --verbose -r . > test_output.log 2>&1 || true
      
      - name: Upload test output
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: test-output
          path: test_output.log
```

---

### Example 5: Scheduled Nightly Tests

Run full regression tests nightly:

```yaml
name: Nightly Regression Tests

on:
  schedule:
    - cron: '0 0 * * *'  # Run at 00:00 UTC every day
  workflow_dispatch:     # Allow manual trigger

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Alire and bbt
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
          alr install bbt
      
      - name: Run regression tests
        run: bbt tests/ --include Regression --verbose
```

---

## Best Practices for GitHub Actions

### 1. Use Specific Triggers

```yaml
on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]
  workflow_dispatch:  # Allow manual runs
```

### 2. Use Caching

Always cache Alire to speed up builds:

```yaml
- name: Cache Alire
  uses: actions/cache@v3
  with:
    path: ~/.alire
    key: ${{ runner.os }}-alire-${{ hashFiles('**/alire.lock') }}
```

### 3. Use Tags for Filtering

Run different test sets based on the trigger:

```yaml
- name: Run smoke tests
  if: github.event_name == 'pull_request'
  run: bbt tests/ --include Smoke

- name: Run full tests
  if: github.ref == 'refs/heads/main' && github.event_name == 'push'
  run: bbt tests/
```

### 4. Handle Failures Gracefully

```yaml
- name: Run tests
  id: tests
  continue-on-error: true
  run: bbt tests/ --stop-on-error

- name: Upload logs on failure
  if: failure() && steps.tests.outcome == 'failure'
  uses: actions/upload-artifact@v3
  with:
    name: test-logs
    path: |
      logs/
      output/
```

### 5. Use Environment Variables

```yaml
env:
  BBT_VERSION: "latest"

jobs:
  test:
    steps:
      - name: Install specific bbt version
        run: alr install bbt@${{ env.BBT_VERSION }}
```

---

## Advanced Configurations

### Matrix Testing with Multiple bbt Versions

```yaml
jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        bbt_version: ["latest", "1.0.0", "0.9.0"]
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Alire
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
      
      - name: Install specific bbt version
        run: alr install bbt@${{ matrix.bbt_version }}
      
      - name: Run tests
        run: bbt tests/
```

---

### Parallel Test Jobs

```yaml
jobs:
  test-smoke:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install bbt
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
          alr install bbt
      - name: Run smoke tests
        run: bbt tests/ --include Smoke

  test-regression:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install bbt
        run: |
          curl -fsSL https://alire.ada.dev/download | bash
          echo "$HOME/.alire/bin" >> $GITHUB_PATH
          alr install bbt
      - name: Run regression tests
        run: bbt tests/ --include Regression
```

---

## Troubleshooting GitHub Actions

### Issue: Alire Installation Fails

**Solution:** Check the Alire installation step:
```yaml
- name: Install Alire
  run: |
    curl -fsSL https://alire.ada.dev/download | bash
    echo "$HOME/.alire/bin" >> $GITHUB_PATH
```

Ensure the PATH is set correctly for subsequent steps.

### Issue: bbt Not Found

**Solution:** Verify Alire is in PATH:
```yaml
- name: Verify bbt
  run: |
    echo "PATH: $PATH"
    which bbt || echo "bbt not found"
    bbt --version
```

### Issue: Tests Fail Only in CI

**Causes:**
- Different environment
- Missing dependencies
- Different shell behavior

**Solutions:**
1. Add setup steps to install dependencies
2. Use absolute paths in tests
3. Check shell differences (bash vs sh)
4. Add debugging to see the environment:
   ```yaml
   - name: Debug environment
     run: |
       env
       which bash
       which sh
       ls -la
   ```

---

## Useful GitHub Actions Features

### Workflow Dispatch

Allow manual triggering of workflows:

```yaml
on:
  workflow_dispatch:
    inputs:
      test_type:
        description: 'Type of tests to run'
        required: true
        default: 'smoke'
        type: choice
        options:
        - smoke
        - regression
        - all

jobs:
  test:
    steps:
      - name: Run selected tests
        run: bbt tests/ --include ${{ github.event.inputs.test_type }}
```

### Concurrency Control

Prevent concurrent runs of the same workflow:

```yaml
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
```

### Notifications

Send notifications on failure:

```yaml
- name: Notify on failure
  if: failure()
  uses: rtCamp/action-slack-notify@v2
  env:
    SLACK_WEBHOOK: ${{ secrets.SLACK_WEBHOOK }}
    SLACK_COLOR: danger
    SLACK_TITLE: "bbt Tests Failed"
```

---

## See Also

- **GitLab CI:** [gitlab-ci.md](./gitlab-ci.md)
- **CI/CD Overview:** [ci-cd.md](../ci-cd.md)
- **CLI Commands:** [commands.md](../commands.md)
