# GitLab CI Integration

Complete guide to integrating bbt tests into GitLab CI pipelines.

---

## Basic Setup

Create a file `.gitlab-ci.yml` at the root of your repository:

```yaml
stages:
  - test

.test:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt --version
    - bbt -r .
```

---

## Complete Examples

### Example 1: Simple Pipeline

```yaml
stages:
  - test

test:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt -r .
  tags:
    - linux
```

---

### Example 2: With Caching

Cache Alire to speed up subsequent runs:

```yaml
stages:
  - test

variables:
  ALIRE_ROOT: "$CI_PROJECT_DIR/.alire"

cache:
  key: alire
  paths:
    - .alire/

test:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - alr install bbt
    - bbt -r .
  tags:
    - linux
```

---

### Example 3: Multiple Jobs

Separate smoke tests from full regression tests:

```yaml
stages:
  - smoke
  - regression

variables:
  ALIRE_ROOT: "$CI_PROJECT_DIR/.alire"

cache:
  key: alire
  paths:
    - .alire/

setup:
  stage: .pre
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - alr install bbt
  artifacts:
    paths:
      - .alire/
  tags:
    - linux

smoke:
  stage: smoke
  script:
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - bbt tests/ --include Smoke --stop-on-error
  dependencies:
    - setup
  tags:
    - linux

regression:
  stage: regression
  script:
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - bbt tests/ --include Regression
  dependencies:
    - setup
  only:
    - main
    - develop
  tags:
    - linux
```

---

### Example 4: Platform-Specific Tests

Run tests on different platforms using tags:

```yaml
stages:
  - test

.test:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt tests/ --exclude Unix_Only
  tags:
    - windows

.test:linux:
  extends: .test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt tests/ --exclude Windows_Only
  tags:
    - linux

.test:macos:
  extends: .test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt tests/ --exclude Windows_Only --exclude Unix_Only
  tags:
    - macos
```

---

### Example 5: Scheduled Tests

Run regression tests nightly:

```yaml
stages:
  - test

nightly:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt tests/ --include Regression --verbose
  only:
    - schedules
  tags:
    - linux

# Define schedule in GitLab UI or use:
# Rules syntax for GitLab Premium/Ultimate
# only:
#   variables:
#     - $CI_PIPELINE_SOURCE == "schedule"
```

---

### Example 6: With Artifacts

Save test outputs as artifacts:

```yaml
stages:
  - test

test:
  stage: test
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
    - bbt --verbose -r . > test_output.log 2>&1 || true
  artifacts:
    when: on_failure
    paths:
      - test_output.log
    expire_in: 1 week
  tags:
    - linux
```

---

## Best Practices for GitLab CI

### 1. Use Caching Effectively

Cache Alire and bbt to speed up pipelines:

```yaml
variables:
  ALIRE_ROOT: "$CI_PROJECT_DIR/.alire"

cache:
  key: "$CI_COMMIT_REF_SLUG-alire"
  paths:
    - .alire/
  policy: pull-push
```

### 2. Use Tags for Test Filtering

Run different test sets in different jobs:

```yaml
smoke:
  script:
    - bbt tests/ --include Smoke

regression:
  script:
    - bbt tests/ --include Regression
  only:
    - main
```

### 3. Handle Failures Gracefully

```yaml
test:
  script:
    - bbt tests/ --stop-on-error || exit 0
    - echo "Tests completed"
  artifacts:
    when: on_failure
    paths:
      - output/
      - logs/
```

### 4. Use Environment Variables

```yaml
variables:
  BBT_VERSION: "latest"
  TEST_DIR: "tests"

test:
  script:
    - alr install bbt@$BBT_VERSION
    - bbt $TEST_DIR/
```

### 5. Parallel Jobs

Run different test types in parallel:

```yaml
stages:
  - test

unit tests:
  stage: test
  script:
    - bbt tests/unit/ --include Unit
  tags:
    - linux

integration tests:
  stage: test
  script:
    - bbt tests/integration/ --include Integration
  tags:
    - linux
```

---

## Advanced Configurations

### Custom Variables

```yaml
variables:
  # Custom Alire location
  ALIRE_ROOT: "$CI_PROJECT_DIR/alire"
  
  # bbt options
  BBT_OPTS: "--verbose --stop-on-error"
  
  # Test directories
  SMOKE_TESTS: "tests/smoke"
  REGRESSION_TESTS: "tests/regression"

test:
  script:
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - alr install bbt
    - bbt $SMOKE_TESTS/ $BBT_OPTS
```

---

### Needs and Dependencies

```yaml
stages:
  - setup
  - test

setup:
  stage: setup
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$HOME/.alire/bin:$PATH"
    - alr install bbt
  artifacts:
    paths:
      - .alire/

unit tests:
  stage: test
  script:
    - export PATH="$HOME/.alire/bin:$PATH"
    - bbt tests/unit/
  needs:
    - setup

integration tests:
  stage: test
  script:
    - export PATH="$HOME/.alire/bin:$PATH"
    - bbt tests/integration/
  needs:
    - setup
```

---

### Rules for Conditional Execution

```yaml
test:
  script:
    - bbt tests/
  rules:
    - if: $CI_COMMIT_BRANCH == "main"
      when: always
    - if: $CI_PIPELINE_SOURCE == "merge_request_event"
      when: always
    - when: never
```

---

### Include for Reusable Configurations

Create reusable configurations in separate files:

**.gitlab-ci/bbt-template.yml:**
```yaml
.bbt_template:
  variables:
    ALIRE_ROOT: "$CI_PROJECT_DIR/.alire"
  cache:
    key: alire
    paths:
      - .alire/
  before_script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - export PATH="$ALIRE_ROOT/bin:$PATH"
    - alr install bbt
```

**.gitlab-ci.yml:**
```yaml
include:
  - local: .gitlab-ci/bbt-template.yml

test:
  extends: .bbt_template
  script:
    - bbt -r .
```

---

## Troubleshooting GitLab CI

### Issue: Alire Installation Fails

**Solution:** Check the Alire installation:
```yaml
test:
  script:
    - curl -fsSL https://alire.ada.dev/download | bash
    - echo "Alire installed"
    - ls -la $HOME/.alire/bin/
    - export PATH="$HOME/.alire/bin:$PATH"
    - which alr
    - alr --version
    - alr install bbt
    - bbt --version
```

### Issue: bbt Not Found

**Solution:** Verify PATH is set correctly:
```yaml
test:
  script:
    - echo "PATH: $PATH"
    - which bbt || echo "bbt not found in PATH"
    - ls -la $HOME/.alire/bin/bbt* || echo "bbt not installed"
```

### Issue: Tests Fail Only in CI

**Causes:**
- Different environment
- Missing dependencies
- Different shell behavior

**Solutions:**
1. Add setup steps for dependencies
2. Use absolute paths in tests
3. Check the shell being used (GitLab uses `/bin/sh` by default)
4. Add debugging:
   ```yaml
   script:
     - env
     - which bash
     - which sh
     - ls -la
     - bbt --version
     - bbt tests/
   ```

### Issue: Cache Not Working

**Solution:**
1. Verify cache key is correct
2. Check cache path exists
3. Use `policy: pull-push` for better cache management
4. Add debugging:
   ```yaml
   script:
     - ls -la .alire/ || echo "Alire not cached"
   ```

---

## Useful GitLab CI Features

### Auto DevOps

GitLab can automatically run tests with Auto DevOps. Add to `.gitlab-ci.yml`:

```yaml
include:
  - template: Auto-DevOps.gitlab-ci.yml

variables:
  AUTO_DEVOPS_BUILD_IMAGE: "ubuntu:22.04"
  AUTO_DEVOPS_TEST_COMMAND: "bbt -r ."
```

### Manual Jobs

Allow manual triggering of jobs:

```yaml
regression:
  stage: test
  script:
    - bbt tests/ --include Regression
  when: manual
  allow_failure: false
```

### Timeouts

Set job timeout:

```yaml
test:
  script:
    - bbt tests/
  timeout: 30m
```

### Retries

Retry failed jobs:

```yaml
test:
  script:
    - bbt tests/
  retry:
    max: 2
    when:
      - runner_system_failure
      - stuck_or_timeout_failure
```

---

## GitLab CI vs GitHub Actions

| **Feature** | **GitLab CI** | **GitHub Actions** |
|-------------|---------------|---------------------|
| Configuration file | `.gitlab-ci.yml` | `.github/workflows/*.yml` |
| Caching | Built-in `cache:` | `actions/cache` action |
| Artifacts | Built-in `artifacts:` | `actions/upload-artifact` |
| Matrix builds | Manual with `parallel:` | Built-in `matrix:` |
| Scheduled runs | `only: - schedules` | `on: schedule:` |
| Manual triggers | `when: manual` | `workflow_dispatch` |

---

## See Also

- **GitHub Actions:** [github-actions.md](./github-actions.md)
- **CI/CD Overview:** [ci-cd.md](../ci-cd.md)
- **CLI Commands:** [commands.md](../commands.md)
