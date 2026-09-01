---
name: bbt-user
description: |
  Reference guide for LLM to generate valid bbt scenarios from natural language and run them.

  Start with the overview below, then dive into specific topics via linked files.
license: CC-BY-NC-SA-4.0
metadata:
  author: lionel-draghi
allowed-tools:
  - read
  - grep
  - bash
  - edit
  - write_file
  - ask_user_question
compatibility: Requires bbt to be installed for execution tasks; authoring tasks require no runtime dependency.
---

# bbt: Behavior-Driven Testing in Markdown

`bbt` lets you **write executable tests in natural-language Markdown** and **run them via CLI**.
This skill is split into two **independent but complementary** areas:

---

## 📝 **Authoring Scenarios** *(Writing Tests)*
For **creating** bbt scenarios from natural language, requirements, or user stories.

| **Topic** | **Description** | **File** |
|----------|----------------|----------|
| **Purpose & Overview** | What is bbt and its dual nature (format + tool) | [Purpose](#purpose) |
| **Syntax Reference** | Grammar, keywords, step structure, parameter formatting | [author/syntax.md](./author/syntax.md) |
| **Common Patterns** | Command execution, file operations, output verification, error handling | [author/patterns/](./author/patterns/) |
| **NL Transformation** | Convert requirements/README/user stories to bbt scenarios | [author/transformation/](./author/transformation/) |
| **Best Practices** | Maintainability, organization, checklists, workflow | [author/best-practices.md](./author/best-practices.md) |
| **Complete Examples** | Full scenarios, practical exercises, decision tree | [author/examples.md](./author/examples.md) |

---

## ▶️ **Running Tests** *(Execution & Debugging)*
For **installing, executing, debugging**, and integrating bbt into CI/CD.

| **Topic** | **Description** | **File** |
|----------|----------------|----------|
| **Installation** | Prerequisites, Alire, AppImage, compiling from source | [runner/installation/](./runner/installation/) |
| **CLI Commands** | `bbt`, `explain`, `help`, options, execution examples | [runner/commands.md](./runner/commands.md) |
| **Debugging** | Verbose mode, common errors, temporary files | [runner/debugging/](./runner/debugging/) |
| **CI/CD Integration** | GitHub Actions, GitLab CI examples | [runner/ci-cd/](./runner/ci-cd/) |

---

## 🚀 **Quick Start**

### 1. Install bbt
```bash
alr install bbt
bbt --version
```
*Details: [runner/installation/](./runner/installation/)*

### 2. Write a test (e.g., `test.md`)
```markdown
## Scenario: Check GCC installation

- When I run `gcc --version`
- Then output contains `gcc`
- And I get no error
```
*Syntax guide: [author/syntax.md](./author/syntax.md)*

### 3. Run it
```bash
bbt test.md
```
*CLI reference: [runner/commands.md](./runner/commands.md)*

---

## 🔍 **Need Help?**

| **Question** | **Answer** |
|--------------|------------|
| Grammar questions? | [author/syntax.md](./author/syntax.md) |
| CLI options? | [runner/commands.md](./runner/commands.md) |
| Debugging? | [runner/debugging/common-errors.md](./runner/debugging/common-errors.md) |
| CI/CD setup? | [runner/ci-cd/](./runner/ci-cd/) |
| Transformation from NL? | [author/transformation/](./author/transformation/) |
| Best practices? | [author/best-practices.md](./author/best-practices.md) |

---

## Purpose
`bbt` is both:
1. a format for embedding test scenarios in almost natural English within Markdown documentation;
2. a tool to run those tests.

*This dual nature is reflected in the skill organization: **author** for the format, **runner** for the tool.*
