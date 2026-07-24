# Natural Language Transformation

This directory contains guides for transforming various forms of natural language into valid bbt scenarios.

---

## 📚 Transformation Guides

| **Source** | **Description** | **File** |
|------------|----------------|----------|
| **Requirements** | Transform formal requirements to bbt | [from-requirements.md](./from-requirements.md) |
| **README Instructions** | Convert README documentation to bbt | [from-readme.md](./from-readme.md) |
| **User Stories** | Transform user stories to bbt scenarios | [from-user-stories.md](./from-user-stories.md) |

---

## General Transformation Process

When asked to "make a file bbt compatible" or "create a bbt test from X", follow this process:

1. **Analyze the source** for:
   - Key actions (commands to run)
   - Inputs (files, parameters)
   - Expected outputs (results, files, messages)
   - Preconditions (setup required)

2. **Identify the scenario structure**:
   - One scenario per **behavior** or **test case**
   - Group related scenarios under a **Feature** header

3. **Map to bbt syntax**:
   - Actions → `When I run`
   - Setup → `Given`
   - Verification → `Then`
   - Inputs → backticks or code blocks

4. **Add context**:
   - Use descriptive scenario names
   - Add tags for platform-specific tests
   - Include comments (ignored by bbt) for clarity

---

## Transformation Rules

### From Natural Language to bbt

When asked to "make a file runnable" or "make a file bbt compatible":

1. Look for sections with:
   - "Scenario" or "Example" in the title
   - Command line examples
   - Description of input and output

2. Transform the content using:
   - The grammar rules from [syntax.md](../syntax.md)
   - The patterns from [patterns/](../patterns/)

---

## Key Principles

1. **One scenario = one behavior**
   - Avoid scenarios with more than 5-6 steps
   - Split complex workflows into multiple scenarios

2. **Use the provided wording**
   - Reuse the original text as much as possible
   - Only modify to fit bbt syntax requirements

3. **Ensure validity**
   - All scenarios must be parseable and executable by bbt
   - Test with `bbt explain filename.md`

4. **Add value**
   - The resulting scenario should be **more precise** than the original text
   - It should be **executable** and **self-validating**

---

## Common Mistakes to Avoid

1. **Starting with `And` or `But`**
   - ❌ `- And given there is a file`
   - ✅ `- Given there is a file`

2. **Missing backticks**
   - ❌ `- When I run gcc --version`
   - ✅ `- When I run `gcc --version`

3. **Incorrect code block nesting**
   - ❌ Adding blank lines between step and code block
   - ✅ Code block immediately follows step

4. **Using bbt keywords in decorative text**
   - ❌ `This test Given a file...`
   - ✅ `This test requires a file...`

---

## Decision Tree for Scenario Generation

### Step 1: Identify Test Objective
- **Command execution** → Use `run` or `successfully run`
- **File operation** → Use file-related keywords
- **Output verification** → Use `get`, `contains`, `matches`
- **Error handling** → Use `get error`, `I get an error`

### Step 2: Determine Parameters
- **Short text (single line)** → Use inline backticks
- **Multiline content** → Use a code block after the step
- **File reference** → Use filename in backticks

### Step 3: Choose Step Type
- **Setup/Precondition** → `Given`
- **Action/Execution** → `When`
- **Verification/Result** → `Then`
- **Continuation** → `And` or `But`

### Step 4: Add Natural Decoration
- Keep it simple and focused
- Avoid bbt keywords in free text
- Use natural language that humans would understand

### Step 5: Validate Structure
- Scenarios must start with Given, When, or Then
- And/But can only follow another step
- Code blocks must immediately follow their step
- All parameters must be properly formatted
