# Transforming README Instructions to bbt

Convert README documentation and instructions into executable bbt scenarios.

---

## README Transformation Guide

README files typically contain:
- Installation instructions
- Usage examples
- Configuration steps
- Expected outputs

Each of these can be transformed into testable bbt scenarios.

---

## Examples

### Example 1: Installation Instructions

**README:**
```markdown
## Installation

1. Run `npm install` to install dependencies
2. Run `npm build` to build the project
```

**Transformation:**
```markdown
## Scenario: Project can be installed and built

- When I successfully run `npm install`
- And I successfully run `npm build`
- Then there is a file `dist/bundle.js`
- And I get no error
```

---

### Example 2: Usage Example

**README:**
~~~markdown
## Usage

To process a file:
```bash
processor input.txt output.txt
```

This will read `input.txt`, process its contents, and write the result to `output.txt`.
~~~

**Transformation:**
```markdown
## Scenario: Processor handles valid input

- Given the file `input.txt` containing `raw data`
- When I successfully run `processor input.txt output.txt`
- Then there is a file `output.txt`
- And file `output.txt` contains `processed data`
- And I get no error
```

---

### Example 3: Configuration Instructions

**README:**
~~~markdown
## Configuration

Create a `config.json` file:
```json
{
  "setting": "value"
}
```

Then run `app --config config.json` to start the application.
~~~

**Transformation:**
~~~markdown
## Scenario: Application starts with valid configuration

- Given the file `config.json` containing
```json
{
  "setting": "value"
}
```
- When I successfully run `app --config config.json`
- Then output contains `Application started`
- And I get no error
~~~

---

### Example 4: Command-Line Options

**README:**
```markdown
## Command-Line Options

- `-v, --verbose`: Enable verbose output
- `-o, --output <file>`: Specify output file
- `--help`: Display help message
```

**Transformation:**
```markdown
## Scenario: Help message displays correctly

- When I run `app --help`
- Then output contains `Usage:`
- And output contains `--verbose`
- And output contains `--output`
- And I get no error

## Scenario: Verbose mode produces more output

- When I run `app -v`
- Then output contains `DEBUG`
- And output contains `Info`
```

---

### Example 5: Expected Output

**README:**
~~~markdown
## Output

The program will output:
```
Processing...
Done!
```
~~~

**Transformation:**
~~~markdown
## Scenario: Program produces expected output

- When I run `program`
- Then output contains 
```
Processing...
Done!
```
- And I get no error
~~~

For **exact** output matching:
~~~markdown
## Scenario: Exact output verification

- When I run `program`
- Then I get
```
Processing...
Done!
```
- And I get no error
~~~

---

### Example 6: Multi-Step Workflow

**README:**
```markdown
## Build Process

1. Create a build directory: `mkdir build`
2. Navigate to it: `cd build`
3. Run cmake: `cmake ..`
4. Build the project: `cmake --build .`
```

**Transformation:**
```markdown
## Scenario: Complete build process

- When I successfully run `mkdir build`
- And I successfully run `cd build`
- And I successfully run `cmake ..`
- And I successfully run `cmake --build .`
- Then there is a file `build/program`
```

---

## Transformation Patterns

| **README Element** | **bbt Pattern** | **Example** |
|--------------------|-----------------|-------------|
| **Command to run** | `When I run \`command\`` | `When I run npm install` |
| **Successful execution** | `When I successfully run \`command\`` | `When I successfully run make` |
| **File creation** | `Given the file \`name\` containing` | `Given the file config.json containing` |
| **Expected output** | `Then output contains \`text\`` | `Then output contains Success` |
| **File should exist** | `Then there is a file \`name\`` | `Then there is a file output.txt` |
| **Sequential steps** | `When...`<br>`And I run...` | `When I run cmd1`<br>`And I run cmd2` |

---

## Tips for README Transformation

1. **Identify executable steps**: Look for commands, code blocks, numbered lists
2. **Identify inputs**: File contents, configuration, parameters
3. **Identify expected results**: Output, created files, success messages
4. **Create one scenario per workflow**: Group related steps into a single scenario
5. **Add assertions**: For each step, add a verification (`Then...`)
6. **Use tags**: Add tags like `[Documentation]`, `[README]` to track source

---

## Handling Different README Formats

### Markdown READMEs
Most common format. Transform code blocks and commands directly.

### Text READMEs
Convert numbered/bulleted lists into bbt steps.

### AsciiDoc/READMEs
Similar to Markdown. Focus on code blocks and commands.

---

## Complete Example: Full README Transformation

**Original README.md:**
~~~markdown
# My Application

## Installation

Install dependencies with:
```bash
npm install
```

## Usage

Process a file:
```bash
node app.js input.txt output.txt
```

This will create `output.txt` with the processed contents of `input.txt`.

## Testing

Run tests with:
```bash
npm test
```

All tests should pass.
~~~

**Transformed bbt Scenarios:**
~~~markdown
# Scenario: My Application

## Background: Installation succeeds

- Given I successfully run `npm install`

## Scenario: File processing works

- Given the file `input.txt` containing `test data`
- When I successfully run `node app.js input.txt output.txt`
- Then there is a file `output.txt`
- And file `output.txt` contains `processed: test data`

## Scenario: All tests pass

- When I successfully run `npm test`
- Then output contains `Test Suites: 1 passed`
~~~

---

## Best Practices

1. **Start with the most critical workflows** from the README
2. **Add error cases**: Test what happens when instructions are followed incorrectly
3. **Keep scenarios focused**: One README section = one or more scenarios
4. **Preserve the original wording** in scenario names and descriptions
5. **Add context**: Include the README section as a comment in the scenario
