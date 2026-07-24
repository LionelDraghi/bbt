# Common Scenario Patterns

This directory contains categorized patterns for writing bbt scenarios. Each file focuses on a specific type of operation.

---

## 📚 Pattern Categories

| **Category** | **Description** | **File** |
|--------------|----------------|----------|
| **Command Execution** | Running commands, checking exit codes, expected output | [commands.md](./commands.md) |
| **File Operations** | Creating, reading, comparing files | [files.md](./files.md) |
| **Output Verification** | Matching output (exact, partial, regex, unordered) | [outputs.md](./outputs.md) |
| **Error Handling** | Detecting errors, checking error messages | [errors.md](./errors.md) |

---

## Quick Reference

### Most Common Patterns

| **Intent** | **Pattern** | **File** |
|------------|-------------|----------|
| Run a command | `- When I run \`command\`` | [commands.md](./commands.md) |
| Run a command (must succeed) | `- When I successfully run \`command\`` | [commands.md](./commands.md) |
| Create a file | `- Given the file \`name\` containing \`content\`` | [files.md](./files.md) |
| Check file exists | `- Given there is a file \`name\`` | [files.md](./files.md) |
| Verify output | `- Then output contains \`text\`` | [outputs.md](./outputs.md) |
| Verify with regex | `- Then output matches \`pattern\`` | [outputs.md](./outputs.md) |
| Check for error | `- Then I get error` | [errors.md](./errors.md) |

---

## When to Use Which Pattern

1. **Testing a command**: Use **Command Execution** patterns
2. **Manipulating files**: Use **File Operations** patterns
3. **Validating output**: Use **Output Verification** patterns
4. **Testing error cases**: Use **Error Handling** patterns

---

## Complete Example Combining Patterns

```markdown
## Scenario: Compile and run a program

- Given the file `main.c` containing
```c
#include <stdio.h>
int main() {
    printf("Hello\n");
    return 0;
}
```
- When I successfully run `gcc main.c -o program`
- Then there is a file `program`
- When I run `./program`
- Then output contains `Hello`
- And I get no error
```

*This example combines:*
- **File Operations**: Creating `main.c`
- **Command Execution**: Compiling with `gcc`, running `./program`
- **Output Verification**: Checking for `Hello`
- **Error Handling**: Ensuring no error
