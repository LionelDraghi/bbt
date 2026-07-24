# Debugging bbt Tests

This directory contains guides for debugging bbt scenarios and resolving common issues.

---

## 📚 Debugging Guides

| **Topic** | **Description** | **File** |
|-----------|----------------|----------|
| **Verbose Mode** | Using `--verbose` for detailed output | [verbose-mode.md](./verbose-mode.md) |
| **Common Errors** | Frequent errors and their solutions | [common-errors.md](./common-errors.md) |
| **Temporary Files** | Inspecting temp files with `--no-cleanup` | [temporary-files.md](./temporary-files.md) |

---

## Debugging Workflow

Follow this workflow when a test fails:

1. **Run with verbose mode** to see detailed execution:
   ```bash
   bbt --verbose my_test.md
   ```

2. **Check if bbt recognizes your scenarios** (dry run):
   ```bash
   bbt explain my_test.md
   ```
   - If steps are missing, see [common-errors.md](./common-errors.md)

3. **Keep temporary files** for inspection:
   ```bash
   bbt --verbose --no-cleanup my_test.md
   ```
   Then examine the temp directory (see [temporary-files.md](./temporary-files.md))

4. **Manually test commands** from your scenario to isolate the issue

5. **Check for syntax errors** in your scenario file

---

## Quick Reference

| **Symptom** | **Solution** | **File** |
|-------------|--------------|----------|
| Steps not recognized | Check syntax and formatting | [common-errors.md](./common-errors.md) |
| Command not found | Verify command is in PATH | [verbose-mode.md](./verbose-mode.md) |
| Output mismatch | Use `--verbose` to see actual vs expected | [verbose-mode.md](./verbose-mode.md) |
| Need to inspect files | Use `--no-cleanup` | [temporary-files.md](./temporary-files.md) |
| Test passes locally but not in CI | Check environment differences | [common-errors.md](./common-errors.md) |

---

## Common Issues

### "bbt does not identify my scenarios/steps"

See [common-errors.md](./common-errors.md) for:
- Missing backticks
- Incorrect code block markers
- Wrong list markers
- Invalid step keywords

### "Command not found" errors

1. Verify the command exists and is in your PATH
2. Check command spelling in backticks
3. Use absolute paths if needed: `` `C:\path\to\command` ``

### Output matching failures

1. Use `--verbose` to see actual output
2. Check for case sensitivity (use `--human_match`)
3. Use `contains` instead of exact matches
4. Use regex for flexible matching

### File-related issues

1. Verify file paths are relative to the `.md` file
2. Check file permissions
3. Use `--no-cleanup` to inspect created files

---

## Pro Tips

1. **Always start with `bbt explain`** when writing new scenarios
2. **Use `--verbose` liberally** during development
3. **Keep temporary files** until you understand why a test fails
4. **Test commands manually** before adding to scenarios
5. **Check file paths** - they are relative to the `.md` file, not the current directory

---

## See Also

- **CLI Commands:** [commands.md](./commands.md)
- **Syntax Reference:** [author/syntax.md](../author/syntax.md)
- **Patterns:** [author/patterns/](../author/patterns/)
