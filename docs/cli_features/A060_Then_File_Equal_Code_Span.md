<!-- omit from toc -->
## Feature: "file is" followed by a code span (a string)

In the rare case where the expected file content is a single line, it can for convenience and readability be given on the same line between backticks, and not in successive lines in a code fence. 

_Table of Contents:_
- [Scenario: test on a single line file](#scenario-test-on-a-single-line-file)
- [Scenario: adding a second line to the file, so the same test should fail](#scenario-adding-a-second-line-to-the-file-so-the-same-test-should-fail)

### Scenario: test on a single line file

  - When I run `./sut create config.ini`
  - When I run `./sut append mode=silent config.ini`
  - Then file `config.ini` is equal to `mode=silent`
  - Then file `config.ini` is          `mode=silent`
  (both form are identical, "equal" and "to" are not keywords)

### Scenario: adding a second line to the file, so the same test should fail

- Given the new `tmp.md` file
```
# scenario:
- Then file `config.ini` is equal to `mode=silent`
```

- When I run `./sut create config.ini`
- When I run `./sut append mode=silent config.ini`
- When I run `./sut append recurse=false config.ini`

- When I run `./bbt tmp.md`
- then I get an error
 
