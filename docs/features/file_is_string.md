## Feature: "file is" followed by a code span (a string)

### Scenario: test on a single line file

  - When I run `uut create config.ini`
  - When I run `uut append mode=silent config.ini`
  - Then file `config.ini` is equal to `mode=silent`
  - Then file `config.ini` is          `mode=silent`
  (both form are identical, "equal" and "to" are not keywords)

### Scenario: adding a second line to the file, so the same test should fail

- Given the new `tmp.md` file
```
# scenario:
- Then file `config.ini` is equal to `mode=silent`
```

- When I run `uut create config.ini`
- When I run `uut append mode=silent config.ini`
- When I run `uut append recurse=false config.ini`

- When I run `bbt tmp.md`
- then I get an error
 