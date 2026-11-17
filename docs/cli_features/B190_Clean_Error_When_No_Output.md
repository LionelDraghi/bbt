<!-- omit from toc -->
## Feature: clean error when checking the output of a command that did not run

When the command of a run step cannot be spawned, typically because the executable does not exist, and the run continues after the failed step (`--keep_going` or `-k`), a following output checking step (`Then I get`, `Then output is`, ...) finds no output file: the command never ran.  
bbt then reports a clean error on the output file, instead of raising an unhandled NAME_ERROR.

_Table of Contents:_
- [Scenario: check the output of a command that does not exist](#scenario-check-the-output-of-a-command-that-does-not-exist)

### Scenario: check the output of a command that does not exist

- Given the new file `no_cmd_test.md`
  ~~~
  # Scenario
  - When I run `./no_such_command`
  - Then I get `whatever`
  ~~~

- When I run `./bbt -k no_cmd_test.md`
- Then output contains
  ~~~
  - **NOK** : When I run `./no_such_command` (no_cmd_test.md:2:)
  no_cmd_test.md:2: Error: ./no_such_command not found
  - **NOK** : Then I get `whatever` (no_cmd_test.md:3:)
  no_cmd_test.md:3: Error: cannot open no_cmd_test.md.out: the command did not run
  - [ ] scenario   [](no_cmd_test.md) **fails**
  ~~~
- and output contains
  ```
  ## Summary : **Fail**
  ```
- Then I get an error
