### Scenario : a step with a missing code block followed by a new Scenario header shall not crash the analysis (bug present in bbt 0.4.2-dev, found on 2026-10-03 while migrating the ArchiCheck test suite)

- Given the new file `missing_code_block_test.md`
  ~~~
  # Feature: f

  ## Scenario: with a missing code block

  - When I run `./sut --version`
  - Then output is

  ## Scenario: following

  - When I run `./sut --version`
  ~~~

- When I run `./bbt explain missing_code_block_test.md`
- Then output do not contain `Exception`
- And output contains `Missing Code Block expected line`
- And output contains `following`

- When I run `./bbt --keep_going missing_code_block_test.md`
- Then I get an error
- And output do not contain `Exception`
