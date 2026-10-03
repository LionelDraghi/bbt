### Scenario : comparing an empty actual output with a non empty expected content shall fail normally, not raise an exception, and the scenario shall be counted as failed (bug present in bbt 0.4.2-dev, found on 2026-10-03 while migrating the ArchiCheck test suite)

- Given the new file `empty_output_test.md`
  ~~~
  # Scenario
  - When I run `./sut create tmp.txt`
  - Then output is
  ```
  some expected line
  ```
  ~~~

- When I run `./bbt --keep_going empty_output_test.md`
- Then I get an error
- And output do not contain `Exception`
- And output contains `Output not equal to expected`
- And output contains `**fails**`
