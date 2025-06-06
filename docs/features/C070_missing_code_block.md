<!-- omit from toc -->
## Feature : missing or erroneous code blocks

bbt try to put clear error messages related to missing code blocks, or to missing code block's marks.

_Table of Contents:_
- [Scenario: Code block missing at the end of the file](#scenario-code-block-missing-at-the-end-of-the-file)
- [Scenario: Code block missing while reaching next step](#scenario-code-block-missing-while-reaching-next-step)
- [Scenario: Code block missing while reaching next scenario](#scenario-code-block-missing-while-reaching-next-scenario)
- [Scenario: Closing code block mark missing](#scenario-closing-code-block-mark-missing)

## Scenario: Code block missing at the end of the file

- Given the file `code_block_missing_at_EOF.md`
~~~
# Scenario
- When I run `./sut --version`
- Then output contains "version"
- And output is
~~~

- When I run `./bbt -c code_block_missing_at_EOF.md`
- Then there is an error 
- And  the output contains 
```
code_block_missing_at_EOF.md:4: Error : Missing expected Code Block expected line 4
```

## Scenario: Code block missing while reaching next step

- Given the file `code_block_missing_in_step.md`
~~~
# Scenario
- When I run `./sut --version`
- Then output is
- Then there is no output
~~~

- When I run `./bbt -c code_block_missing_in_step.md`
- Then there is an error 
- And  the output contains 
```
code_block_missing_in_step.md:4: Error : missing expected Code Block expected line 3
```

## Scenario: Code block missing while reaching next scenario

- Given the file `code_block_missing_in_scenario.md`
~~~
# Scenario 1
- When I run `./sut --version`
- Then output is

# Scenario 2
~~~

- When I run `./bbt -c code_block_missing_in_scenario.md`
- Then there is an error 
- And  the output contains 
```
code_block_missing_in_scenario.md:5: Error : missing expected Code Block expected line 3
```

## Scenario: Closing code block mark missing 

- Given the file `code_block_not_closed.md`
~~~
# Scenario
- When I run `./sut --version`
- Then output is
```
Whatever
- And there is no output
~~~

- When I run `./bbt -c code_block_not_closed.md`
- Then there is an error 
- And  the output contains 
```
code_block_not_closed.md:6: Error : Code fenced block opened line 4, but not closed
```

- Given the file `code_block_not_closed2.md`
~~~
# Scenario
- When I run `./sut --version`

- Then output is
```
Whatever
~~~

- When I run `./bbt -c code_block_not_closed2.md`
- Then there is an error 
- And  the output contains 
```
code_block_not_closed2.md:6: Error : Code fenced block opened line 5, but not closed
```
