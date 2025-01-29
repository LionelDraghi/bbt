## Feature : missing or erroneous code blocks

bbt try to put clear error messages related to missing code blocks, or to missing code block's marks.

## Scenario: Code block missing at the end of the file

- Given the file `code_block_missing_at_EOF.md`
~~~
# Scenario
- When I run `./sut --help`
- Then output is
- Then there is no output
~~~

- When I run `./bbt -c code_block_missing_at_EOF.md`
- Then there is an error 
- And  the output contains 
```
code_block_missing_at_EOF.md:4: Error : missing expected Code Block
```

## Scenario: closing code block mark missing 

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
code_block_not_closed.md:6: Error : Code fenced block opened line 4, but not not closed
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
code_block_not_closed2.md:6: Error : Code fenced block opened line 5, but not not closed
```
