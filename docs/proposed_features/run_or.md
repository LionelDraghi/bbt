## Feature : “when I run X or Y” test

Test the ability to duplicate a test thanks to "When I run X or Y" form

### Scenario : normal use case
  - When I run `sut -v` or `sut --version`
  - Then I should get no error
  - And the output is
~~~
~~~

### Scenario : the last command does not meet expectation (test should fail)
- Given the file `erroneous_or.md` 
~~~
# Scenario 1
  - When I run `sut -v` or `sut --version` or `sut --qddfg` 
  - Then I should get no error
  - And the output should contains `unknown option --qddfg`
~~~

- When I run `./bbt -c erroneous_or.md`
- Then I get an error
- And the output is
~~~
~~~
