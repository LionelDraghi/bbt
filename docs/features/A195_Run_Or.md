## Feature : “when I run X or Y” test

Test the ability to duplicate a test thanks to "When I run X or Y" form.

### Scenario : normal use case
- Given the new file `simple_or.md` 
~~~
# Scenario 1
  - When I run `./sut -v` or `./sut --version`
~~~

- When I run `./bbt simple_or.md`

- Then I get no error
- And the output is
~~~
~~~

### Scenario : the last command does not meet expectation (test should fail)
#- Given the new file `erroneous_or.md` 
~~~
# Scenario 1
  - When I run `./sut -v` or `./sut --version` or `sut --qddfg` 
  - Then I should get no error
  - And the output should contains `unknown option --qddfg`
~~~

+- When I run `./bbt -c erroneous_or.md`
+- Then I get an error
+- And the output is
~~~
~~~

## Feature: Syntactic and Semantic error related to "or" should be detected

### Scenario: Missing command before "or"
+- Given the new file `erroneous1.md` 
~~~
# Scenario 1
  - When I run or `./sut --version` or `./sut -v` 
~~~

+- When I run `./bbt -c erroneous1.md`
+- Then I get an error
+- And the output is
~~~
~~~

### Scenario: Missing command after "or"
+- Given the new file `erroneous2.md` 
~~~
# Scenario 1
  - When I run `./sut -v` or `./sut --version` or  
~~~

+- When I run `./bbt -c erroneous2.md`
+- Then I get an error
+- And the output is
~~~
~~~

### Scenario: Two consecutive "or"
+- Given the new file `erroneous3.md` 
~~~
# Scenario 1
  - When I run `./sut -v` or or `./sut --version`
~~~

+- When I run `./bbt -c erroneous3.md`
+- Then I get an error
+- And the output is
~~~
~~~
