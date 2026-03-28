## Feature : “when I run X or Y” test

Test the ability to duplicate a test thanks to "When I run X or Y" form.

### Scenario : normal use case
- Given the new file `simple_or.md` 
~~~
# Scenario A
  - When I run `./sut -v` or `./sut --version` whatever comment here
~~~

- When I run `./bbt simple_or.md`

- Then I get no error
- And the output contains
~~~
   ### Scenario: [A 1/2](simple_or.md): 
   - OK : When I run `./sut -v` whatever comment here  
   - [X] scenario   [A 1/2](simple_or.md) pass  
   ### Scenario: [A 2/2](simple_or.md): 
   - OK : When I run `./sut --version` whatever comment here  
   - [X] scenario   [A 2/2](simple_or.md) pass  
~~~

### Scenario : the last command does not meet expectation (test should fail)
- Given the new file `erroneous_or.md` 
~~~
# Scenario B
  - When I run `./sut -v` or `./sut --version` or `./sut --qddfg` 
  - Then I should get no error
~~~

- When I run `./bbt -c erroneous_or.md`
- Then I get an error
- And the output contains
~~~
   - **NOK** : Then I should get no error (erroneous_or.md:3:)  
erroneous_or.md:3: Error: No error expected, but got one ( 1)  
   - [ ] scenario   [B 3/3](erroneous_or.md) **fails**  
~~~

## Feature: Syntactic and Semantic error related to "or" should be detected

### Scenario: Missing command before "or"

Before the first Code Span, "or" is considered as a comment and should be ignored.

- Given the new file `erroneous1.md` 
~~~
# Scenario C
  - When I run or `./sut --version` or `./sut -v` 
~~~

- When I run `./bbt -c erroneous1.md`
- Then I get no error
- And the output contains
~~~
   ### Scenario: [C 1/2](erroneous1.md): 
   - OK : When I run or `./sut --version`   
   - [X] scenario   [C 1/2](erroneous1.md) pass  

   ### Scenario: [C 2/2](erroneous1.md): 
   - OK : When I run or `./sut -v`   
   - [X] scenario   [C 2/2](erroneous1.md) pass  

## Summary : **Success**, 2 scenarios OK
~~~

### Scenario: Missing command after "or"
- Given the new file `erroneous2.md` 
~~~
# Scenario D
  - When I run `./sut -v` or `./sut --version` or  
~~~

- When I run `./bbt -c erroneous2.md`
- Then I get an error
- And the output contains
~~~
erroneous2.md:2: Error: Missing command after last 'or'
~~~

### Scenario: Two consecutive "or"
- Given the new file `erroneous3.md` 
~~~
# Scenario E
  - When I run `./sut -v` or or `./sut --version`
~~~

- When I run `./bbt -c erroneous3.md`
- Then I get no error
- And the output contains
~~~
erroneous3.md:2: Warning: Command missing between 'or' ?
~~~
