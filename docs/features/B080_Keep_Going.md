<!-- omit from toc -->
## Feature: on error, Keep going or stop

`bbt`default behavior is to stop when there is an error.
But in some circumstances, we want a complete overview and want to run all the tests.
This is what the `-k`/ `--keep_going`option is for.

Here we run a test with 3 scenarios.  
The second fails.  
With `-k` the run will end with 1 fail test and 2 success.
Without, the run ends with 1 fail, 1 success and 1 not run.

- [Scenario: with `-k`](#scenario-with--k)
- [Scenario: without `-k`](#scenario-without--k)

### Background:
- Given the file `feature1.md`
```
### Scenario 1
- When I run `./sut -h`
- Then I get no error

### Scenario 2
- When I `./sut -h` --> ** bbt syntax error here **

### Scenario 3
- When I run `./sut -v`
- Then I get no error
```

# Scenario: with `-k`
- When I run `./bbt -k feature1.md`
- then output contains
```
- Failed    =  1  
- Successful =  2  
- Empty      =  0  
- Not run    =  0  
```

# Scenario: without `-k`
- When I run `./bbt feature1.md`
- then output contains
```
- Failed    =  1
- Successful =  1
- Empty      =  0
- Not run    =  1
```