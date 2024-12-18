<!-- omit from toc -->
## Feature: on error, Keep going or stop

During debug, bbt helps the developer focusing on the first problem by just stopping the run.  
This is the default behavior.  
On the other hand, in a batch run, one usually wants to run all tests even if some fails.  
btt allows this second behavior through the `--keep_going` (or `-k`) option.  

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
- Failed     =  1  
- Successful =  2  
- Empty      =  0  
- Not run    =  0  
```

# Scenario: without `-k`
- When I run `./bbt feature1.md`
- then output contains
```
- Failed     =  1
- Successful =  1
- Empty      =  0
- Not run    =  1
```
