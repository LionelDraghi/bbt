<!-- omit from toc -->
## Feature: on error, Keep going or stop

During debug, bbt helps the developer focusing on the first problem by just stopping the run.  
This is the default behavior.  
On the other hand, in a batch run, one usually wants to run all tests even if some fails.  
btt allows this second behavior through the `--keep_going` (or `-k`) option.  

The default behavior is to stop on the first error.
If there is a syntax error in the md file, following steps are ignored.
If there is an error when running 
a step, the result synthesis will display some "not run" tests.

But if `-k` is specified, the run goes to the end, all scenarios and steps are run, and the synthesis is displayed with fail/pass/empty tests, but no "not run" tests.

In both case, the total is the same.

Here we run a test with 3 scenarios.  
The second fails.  
When run with `-k`, the run will end with 1 fail test and 2 success.  
When run without, the run ends with 1 fail, 1 success and 1 not run.

_Table of Contents:_
- [Scenario: with `-k`](#scenario-with--k)
- [Scenario: without `-k`](#scenario-without--k)

### Background: setup
- Given the file `feature1.md`
  ```
  ### Scenario 1
  - When I run `./sut -h`
  - Then I get no error
  
  ### Scenario 2
  - When I `./sut -h`   
    --> ** bbt syntax error here (the verb is missing)**
  
  - Then I get no error 
    If run, should this line failed? Not obvious, because actually nothing was run.
    --> the current implementation, by far the easiest, doesn't raise an error.
    Not need to change, an error is already raised on the previous step.

  ### Scenario 3
  - When I run `./sut -v`
  - Then I get no error
  ```

# Scenario: with `-k`
- When I run `./bbt -k feature1.md`
- then output contains
  ```
  # Document: [feature1.md](feature1.md)    
   ### Scenario: [1](feature1.md):   
   - OK : When I run `./sut -h`    
   - OK : Then I get no error    
   - [X] scenario   [1](feature1.md) pass
  ```  

- and output contains
  ~~~
    ### Scenario: [2](feature1.md):
  feature1.md:6: Error: Unrecognized step "When I `./sut -h`   "
  ~~~  

- and output contains
  ```
  - [ ] scenario   [2](feature1.md) **fails**
  ```

- and output contains
  ```
   ### Scenario: [3](feature1.md):   
   - OK : When I run `./sut -v`    
   - OK : Then I get no error    
   - [X] scenario   [3](feature1.md) pass 
  
  ## Summary : **Fail**

  | Status     | Count |
  |------------|-------|
  | Failed     | 1     |
  | Successful | 2     |
  | Empty      | 0     |
  | Not Run    | 0     |
  ```

# Scenario: without `-k`
- When I run `./bbt feature1.md`
- then output is
  ```
  # Document: [feature1.md](feature1.md)    
    ### Scenario: [1](feature1.md):   
    - OK : When I run `./sut -h`    
    - OK : Then I get no error    
    - [X] scenario   [1](feature1.md) pass    
    ### Scenario: [2](feature1.md):   
  feature1.md:6: Error: Unrecognized step "When I `./sut -h`   "  
    - [ ] scenario   [2](feature1.md) **fails**    
  
  ## Summary : **Fail**  
  
  | Status     | Count |  
  |------------|-------|  
  | Failed     | 1     |  
  | Successful | 1     |  
  | Empty      | 0     |  
  | Not Run    | 1     |  
  ```
