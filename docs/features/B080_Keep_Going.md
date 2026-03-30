<!-- omit from toc -->
## Feature: on error, Keep going or stop

During debug, bbt helps the developer focusing on the first problem by just stopping the run.  
This is the default behavior.  
On the other hand, in a batch run, one usually wants to run all tests even if some fails.  
btt allows this second behavior through the `--keep_going` (or `-k`) option.  

The default behavior is to stop on the first error.
If there is a syntax error in the md file, following steps are ignored.
If there is an error when running a step, the result synthesis will display some "not run" tests.

But if `-k` is specified, the run goes to the end, all scenarios and steps are run, and the synthesis is displayed with fail/pass/empty tests, but no "not run" tests.

Here we run a two files with 3 scenarios.  

In the first file, the second scenario contain a syntax error.  
When run with `-k`, the run will end with 1 fail test and 2 success.  
When run without, the analysis is interrupted on the syntax error and there is no run and no synthesis.

In the second file, the second scenario contain no syntax error but one of the assertion is not satisfied.  
When run with `-k`, the run will end with 1 fail test and 2 success.  
When run without, the run ends with 1 fail, 1 success and 1 not run.

The effect of -k on `explain | ex` is also tested.

_Table of Contents:_
- [Scenario: scenario\_with\_syntax\_error run without `-k`](#scenario-scenario_with_syntax_error-run-without--k)
- [Scenario: scenario\_with\_syntax\_error run with `-k`](#scenario-scenario_with_syntax_error-run-with--k)
- [Scenario: scenario\_with\_assertion\_error run without `-k`](#scenario-scenario_with_assertion_error-run-without--k)
- [Scenario: scenario\_with\_assertion\_error run with `-k`](#scenario-scenario_with_assertion_error-run-with--k)
- [Scenario: explain scenario\_with\_syntax\_error without `-k`](#scenario-explain-scenario_with_syntax_error-without--k)
- [Scenario: explain scenario\_with\_syntax\_error with `-k`](#scenario-explain-scenario_with_syntax_error-with--k)

### Background: setup
- Given the file `scenario_with_syntax_error.md`
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

- Given the file `scenario_with_assertion_error.md`
  ```
  ### Scenario 1
  - When I run `./sut -h`
  - Then I get no error
  
  ### Scenario 2
  - When I run `./sut -h`   
  
  - Then I get an error 
    --> This is going to fail

  ### Scenario 3
  - When I run `./sut -v`
  - Then I get no error
  ```

# Scenario: scenario_with_syntax_error run without `-k`
- When I run `./bbt scenario_with_syntax_error.md`
- then output is
  ```
  scenario_with_syntax_error.md:6: Error: Unrecognized step "When I `./sut -h`   "  
  ```
  No Summary, run is interrupted on the first error during analysis.

# Scenario: scenario_with_syntax_error run with `-k`
- When I run `./bbt -k scenario_with_syntax_error.md`
- then output contains
  ```
   - [X] scenario   [1](scenario_with_syntax_error.md) pass
  ```  
- and output contains
  ~~~
   - [ ] scenario   [2](scenario_with_syntax_error.md) **fails**  
  ~~~
- and output contains
  ```
   - [X] scenario   [3](scenario_with_syntax_error.md) pass  
  ```
- and output contains
  ~~~
  scenario_with_syntax_error.md:6: Error: Unrecognized step "When I `./sut -h`   "
  ~~~  
- and output contains
  ```
scenario_with_syntax_error.md:6: Warning: Skipping step with syntax error
  ```
- and output contains
  ```
  ## Summary : **Fail**

  | Status     | Count |
  |------------|-------|
  | Failed     | 1     |
  | Successful | 2     |
  | Empty      | 0     |
  | Not Run    | 0     |
  ```

# Scenario: scenario_with_assertion_error run without `-k`
- When I run `./bbt scenario_with_assertion_error.md`
- then output is
  ```
# Document: [scenario_with_assertion_error.md](scenario_with_assertion_error.md)  
   ### Scenario: [1](scenario_with_assertion_error.md): 
   - OK : When I run `./sut -h`  
   - OK : Then I get no error  
   - [X] scenario   [1](scenario_with_assertion_error.md) pass  
   ### Scenario: [2](scenario_with_assertion_error.md): 
   - OK : When I run `./sut -h`     
   - **NOK** : Then I get an error  (scenario_with_assertion_error.md:8:)  
scenario_with_assertion_error.md:8: Error: Expected error code, got no error  
   - [ ] scenario   [2](scenario_with_assertion_error.md) **fails**  

## Summary : **Fail**

| Status     | Count |
|------------|-------|
| Failed     | 1     |
| Successful | 1     |
| Empty      | 0     |
| Not Run    | 1     |
  ```

# Scenario: scenario_with_assertion_error run with `-k`
- When I run `./bbt -k scenario_with_assertion_error.md`
- then output is
  ```
# Document: [scenario_with_assertion_error.md](scenario_with_assertion_error.md)  
   ### Scenario: [1](scenario_with_assertion_error.md): 
   - OK : When I run `./sut -h`  
   - OK : Then I get no error  
   - [X] scenario   [1](scenario_with_assertion_error.md) pass  
   ### Scenario: [2](scenario_with_assertion_error.md): 
   - OK : When I run `./sut -h`     
   - **NOK** : Then I get an error  (scenario_with_assertion_error.md:8:)  
scenario_with_assertion_error.md:8: Error: Expected error code, got no error  
   - [ ] scenario   [2](scenario_with_assertion_error.md) **fails**  
   ### Scenario: [3](scenario_with_assertion_error.md): 
   - OK : When I run `./sut -v`  
   - OK : Then I get no error  
   - [X] scenario   [3](scenario_with_assertion_error.md) pass  

## Summary : **Fail**

| Status     | Count |
|------------|-------|
| Failed     | 1     |
| Successful | 2     |
| Empty      | 0     |
| Not Run    | 0     |
  ```

# Scenario: explain scenario_with_syntax_error without `-k`
- When I run `./bbt ex scenario_with_syntax_error.md`
- then output is
  ```
  scenario_with_syntax_error.md:6: Error: Unrecognized step "When I `./sut -h`   "

  Document `scenario_with_syntax_error.md`

  1: Scenario `1`
  2: - Run command `./sut -h` 
  3: - Check that command does not return an error

  5: Scenario `2`
  6: - Run *** Unrecognized step **** 
  ```

# Scenario: explain scenario_with_syntax_error with `-k`
- When I run `./bbt -k ex scenario_with_syntax_error.md`
- then output is
  ```
  scenario_with_syntax_error.md:6: Error: Unrecognized step "When I `./sut -h`   "

  Document `scenario_with_syntax_error.md`

  1: Scenario `1`
  2: - Run command `./sut -h` 
  3: - Check that command does not return an error

  5: Scenario `2`
  6: - Run *** Unrecognized step **** 
  9: - Check that command does not return an error

  14: Scenario `3`
  15: - Run command `./sut -v` 
  16: - Check that command does not return an error
  ```
