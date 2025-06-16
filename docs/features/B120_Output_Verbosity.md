<!-- omit from toc -->
## Features : Output verbosity is controlled through command line options

`bbt` has three verbosity levels : Quiet / Normal / Verbose 

Those levels are controlled with `--quiet` and `--verbose` options.

- Quiet mode  
  No output.  
  There is a single line summary.

- Normal mode  
  Result is output for each Feature and Scenario.  
  Backgrounds are not displayed.  
  There is a full summary.
  
- Verbose mode  
  Result is also output for each step.  
  Backgrounds are displayed as a scenario.  
  There is a full summary.  

Errors are always output in verbose mode, meaning that you have the info on the failing step even in Quiet mode. 

_Table of Contents:_
- [Background:](#background)
- [Scenario: Quiet mode run](#scenario-quiet-mode-run)
- [Scenario: Default mode run](#scenario-default-mode-run)
- [Scenario: Verbose mode run](#scenario-verbose-mode-run)
- [Scenario: Normal mode with an error](#scenario-normal-mode-with-an-error)
- [Scenario: Quiet mode with an error](#scenario-quiet-mode-with-an-error)

### Background:

- Given the file `OK_scen.md`
  ~~~
  # Feature : Getting info
  ## Background: setup
     - When I run `./sut --help`
     - Then the output contains `Return code:`
  ## Scenario : Getting the version
     - When I run `./sut -v`
     - Then I get `sut version 1.0`
  ## Scenario : Getting help
     - When I run `./sut -h`
     - Then output contains `Usage:`
  ~~~

- Given the file `NOK_scen.md`
  ~~~
  # Scenario: sut version
  - When I run `./sut -v`
  - Then I get `v3.1`
  ~~~

### Scenario: Quiet mode run

- When I successfully run `./bbt -q OK_scen.md`

- Then the output is `## Summary : **Success**, 2 scenarios OK`

### Scenario: Default mode run

- When I successfully run `./bbt OK_scen.md`

- Then the output is
  ~~~
  ## [OK_scen.md](OK_scen.md)  
    ### Feature: Getting info    
    - [X] scenario [Getting the version](OK_scen.md) pass  
    - [X] scenario [Getting help](OK_scen.md) pass  

  ## Summary : **Success**, 2 scenarios OK
  ~~~

### Scenario: Verbose mode run

- When I successfully run `./bbt -v OK_scen.md`

- Then the output is
  ~~~
  ## [OK_scen.md](OK_scen.md)  
    ### Feature: Getting info    
      OK  : When I run `./sut --help`    
      OK  : Then the output contains `Return code:`    
    - [X] background [setup](OK_scen.md) pass    
  
      OK  : When I run `./sut -v`  
      OK  : Then I get `sut version 1.0`  
    - [X] scenario [Getting the version](OK_scen.md) pass    
  
      OK  : When I run `./sut --help`    
      OK  : Then the output contains `Return code:`    
    - [X] background [setup](OK_scen.md) pass    
  
      OK  : When I run `./sut -h`    
      OK  : Then output contains `Usage:`    
    - [X] scenario [Getting help](OK_scen.md) pass    

  ## Summary : **Success**, 2 scenarios OK

  | Status     | Count |
  |------------|-------|
  | Failed     | 0     |
  | Successful | 2     |
  | Empty      | 0     |
  | Not Run    | 0     |
  ~~~

### Scenario: Normal mode with an error

- When I run `./bbt NOK_scen.md`

- Then there is an error
- And  the output is
  ```
  ## [NOK_scen.md](NOK_scen.md)  
  *** NOK : Then I get `v3.1` (NOK_scen.md:3:)  
  NOK_scen.md:3: Error: Output:  
  ~~~
  sut version 1.0
  ~~~
  not equal to expected:  
  ~~~
  v3.1
  ~~~
  
    - [ ] scenario   [sut version](NOK_scen.md) fails  

  ## Summary : **Fail**

  | Status     | Count |
  |------------|-------|
  | Failed     | 1     |
  | Successful | 0     |
  | Empty      | 0     |
  | Not Run    | 0     |
  ```

### Scenario: Quiet mode with an error

When there is an error, even if quiet mode is specified, we output all info.

- When I run `./bbt --quiet NOK_scen.md`

- Then there is an error
- And  the output is
  ```
  NOK_scen.md:3: Error: Output:  
  ~~~
  sut version 1.0
  ~~~
  not equal to expected:  
  ~~~
  v3.1
  ~~~
  
  ## Summary : **Fail**

  | Status     | Count |
  |------------|-------|
  | Failed     | 1     |
  | Successful | 0     |
  | Empty      | 0     |
  | Not Run    | 0     |
  ```
