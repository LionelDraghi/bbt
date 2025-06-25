<!-- omit from toc -->
## Features : Index output

The index output produce a single file containing the result of all run tests, with links to the source scenario files. 

It is triggered by the `--index filename` option (short form `-i`). 

The content is equal to the normal output in verbose mode, whatever verbosity is specified on the command line.
Meaning that you can have `--quiet` and no standard output, the index file will still contains all the details.

In this test, a scenario is run in quiet / normal / verbose mode while writing an index, and check that the index file is always equal to the verbose output.

_Table of Contents:_
- [Background:](#background)
- [Scenario: Quiet mode run](#scenario-quiet-mode-run)
- [Scenario: Default mode run](#scenario-default-mode-run)
- [Scenario: Verbose mode run](#scenario-verbose-mode-run)

### Background:

- Given the file `OK_scen.md`
  ~~~md
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
  ~~~md
  # Scenario: sut version
  - When I run `./sut -v`
  - Then I get `v3.1`
  ~~~

- Given the file `verbose_output_OK.md`
  ~~~md
  
 # Document: [OK_scen.md](OK_scen.md)  
  ## Feature: Getting info  
   ### Background: [setup](OK_scen.md): 
   - OK : When I run `./sut --help`  
   - OK : Then the output contains `Return code:`  
   - [X] background [setup](OK_scen.md) pass  

   ### Scenario: [Getting the version](OK_scen.md): 
   - OK : When I run `./sut -v`  
   - OK : Then I get `sut version 1.0`  
   - [X] scenario   [Getting the version](OK_scen.md) pass  

   ### Background: [setup](OK_scen.md): 
   - OK : When I run `./sut --help`  
   - OK : Then the output contains `Return code:`  
   - [X] background [setup](OK_scen.md) pass  

   ### Scenario: [Getting help](OK_scen.md): 
   - OK : When I run `./sut -h`  
   - OK : Then output contains `Usage:`  
   - [X] scenario   [Getting help](OK_scen.md) pass  


## Summary : **Success**, 2 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 2     |
| Empty      | 0     |
| Not Run    | 0     |

  ~~~

- Given the file `verbose_output_NOK.md`
  ```md
  # Document: [NOK_scen.md](NOK_scen.md)  
   ### Scenario: [sut version](NOK_scen.md): 
   - OK : When I run `./sut -v`  
   - **NOK** : Then I get `v3.1` (NOK_scen.md:3:)  
  NOK_scen.md:3: Error: Output:  
  ~~~
  sut version 1.0
  ~~~
  not equal to expected:  
  ~~~
  v3.1
  ~~~
  
   - [ ] scenario   [sut version](NOK_scen.md) **fails**  


  ## Summary : **Fail**

  | Status     | Count |
  |------------|-------|
  | Failed     | 1     |
  | Successful | 0     |
  | Empty      | 0     |
  | Not Run    | 0     |
  ```

### Scenario: Quiet mode run

- When I successfully run `./bbt -q OK_scen.md  --index index_1.md`
- Then `index_1.md` is equal to file `verbose_output_OK.md`
  
- When I run `./bbt -q NOK_scen.md --index index_2.md`
- Then I get an error
- And `index_2.md` is equal to file `verbose_output_NOK.md`

### Scenario: Default mode run

- When I successfully run `./bbt    OK_scen.md  --index index_3.md`
- Then `index_3.md` is equal to file `verbose_output_OK.md`

- When I  run `./bbt    NOK_scen.md --index index_4.md`
- Then  I get an error
- And `index_4.md` is equal to file `verbose_output_NOK.md`

### Scenario: Verbose mode run

- When I successfully run `./bbt -v OK_scen.md  --index index_5.md`
- Then `index_5.md` is equal to file `verbose_output_OK.md`

- When I run `./bbt -v NOK_scen.md --index index_6.md`
- Then I get an error
- And `index_6.md` is equal to file `verbose_output_NOK.md`

