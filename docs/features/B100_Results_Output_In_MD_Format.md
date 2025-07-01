<!-- omit from toc -->
## Features : run results are output in Markdown format

The output of bbt is using Markdown.  

Scenario, Feature and all Gherkin reserved words are Markdown headers.  

Each step is a prefixed by OK or NOK.  

For each scenario, there is a summary line, with a bullet list item, using the [GFM Task list items extension](https://github.github.com/gfm/#task-list-items-extension-). If the test pass, the box is checked `[X]`, empty otherwise `[ ]`.    

In error messages, outputs are put in code blocks to be easily identified (Refer to [Issue #9](https://github.com/LionelDraghi/bbt/issues/9)).

_Table of Contents:_
- [Background:](#background)
  - [Scenario: default mode run](#scenario-default-mode-run)
  - [Scenario: verbose mode run](#scenario-verbose-mode-run)
  - [Scenario: run with an error](#scenario-run-with-an-error)

## Background:

- Given the file `OK_scen.md`
  ~~~
  # Scenario
  - When I run `./sut -v`
  - Then I get `sut version 1.0`
  ~~~

- Given the file `NOK_scen.md`
  ~~~
  # Scenario
  - When I run `./sut -v`
  - Then I get `v3.1`
  ~~~


### Scenario: default mode run

- When I run `./bbt -c --yes OK_scen.md`

- Then the output is
  ~~~
  # Document: [OK_scen.md](OK_scen.md)    
  ### Scenario: [](OK_scen.md):   
  - OK : When I run `./sut -v`  
  - OK : Then I get `sut version 1.0`  
  - [X] scenario   [](OK_scen.md) pass    
  
  ## Summary : **Success**, 1 scenarios OK
  ~~~

### Scenario: verbose mode run

- When I run `./bbt -v -c --yes OK_scen.md`

- Then the output contains
  ~~~
  # Document: [OK_scen.md](OK_scen.md)    
  ### Scenario: [](OK_scen.md):   
  -  OK : When I run `./sut -v`    
  -  OK : Then I get `sut version 1.0`    
  - [X] scenario   [](OK_scen.md) pass    
  
  
  ## Summary : **Success**, 1 scenarios OK  
  
  | Status     | Count |  
  |------------|-------|  
  | Failed     | 0     |  
  | Successful | 1     |  
  | Empty      | 0     |  
  | Not Run    | 0     |  
  ~~~

### Scenario: run with an error

- When I run `./bbt -c --yes NOK_scen.md`

- Then the output contains
  ```
  NOK_scen.md:3: Error: Output:    
  ~~~  
  sut version 1.0  
  ~~~  
  not equal to expected:    
  ~~~  
  v3.1  
  ~~~    
  ```

- And the output contains
  ```
    - [ ] scenario [](NOK_scen.md) **fails**
  ```

