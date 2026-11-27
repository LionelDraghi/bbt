<!-- omit from toc -->
## Features : run results are output in Markdown format

The output of bbt is using Markdown.  

Scenario, Feature and all Gherkin reserved words are Markdown headers.  

Each step is a prefixed by OK or NOK.  

For each scenario, there is a summary line, with a bullet list item, using the [GFM Task list items extension](https://github.github.com/gfm/#task-list-items-extension-). If the test pass, the box is checked `[X]`, empty otherwise `[ ]`.    

In error messages, outputs are put in code blocks to be easily identified (Refer to [Issue #9](https://github.com/LionelDraghi/bbt/issues/9)).

When a run fails, its summary names the failed scenarios again, between the summary title and the results table: the run above is long, and they would otherwise have to be looked for by scrolling back up.

_Table of Contents:_
- [Background:](#background)
  - [Scenario: default mode run](#scenario-default-mode-run)
  - [Scenario: verbose mode run](#scenario-verbose-mode-run)
  - [Scenario: failing run names the failed scenarios](#scenario-failing-run-names-the-failed-scenarios)

## Background:

- Given the new file `OK_scen.md`
  ~~~
  # Scenario
  - When I run `./sut -v`
  - Then I get `sut version 1.0`
  ~~~

- Given the new file `NOK_scen.md`
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

### Scenario: failing run names the failed scenarios

- When I run `./bbt -c --yes NOK_scen.md`

- Then there is an error
- And the output contains
  ~~~
  - [ ] scenario   [](NOK_scen.md) **fails**

  ## Summary : **Fail**
     - [ ] scenario   [](NOK_scen.md) **fails**

  | Status     | Count |
  |------------|-------|
  | Failed     | 1     |
  | Successful | 0     |
  | Empty      | 0     |
  | Not Run    | 0     |
  ~~~
