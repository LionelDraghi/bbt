<!-- omit from toc -->
## Scenario: Missing tittle in scenario, background and feature

Titles are not mandatory for feature, background and scenario Header.
Report will obviously be less readable, but it should not prevent to run the test.

- Given the new file `no_title.md`
  ```md
  ## Feature

  ### Background
  No Tittle Background
  - When I successfully run `./sut --version`

  ### Scenario
  No Tittle Scenario
  - When I successfully run `./sut -v`
  ```

- When I run `./bbt -c --yes no_title.md`
- Then output is
  ```md
  # Document: [no_title.md](no_title.md)
  ## Feature:   
  ### Scenario: [](no_title.md):
   - OK : When I successfully run `./sut -v`  
   - [X] scenario   [](no_title.md) pass  

  ## Summary : **Success**, 1 scenarios OK
  ```