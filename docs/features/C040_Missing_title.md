<!-- omit from toc -->
## Scenario: Missing tittle in scenario, background and feature

Titles are not mandatory for feature, background and scenario Header.
Report will obviously be less readable, but it should not prevent to run the test.

- Given the new file `no_title.input`
```
## Feature

### Background
No Tittle Background
- When I successfully run `./sut --version`

### Scenario
No Tittle Scenario
- When I successfully run `./sut -v`
```

- When I run `./bbt no_title.input`
- Then output contains 
```
## [no_title.input](no_title.input)  
  ### Feature:   
  - [X] scenario [](no_title.input) pass  
  - [X] scenario [](no_title.input) pass 
```
- And output contains 
```
Success, 1 scenarios OK  
```