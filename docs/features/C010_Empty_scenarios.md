<!-- omit from toc -->
## Empty scenarios

A scenario without any step may be a work in progress, in which case this is not an error.

But it could also be an error (for example, a missing dash ("-") at the beginning of step lines, or a missing "##" before the scenario line).
And we don't want that to be unnoticed, so :
1. there is a specific "Empty tests" count in the output.
2. When there is a scenario, it will be displayed like a fail scenario, with a non checked box.
3. when there is no step **and** no scenario, there is a specific warning.

_Table of Contents:_
- [Scenario: No step test](#scenario-no-step-test)
- [Scenario: No scenario in Feature](#scenario-no-scenario-in-feature)

### Scenario: No step test

- Given the `no_step_in_scenario.input` file
```md
## Scenario: My_Scenario
```

- When I run `./bbt no_step_in_scenario.input` 

- Then the output contains `scenario [My_Scenario](no_step_in_scenario.input) is empty, nothing tested`
- And  the output contains `| Empty      |  1`
- And  I get no error
  
### Scenario: No scenario in Feature

- Given the `no_step_or_scenario_in_feature.input` file
```md
## Feature: My_Feature
```

- When I run `./bbt no_step_or_scenario_in_feature.input` 

- Then the output contains `Warning : No scenario in feature "My_Feature"`
- And  the output contains `| Empty      |  1`
- And I get no error
