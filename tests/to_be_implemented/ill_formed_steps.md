# Feature: Ill Formed Steps detection

Partial formater are very tolerant to alternate wording. 
The downside is more difficult to identify grammar problem.
Here are some detected problem.

## Scenario: `run` not followed by a command

- Given the file `scen1.md`
```
# Scenario:
- When I run
```

- When I run `./bbt scen1.md`
- Then I get an error
- And the output contains `Error : Unrecognized step `
- And the output contains `Warning : No command after "run" keyword`

## Scenario: `run X or` not followed by a second command

- Given the new file `scen1.md`
```
# Scenario:
- When I run `./sut -h` or xyz
```

- When I run `./bbt scen1.md`
- Then I get an error
- And the output contains `Error : Unrecognized step `
- And the output contains `Warning : Command missing after last "or"`
