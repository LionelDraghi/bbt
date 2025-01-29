## Feature : Strict Gherkin rules 

Gherkin is typically used in a BDD context, where it is advised for example to:
- avoid multiples When steps in the same scenario,
- avoid multiples When/Then sequences in the same scenario, 
- use only Given steps in Background,
- etc.

Even if those rules are far from being universally accepted, there is a rational behind : BDD is about communication, and so each scenario should focus on a single behavior, and as a consequence there is only one When per scenario.  

But in a test context, this doesn't apply. If for example I'm writing a non regression test for a bug, I'm likely to sequence all user actions that lead to the bug situation. It will be a single scenario. If the bug arise after 5 user actions, I'm not going to write 5 scenarios.
It would be less readable, going against the communication goal.  

By default, bbt is much more flexible than Gherkin, allowing to uses Steps in whatever order, including in Background.

Never the less, a `--strict` option is provided that emit Warnings on (some) of those rules. 


### Scenario : Multiple When in a scenario

- Given there is no file `tmp.txt`
- Given the file `t1.md`
  ```
  # Scenario
  - When I run `./sut create tmp.txt`
  - When I run `./sut append fillet  tmp.txt`
  - When I run `./sut append chamfer tmp.txt`
  ```

- When I successfully run `./bbt --strict t1.md`
- Then the output contains
  ```
  t1.md:3: Warning : Multiple When in the same Scenario
  ```
- And `tmp.txt` contains 
  ```
  fillet
  chamfer
  ```

- When I successfully run `./bbt t1.md`
- Then the output do not contain `Warning`
- And `tmp.txt` contains 
  ```
  fillet
  chamfer
  ```  
  (We check that the result is the same with or without the Warning)
