# Feature: bbt is not perturbed by rich Markdown constructs in scenario bodies

Tables, mermaid diagrams and other ordinary Markdown constructs are
just text for the bbt analyzer: they never generate steps, scenarios
or errors, even when their content looks like Gherkin (keywords in
table cells, step-like lines in a diagram, list-items in table rows).

### Scenario: tables and diagrams with Gherkin-like content

The expected report below is the whole point: only the two real steps
appear, the table row carrying a step-like line generates nothing, and
the run is successful.

- Given the new file `md_constructs.md`
  ~~~md
  # Feature: rich constructs

  ## Scenario: md constructs in body

  | Given | When | Then |
  |-------|------|------|
  | a     | b    | c    |

  ```mermaid
  graph TD;
    Given --> When;
    When --> Then;
  ```

  - When I run `echo ok`
  - Then the output contains `ok`

  | Col here |
  |----------|
  | - Then fake step |
  ~~~

- When I run `./bbt -c md_constructs.md`
- Then the output is
  ~~~

  # Document: [md_constructs.md](md_constructs.md)
    ## Feature: rich constructs
     ### Scenario: [md constructs in body](md_constructs.md):
     - OK : When I run `echo ok`
     - OK : Then the output contains `ok`
     - [X] scenario   [md constructs in body](md_constructs.md) pass

  ## Summary : **Success**, 1 scenarios OK
  ~~~
