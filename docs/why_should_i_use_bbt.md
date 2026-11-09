# Why should I use *bbt*?

## Write once

The specification is the only source of truth. This is *bbt*'s most compelling feature, there is nothing else: no intermediate representation, no glue code, no scripting language, no duplication of the original source at all.  

With two main consequences: 
1. writing a test is a matter of minutes,
2. there is no more place for a discrepancy between documentation and tests.

Alternative tools exist, some are mentioned in [my quick overview of some comparable tools](comparables.md).  
But as far as I know, ***bbt* is the only one to provide such a direct "run the doc" approach**.

## Write real documentation, not just a ".feature" Gherkin file  

*bbt* effectiveness does not come at the cost of limiting documentation readability or expressiveness: 

- First, the vast majority of the file is just plain markdown : use it, structure it the way you like, give as much context as you want, and use all Markdown cool extensions (for example graphics with [Mermaid](https://mermaid.js.org/intro/));
- Second, even the part that is interpreted by *bbt*, the steps, is written in readable English thanks to the partial parsing.

Nice consequence, *bbt* scenarios may be written by non coders people.

## Be proficient in no time

*bbt* Steps uses a limited English subset, with a vocabulary dedicated to test with no-surprise keywords like *run*, *output*, *contains*, etc.  

Although simple, you don't need to memorize this subset, you can :
- ask for a example scenario by running `bbt help example`, 
- ask for a short but pretty comprehensive tutorial with `bbt help tutorial`.  

## No-fuss, no-wait run

To run a scenario : `bbt my_scenario.md`  
To run all the md files in the *tests* tree `bbt -r tests`  
To run only a selection `bbt --select "Sanity check" tests`  

*bbt* has no dependencies on external lib or tools (diff, for example), and is tested on Linux, Darwin and Windows.  

## Ready-to-publish output 

*bbt* output is in Markdown format. You can adjust the detail level with the usual "-q" and "-v" options.

The output cross-references the executed scenario files: if a test fails, just click on the link and you are in the scenario.  
You can push it on GitHub without further processing.  

To see what it looks like, consider [*bbt* own tests](tests_results/Linux/features_results.md).  

Test results are generated when running *bbt*, by just using the `--output | -o` option.

## Prefer end-to-end tests

Everything above boils down to one guideline, the same one that
[Sergio Delgado's frontend-testing best practices](https://github.com/sergiodxa/agent-skills)
summarize in four rules:

- **Prefer E2E tests over unit tests** — test the whole system, not isolated pieces;
- **Minimize mocking** — if you need complex mocks, write an E2E test instead;
- **Test behavior, not implementation** — assert what users see and do, not how the code is organized;
- **Don't test UI components directly** — exercise them through the user path.

*bbt* applies exactly this philosophy, with an extra twist: since scenarios
need no driver other than the CLI, and no mocking at all, they can live
right in the README — one single source of truth for documentation and tests.
  
