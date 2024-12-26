Project  <!-- omit from toc -->
=======

- [Development Status](#development-status)
- [Tests](#tests)
- [Help, comments, suggestions, feedback...](#help-comments-suggestions-feedback)
- [TDL](#tdl)
  - [Near future](#near-future)
  - [Distant future or low priority](#distant-future-or-low-priority)

## Development Status
- [Changelog](changelog.md)

## Tests

`make` or `make check` run different kind of tests :
1. First of all, this is bbt vocation, all feature descriptions in docs/features

2. Examples from the documentation in docs/examples

3. A few unit testing in tests subdirectories 

Regarding features, files are prefixed with a category letter and index number.
Categories are :  
  A. Features related to the language (both Gherkin framework and bbt steps) : for example, the test of the `output does not contain` form.  
  B. Features of bbt overall behavior : for example, test of the --cleanup feature;  
  C. Robustness and compliance test : for example, test of bbt tolerance to weird formatted markdown files.
 
The index number is not significant, but lowest number are supposed to be more basic features.  

Tests are run in the tests directory, so that docs will not be polluted with possibly remaining files.

All test results are then available in docs/tests_results


## Help, comments, suggestions, feedback...
- [Discussions](https://github.com/LionelDraghi/bbt/discussions)
- [Issues](https://github.com/LionelDraghi/bbt/issues)

## TDL

Note that Ideas are welcomed. You may submit yours through [Issue](https://github.com/LionelDraghi/bbt/issues), or by directly pushing a new file in docs/features/proposed_features.


### Near future

### Distant future or low priority
- interactive exec
  Test of command waiting for user input

- environment  
  Check and/or set environment variable

- append / remove  
  To append / remove text to an existing text file

- implement "case insensitive" and "ignore blank lines" modifiers
  
- explore the possibility to run multiple exe asynchronously, while staying simple.  
  Maybe by using the AdaCore spawn lib.

- "no new files" and "no env change" check

- Table input (In gherkin : `Scenario Outlines` / `Examples` https://cucumber.io/docs/gherkin/reference/)
May imply to switch to Max Reznik's more sophisticated MarkDown parser...

- Add a strategy pattern to be able to process files according to their extension :

  - Markdown : .md (.markdown .mkd .mdown)
  - AsciiDoc : .adoc (.asciidoc .ad .asc)
  - reStructuredText : .rst (.rest) 
  and separate presentation vs semantic in the code, with a strategy pattern for presentation.

- Synonyms : 

  Stderr = error output  
  Stdout = output

