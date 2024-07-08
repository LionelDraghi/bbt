Project  <!-- omit from toc -->
=======

- [Development Status](#development-status)
- [Tests](#tests)
- [Help and comments](#help-and-comments)
- [TDL](#tdl)
  - [Near future](#near-future)
  - [Distant future or low priority](#distant-future-or-low-priority)

## Development Status
- [Changelog](changelog.md)
- [Fixme](fixme.md)

## Tests
- [Examples](examples_results.md)
- [Features](features_results.md): **this is where behavior is defined** 
- *NB: Robustness tests and tests returning error are not yet reported here*

## Help and comments
Comments are welcome [here](https://github.com/LionelDraghi/bbt/discussions)

## TDL

### Near future
  - clean function  
  ask bbt to delete all files created during test run (ongoing effort)

### Distant future or low priority
- ``Given execution directory `dir1` ``  
  Create a dir and move test run into that dir

- interactive exec
  Test of command waiting for user input

- environment  
  Check and/or set environment variable

- append / remove  
  To append / remove text to an existing text file

- implement "case insensitive" and "ignore blank lines" 
  
- explore the possibility to run multiple exe asynchronously, while staying simple.  
  Maybe by using the AdaCore spawn lib.

- "should be" as "is" synonym?
  
- "no new files" and "no env change" check

- Table input (In gherkin : `Scenario Outlines` / `Examples` https://cucumber.io/docs/gherkin/reference/)
May imply to switch to Max Reznik's more sophisticated MarkDown parser...

