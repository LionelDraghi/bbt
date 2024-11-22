Project  <!-- omit from toc -->
=======

- [Development Status](#development-status)
- [Tests](#tests)
- [Help, comments, feedback...](#help-comments-feedback)
- [TDL](#tdl)
  - [Near future](#near-future)
  - [Distant future or low priority](#distant-future-or-low-priority)

## Development Status
- [Changelog](changelog.md)

## Tests
- [Examples](tests/examples_results.md)
- [Features](tests/features_results.md): **this is where behavior is defined**  
  Features file are prefixed with a category letter and index number.
  Categories are :  
  A. Features related to the language (both Gherkin framework and bbt steps) : for example, the test of the `output does not contain` form.  
  B. Features of bbt overall behavior : for example, test of the --cleanup feature;  
  C. Robustness and compliance test : for example, test of bbt tolerance to weird formatted markdown files.
  
  The index number is not significant, but lowest number are supposed to be more basic features.  

## Help, comments, feedback...
- [Discussions](https://github.com/LionelDraghi/bbt/discussions)
- [Issues](https://github.com/LionelDraghi/bbt/issues)

## TDL

### Near future

### Distant future or low priority
- interactive exec
  Test of command waiting for user input

- environment  
  Check and/or set environment variable

- append / remove  
  To append / remove text to an existing text file

- implement "case insensitive" and "ignore blank lines" modifyers
  
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

