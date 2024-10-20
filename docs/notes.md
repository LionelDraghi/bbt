# Notes

## Manipulation du code de retour avec bash :
https://debian-facile.org/doc:programmation:shells:script-bash-etat-de-sorie-et-les-tests


## What I don’t want :

  - Cram uses snippets of interactive shell sessions.
    This is not portable, unless providing a complete “bash like” environment in Windows, for example.
    And it’s using a “shell like” syntax, with meaningful indentation (horror), a $ sign at the beginning of the command, and a > sign for continuation line.  
    I want something clear, as close as English as possible, with no cryptic signs.  
    Same apply to output : when comparing the actual output with the expected one, I don’t care to have a classical [unified diff format](https://en.wikipedia.org/wiki/Diff#Unified_format) that could be used by some other tools.  
    What a want is directly a clear side by side diff that is understandable by humans.

## Design

### AST

Gerkhin file data model :  https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast

### Markdown compliance and vocabulary
The BBT Markdown subset try to comply with [CommonMark Spec](https://spec.commonmark.org/)

Only [ATX Heading](https://spec.commonmark.org/0.31.2/#atx-headings) are supported, not [Setext](https://spec.commonmark.org/0.31.2/#setext-headings), meaning that you can write :
```
## Feature
```
but not
```
Feature
-------
```
in the test file.

### References
https://github.com/briot/gnatbdd/tree/master
https://github.com/dcurtis/markdown-mark


[Specification as a ubiquitous language](https://en.wikipedia.org/wiki/Behavior-driven_development#Specification_as_a_ubiquitous_language)
*A ubiquitous language is a (semi-)formal language that is shared by all members of a software development team — both software developers and non-technical personnel.*

Interesting discussion on observability and TTD [here](https://www.youtube.com/watch?v=prLRI3VEVq4&t=2190s)


## Synonymes : ???

  Stderr = error output  
  Stdout = output

# Design decision
