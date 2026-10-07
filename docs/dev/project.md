Project Status <!-- omit from toc -->
==============

- [Development Status](#development-status)
- [Help, comments, suggestions, feedback...](#help-comments-suggestions-feedback)
- [TDL](#tdl)
  - [High priority](#high-priority)
  - [Low priority](#low-priority)

## Development Status

- [Changelog](../changelog.md)

## Help, comments, suggestions, feedback...

- [Discussions](https://github.com/LionelDraghi/bbt/discussions)
- [Issues](https://github.com/LionelDraghi/bbt/issues)

## TDL

Note that Ideas are welcomed. You may submit yours through [Issue](https://github.com/LionelDraghi/bbt/issues), or by directly pushing a new file in docs/features/proposed_features.

### High priority

- interactive exec
  Test of command waiting for user input

### Low priority

- environment  
  Check and/or set environment variable

- append / remove  
  To append / remove text to an existing text file

- implement "case insensitive" and "ignore blank lines" modifiers
  
- explore the possibility to run multiple exe in //, while staying simple.  
  Maybe by using the AdaCore spawn lib.

- "no new files" and "no env change" check

- bounding the tests execution time  
  A hanging command under test, typically one waiting for an input
  that no step provides, blocks the whole run: an option bounding the
  time per scenario, or the total run time, would make bbt fail fast
  with an explicit message instead.  
  cf. [timeouts](../proposed_features/timeouts.md)

- LCS based diff alignment  
  Error messages compare expected and actual positionally; an LCS based
  alignment, as in diff or git, would give more relevant results.  
  cf. [LCS_diff_alignment](../proposed_features/LCS_diff_alignment.md)

- Table input (In gherkin : `Scenario Outlines` / `Examples` https://cucumber.io/docs/gherkin/reference/)
May imply to switch to Max Reznik's more sophisticated MarkDown parser...

- Synonyms : 

  Stderr = error output  
  Stdout = output

- Adding a command line completion generation  
  cf. to https://rust-lang.github.io/rustup/installation/index.html#enable-tab-completion-for-bash-fish-zsh-or-powershell  
  and https://github.com/carapace-sh


