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

- PTY for interactive commands  
  Interactive input is now supported (`When I type`, `When I enter`), but
  commands are fed through pipes, and a program that does not flush its
  prompt before reading cannot be tested. Giving it a pseudo terminal
  would remove this constraint on the software under test.  
  cf. [pty](../proposed_features/pty.md)

### Low priority

- append / remove  
  To append / remove text to an existing text file

- implement a "case insensitive" modifier
- explore the possibility to run multiple exe in //, while staying simple.  
  Maybe by using the AdaCore spawn lib.

- "no new files" and "no env change" check


- status bar animation during long commands  
  The status bar, displayed by default on a terminal, draws a
  transient bar on the current line, with a spinner and a
  `<done>/<total>` scenario counter
  (cf. [B210_Status_Bar.md](../features/B210_Status_Bar.md)).
  The spinner advances at each step only: it stays frozen while a
  single command runs for a long time. A background task refreshing
  the bar on a timer would remove this, at the cost of serializing
  the writes on the standard output.  
  cf. the [status bar rendering](design_discussions.md) design discussion

- readers and writers organization  
  Factorize the format knowledge (Markdown_Utilities is a first
  step), and design for a future non Markdown format: Text_Writer
  is de facto a second Markdown writer, and the balance between
  factorization and flexibility has to be worked.  
  cf. the [readers and writers organization](design_discussions.md)
  design discussion

- LCS based diff alignment  
  Error messages compare expected and actual positionally; an LCS based
  alignment, as in diff or git, would give more relevant results.  
  cf. [LCS_diff_alignment](../proposed_features/LCS_diff_alignment.md)

- Table input (In gherkin : `Scenario Outlines` / `Examples` https://cucumber.io/docs/gherkin/reference/)
May imply to switch to Max Reznik's more sophisticated MarkDown parser...

- Adding a command line completion generation  
  cf. to https://rust-lang.github.io/rustup/installation/index.html#enable-tab-completion-for-bash-fish-zsh-or-powershell  
  and https://github.com/carapace-sh


