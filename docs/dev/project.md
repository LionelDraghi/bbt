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
  Implemented on the `pty` branch (Spawn replaced by ada-util, the
  commands of the interactive scenarios run on a pseudo terminal in
  raw mode), all suites green on Linux; pending the Spawn / utilada
  comparison and the Windows paths validation before arbitration.  
  cf. [pty](../proposed_features/pty.md)

- Explicit terminal context  
  The interactive scenarios (`type` / `enter`) now always run on a
  pseudo terminal: the "answers fed through a pipe" context
  (`printf 'y\n' | ask`) is not testable anymore, although it is a
  real deployment context, that a program detecting a non-terminal
  standard input behaves differently there. An explicit context
  keyword, e.g. `When I run in a terminal`, pipes staying the
  default, would let a scenario state the deployment context it
  tests, instead of the harness inferring it from the steps.  
  cf. [pty](../proposed_features/pty.md)

- ada-util upstream proposals  
  Three functions that would simplify the bbt engine and add value
  to the library: `Wait_Event` (the missing event model, that would
  also make the Windows pump port trivial), `Set_Terminal_Size`,
  and a non blocking termination check. Filed on stcarrez/ada-util
  as #72, #73 and #74; #74 is implemented by PR #75 and #73 by
  PR #76, both pending review.  
  cf. [pty](../proposed_features/pty.md)

### Low priority

- non interactive commands output redirect  
  Let the child write the output files directly through the
  library redirections, instead of pumping, for the commands that
  run to completion; the pump would remain for the interactive
  commands only.  
  cf. [non_interactive_redirect](../proposed_features/non_interactive_redirect.md)

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


