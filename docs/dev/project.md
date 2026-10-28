<!-- omit from toc -->
Project Status 
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

Note that Ideas are welcomed. You may submit yours through [Issue](https://github.com/LionelDraghi/bbt/issues), or by directly pushing a new file in docs/proposed.

### High priority

- Explicit terminal context  
  The interactive scenarios (`type` / `enter`) now always run on a
  pseudo terminal: the "answers fed through a pipe" context
  (`printf 'y\n' | ask`) is not testable anymore, although it is a
  real deployment context, that a program detecting a non-terminal
  standard input behaves differently there. An explicit context
  keyword, e.g. `When I run in a terminal`, pipes staying the
  default, would let a scenario state the deployment context it
  tests, instead of the harness inferring it from the steps.  
  cf. [pty](../proposed/pty.md)

- ada-util upstream proposals  
  Three functions that would simplify the bbt engine and add value
  to the library: `Wait_Event` (the missing event model, that would
  also make the Windows pump port trivial), `Set_Terminal_Size`,
  and a non blocking termination check. Filed on stcarrez/ada-util
  as #72, #73 and #74; the PRs #75 and #76 were taken into account
  upstream and closed. What remains is the release of the next
  utilada version and its publication in Alire, so that bbt can
  raise its `utilada` constraint and use the functions.  
  cf. [pty](../proposed/pty.md)

### Low priority

- model types encapsulation: limited private  
  The model specs (`bbt-model-documents.ads`, `bbt-model-features.ads`,
  `bbt-model-scenarios.ads`, `bbt-model-steps.ads`) still expose more
  than needed (`Document_Type` is not private yet, cf. the fixme in the
  spec). A first pass at making the model types limited private was
  stashed away unfinished (git stash « passage en limited private du
  modèle », April 2026, ~100 lines on the model specs only): it is kept
  as the historical basis, but is two major refactors old — the work is
  to be redone on the current code base, using the stashed intent as a
  guide, and completed (cf. also the `Fixme: should be private` marks
  in the model specs).

- a User Guide entry on snapshot testing / golden files  
  [UG.md](../UG.md) has no section on comparing a whole output against a
  reference file: when it is the right technique, how to keep the
  reference stable (time tags, paths, version numbers, ... masked with
  the `(ignoring ...)` clause), how to generate and refresh the
  reference file, and when to prefer focused `is` / `contains` /
  `matches` checks instead. cf. [A080_Then_Output_Equal_To_File.md](../features/A080_Then_Output_Equal_To_File.md),
  [A300_Ignoring.md](../features/A300_Ignoring.md) and the bbt skill
  authoring principles on expected results.

- the (ignoring ...) clause on the matches steps  
  The clause is currently accepted on the comparison steps `is`,
  `contains` and their negated forms, on output, file and error output,
  but rejected on `matches` / `does not match`. The semantics, when
  allowed, is arbitrated: the clause masks the volatile parts **of the
  actual line, outside of what the regexp already matches** — the
  regexp itself is applied as is, never masked. In other words, each
  part of the line must be either matched by the regexp or masked by
  the clause: the regexp states the stable part it checks precisely,
  and the clause neutralizes the rest (a varying counter, a time tag,
  a path ...), instead of forcing the regexp to abstract the volatile
  parts itself.  
  The change is twofold: the classification in Validate_Step_State
  (bbt-scenarios-steps-validate_step_state.adb), and the check itself,
  which must combine the regexp spans and the clause masks on the line
  before deciding (the other comparisons mask both sides before
  comparing, which does not transpose to a regexp).  
  Scenarios to be added in [A300_Ignoring.md](../features/A300_Ignoring.md).

- masking patterns scope and danger: review whether the doc, help and
  examples warn enough about the `(ign ...)` clauses dangers - silently
  weakened checks, too broad patterns, interactions between named
  patterns (e.g. a clock time vs a location). The `version` pattern is
  too permissive: it masks any isolated decimal, not only version
  numbers, so durations in compared files are silently filtered too.
  To be treated later.
- cleanup level 2, snapshot and diff of the exec dir: implemented as
  `--cleanup=full` (cf. [B010](../features/B010_Deleting_created_files.md)
  and the changelog); to be validated on Windows and macOS  

- non interactive commands output redirect  
  Let the child write the output files directly through the
  library redirections, instead of pumping, for the commands that
  run to completion; the pump would remain for the interactive
  commands only.  
  cf. [non_interactive_redirect](../proposed/non_interactive_redirect.md)

- run in shell step: implemented (cf.
  [A310_Run_Shell.md](../features/A310_Run_Shell.md) and the design
  discussion [D11](design_discussions.md))  
  A dedicated step running the command through a configurable
  shell (`--shell` setting, `BBT_SHELL` override), for the steps
  that need pipes, redirections or command substitutions; before it,
  the author had to spawn the shell explicitly, e.g.
  `sh -c "cmd1 | cmd2"`.

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

- ignoring volatile parts in comparisons  
  Time tags, machine dependent absolute paths and Windows versus Unix
  path syntax frequently break comparisons on parts the scenario does
  not target; the workarounds (full line regexps in `matches`, sed
  filtering) are heavy and weaken the stable part of the check. An
  explicit `ignoring` clause on the comparison steps masks or
  normalizes the volatile parts on both sides before comparing.  
  The keyword is arbitrated and the step clause increments (`ignoring time`,
  `ignoring dir`, `ignoring version`, `ignoring location`,
  `ignoring path syntax`, and any regexp as in
  `ignoring [0-9.]+`) are specified and implemented
  in [A300_Ignoring.md](../features/A300_Ignoring.md); the `Given`
  step, the command line option
  and the scope precedence are deferred, as is the diff display
  showing the ignored segments in a distinct color, which pairs with
  the [diff display alignment](design_discussions.md).  
  cf. the [ignoring design discussion](design_discussions.md)

- Table input (In gherkin : `Scenario Outlines` / `Examples` https://cucumber.io/docs/gherkin/reference/)
May imply to switch to Max Reznik's more sophisticated MarkDown parser...

- Adding a command line completion generation  
  cf. to https://rust-lang.github.io/rustup/installation/index.html#enable-tab-completion-for-bash-fish-zsh-or-powershell  
  and https://github.com/carapace-sh


