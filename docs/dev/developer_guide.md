# Developer Guide <!-- omit from toc -->

- [Design overview](#design-overview)
- [Tests](#tests)
- [Issues and discussions](#issues-and-discussions)
- [Development Status](#development-status)
- [Development environment](#development-environment)
- [External dependencies](#external-dependencies)


## Design overview

==== DRAFT ====

bbt processing implements essentially a sequential process:

1. First step is the command line analysis.

It’s implemented in BBT.Main procedure and child packages.  
   
The list of scenario files matching the pattern on command line is stored in BBT.Scenario.Files.BBT_Files during this step.  
Options resulting from that analysis are store in the BBT.Settings package, and may be displayed thanks to `bbt -ls` (that stands for list settings).

2. Second step is the parsing of the documentation files.

It is mostly implemented in BBT.Scenarios package and child packages.

It’s goal is to extract the Gherkin structure from the scenario files, that is mostly identifying scenarios, features, background and steps text, whatever is the analyzed text format.

For example, in a Markdown file, a scenario may be declared this way:  
`## Scenario: My_Scenario`

But in a Asciidoc text, it will be:  
`== Scenario: My_Scenario`

In both cases, this first lexer will return a Scenario called My_Scenario, and the location in the file for error message purpose.
 

3. Building the scenario

Each token recognized by the scenario lexer is pushed to a scenario builder, with an internal FSM.  
The scenario builder is implemented in BBT.Tests.Builder package and child packages.  

It is in charge of building the internal representation of analyzed scenarios, that is a list of documents (files), containing a list of feature and background and scenarios.  
Coherency tests on the scenario structure are done here: for example, if a step is declared before any background or scenario declaration, it will be an error.  

The internal representation of the scenarios is stored in the BBT documentation package.

4. Parsing of the steps

One of the component of the scenario file will be further analyzed, it’s the step text. 

=== to be completed ===

## Command execution and interaction

The commands of the `When I run` steps are executed through the
[ada-util](https://github.com/stcarrez/ada-util) library (Alire
dependency `utilada`), synchronously: the command output and error
descriptors, exposed by the library raw streams, are polled and
appended to the same output files as the previous Spawn based
implementation did, and the exit code is collected when the command
terminates.

The choice of this library, and the alternatives that were rejected,
are recorded in [design_discussions.md](design_discussions.md).

A fresh process object is created for each command, and freed once
the command is terminated and reaped: contrary to the Spawn monitor,
the library keeps no process map surviving the command. The child
environment is set with `Set_Default_Environment` at spawn time, a
snapshot of the bbt current environment, so that the environment
variable steps apply to the commands.

When a scenario contains a `When I type` or a `When I enter` step,
its commands are started on a pseudo terminal allocated by the library
(`Set_Allocate_TTY`), configured in raw mode by construction: the
prompts are visible before the program waits for input, without any
explicit flush, the input is not echoed, and the output bytes are
exact, so the expected outputs are the same as over pipes
(cf. [A290](../features/A290_When_I_Type_Or_Enter.md), and
[proposed_features/pty.md](../proposed_features/pty.md) for the
experiment results that led to this design). The pseudo terminal is
given a fixed 80x24 window size (`BBT.Terminal`, through a thin C
wrapper on `ioctl(TIOCSWINSZ)`, as `ioctl` is variadic and its
request constant is platform dependent). The command is
started and not awaited: it is fed across steps, and the synchronization
relies on two event pumping primitives:

- `Wait_Quiet`: the command has produced no more output during a short
  window: its prompt is complete, the input can be sent;
- `Wait_Response`: the command has produced some output after the last
  input (or since the command start, when no input was sent yet), and
  then stayed quiet: its response is complete, and can be checked.

## Tests

`make` or `make check` run different kind of tests :
1. First of all, this is what it was designed to achive, all feature descriptions in docs/features

2. Examples from the documentation in docs/examples

3. A few unit testing in tests subdirectories 

Regarding features, files are prefixed with a category letter and index number.
Categories are :  
  A. Features related to the language (both Gherkin framework and bbt steps) : for example, the test of the `output does not contain` form.  
  B. Features of bbt overall behavior : for example, test of the --cleanup feature;  
  C. Robustness and compliance test : for example, test of bbt tolerance to weird formatted markdown files.
 
The index number is not significant, but lowest number are supposed to be more basic features.  

Tests are run in the tests directory, so that docs will not be polluted with possibly remaining files.

All test results per platform are then available in docs/tests_results/Windows|Linux|Darwin  
On each push on main, the CI republishes the results and badges of the three platforms, so a manual run on each platform is no more needed to update them.

 > [!NOTE] On Mac, GNAT_FILE_NAME_CASE_SENSITIVE must be set to 1 (refer to https://forum.ada-lang.io/t/name-file-casing-error-on-darwin/1795) othwise some tests will fail. 


## Issues and discussions

- [Issues](https://github.com/LionelDraghi/bbt/issues)
- [Discussions](https://github.com/LionelDraghi/bbt/discussions)

## Development Status

- [Changelog](../changelog.md)
- [Fixme](fixme_index.md)
- [References to issue in code and tests](issues_index.md)

## Development environment

Some external tools are required to fully run the Makefile, and 
I don't know whether they are available on Windows, Mac-OS or even on all Linux distributions.  
I try to minimise the number of those dependencies, and also try to choose widely available tools.

No external tools are required to compile and run all tests (within docs/features), except the gnat compiler and Alire.
To avoid external dependencies, a false exe named `sut` (that means Software Under Test) is used for `bbt` tests own needs.  
Sources of `sut` are in the `tools` sub-directory.

## External dependencies

1. For the sake of clarity, the examples (within docs/examples) use a real life app and you'll need to have in the PATH the exe "tested", that is `gcc`, `rpl`, etc.  
2. [mlc](https://github.com/becheran/mlc?tab=readme-ov-file#markup-link-checker) is used to check links in all Markdown files.
3. Alire dependencies, declared in alire.toml: `ansiada` (colors in the results output), and `utilada` (command execution and pseudo terminal for the interactive commands, cf. [Command execution and interaction](#command-execution-and-interaction)).
