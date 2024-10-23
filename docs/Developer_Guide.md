# Developer Guide <!-- omit from toc -->

- [Design overview](#design-overview)
- [Issues and discussions](#issues-and-discussions)
- [Development Status](#development-status)

## Design overview 

==== DRAFT ====

bbt processing implement essentially a sequential process:

1. First step is the command line analysis.

It’s implemented in BBT.Main procedure and child packages.  
   
The list of scenario file matching the pattern on command line is stored in BBT.Scenario.Files.BBT_Files during this step.  
Options resulting from that analysis are store in the BBT.Settings package, and may be displayed thanks to `bbt -ls` (that stands for list settings).

2. Second step is the parsing of the documentation files.

It is mostly implemented in BBT.Scenarios package and child packages.

It’s goal is to extract the Gherkin structure from the scenario files, that is mostly identifying scenarios, features, background and steps text, whatever is the analyzed text format.

For example, in a Markdown file, a scenario may be declared this way:  
> ## Scenario: My_Scenario

But in a Asciidoc text it will be:  
> == Scenario: My_Scenario

In both case, this first lexer will return a Scenario called My_Scenario, and the location in the file for error message purpose.
 

3. Building the scenario

Each token recognized by the scenario lexer is pushed to a scenario builder, with an internal FSM.  
The scenario builder is implemented in BBT.Tests.Builder package and child packages.  

It is in charge of building the internal representation of analyzed scenarios, that is a list of documents (files), containing a list of feature and background and scenarios.  
Coherency tests on the scenario structure are done here: for example, if a step is declared before any background or scenario declaration, it will be an error.  

The internal representation of the scenarios is stored in the BBT. Documentation package

4. Parsing of the steps

One of the component of the scenario file will be further analyzed, it’s the step text. 

=== to be completed ===

## Issues and discussions

- [Issues](https://github.com/LionelDraghi/bbt/issues)
- [Discussions](https://github.com/LionelDraghi/bbt/discussions)

## Development Status
- [Changelog](changelog.md)
- [Fixme](fixme.md)
- [References to issue in code and tests](issues.md)

