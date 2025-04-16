[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

[![Made with Ada](https://img.shields.io/badge/Made%20with-Ada-1f425f.svg)](https://ada-lang.io/) [![Awarded](https://img.shields.io/badge/Ada_Crate_of_the_Year-2024-black)](https://blog.adacore.com/ada-spark-crate-of-the-year-2024-winners-announced)

[![truc](https://img.shields.io/badge/play-station-blue.svg?logo=data:image/svg%2bxml;base64,CiAgICA8c3ZnIGlkPSJzdmdfcm9vdCIgdmlld0JveD0iMCAwIDMwMCAzMDAiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+CiAgICAgIDxyZWN0IGlkPSJzdmdfZnJhbWUiIHdpZHRoPSIzMDAiIGhlaWdodD0iMzAwIiBzdHlsZT0iZmlsbDpub25lO3N0cm9rZS13aWR0aDoxMCIgc3Ryb2tlPSJub25lIj48L3JlY3Q+CiAgICAgIDwhLS0gPGNpcmNsZSBpZD0ic3ZnX2NpcmNsZV9mcmFtZSIgY3g9IjE1MCIgY3k9IjE1MCIgcj0iMTQ4IiBzdHlsZT0iZmlsbDpub25lO3N0cm9rZS13aWR0aDo1IiBzdHJva2U9ImJsYWNrIi8+IC0tPgogICAgICA8ZyB0cmFuc2Zvcm09InRyYW5zbGF0ZSg4LCA0MCkiPgogICAgICAgIDxnIHRyYW5zZm9ybT0ibWF0cml4KDEuMzMzMzMzMywwLDAsLTEuMzMzMzMzMywwLDE5OS45OTk5OSkiIGlkPSJnMTAiPgogICAgICAgICAgPGcgdHJhbnNmb3JtPSJzY2FsZSgwLjA5NSkiPgogICAgICAgICAgICA8cGF0aCBpZD0ic3ZnX2xvZ28iIGZpbGw9IiMxMTQ0NzMiIHN0eWxlPSJmaWxsLW9wYWNpdHk6MTtmaWxsLXJ1bGU6bm9uemVybztzdHJva2U6bm9uZTtzdHJva2Utd2lkdGg6MC43OTk3ODQ3MiIgZD0ibSAxNzM1Ljk0LDYwOC40OTY4NyBjIDAsMzguODk1OTMgNDAuOTQxLDUzLjIxNjg3IDEwNy40OTEsNjAuMzg2OTUgNTUuMjY1Miw2LjEzOTE0IDk5LjI4NTMsMTUuMzU1MDcgMTI3Ljk0OTYsMzMuNzc4OSB2IC00OS4xMzA3NyBjIDAsLTU4LjY0MDIyIC00OS4zNzA2LC04Ny4xNjQ1NSAtMTE1LjI4MDksLTkzLjY3Nzk5IC0yNi44ODg4LDIuNDk5MzMgLTU1LjAyNTMsNC44NDI2OSAtODQuMjQxNCw2Ljk4MjEzIC0yMi43Mjk5LDguNjY5NjYgLTM1LjkxODMsMjMuNDAwMDkgLTM1LjkxODMsNDEuNjYwNzggeiBNIDQ2MS40ODcwNCw1OTEuMDk1MTUgaCAyODMuNTU4MDggbCAyMy4yNjU3NCwtNjguODA5NDggQyA2NDYuMjY1MzEsNTAwLjA3MzI1IDUyNC43NjY4MSw0NzAuODE1NTIgNDA3LjU1NDM2LDQzMy45MzUwNSBaIE0gNjAyLjc2MTAxLDEwMDguNzM2NCA3MDIuMDQ3MDksNzE2Ljk5NTY3IEggNTAzLjQ1OTc0IFogTSAxMjMzLjMyMzIsODYyLjM1NDE0IGMgNzQuNzMyLDAgMTM1LjEyMzcsLTY3LjU2NjYxIDEzNS4xMjM3LC0xNTEuNDk2ODIgMCwtNTkuMzU2MDIgLTMxLjc5MTQsLTExMC40ODMwNyAtNzcuNjk5MSwtMTM0LjIyNjI3IC0zNC41NzQ2LC0xLjE1NjQ5IC02OS41OTcyLC0yLjc0OTY3IC0xMDQuOTYzNywtNC44MTE1MSAtNTEuMTc4MywyMC42ODE2NCAtODcuNTc2NCw3My45MzI5MSAtODcuNTc2NCwxMzkuMDM3NzggMCw4NS45ODI0NiA2MC4zOTE3LDE1MS40OTY4MiAxMzUuMTE1NSwxNTEuNDk2ODIgeiBtIDg2NC4xMDM1LC0zMzMuMjQyMyBjIC0wLjA3MiwxLjkzNzA4IC0wLjE0MzksMy44NTg5NiAtMC4xNDM5LDUuNjcwNDcgdiAyNzQuMzQ1MzYgYyAwLDQxLjk2MzExIC0xNi4zNzE3LDE4MC4xNTc5MiAtMjMwLjMxNDEsMTgwLjE1NzkyIC0xMzguMTk0OCwwIC0yMzQuNDI0OCwtNzQuNzI3MSAtMjQ3LjczMzMsLTE3NS4wNDQxIGggMTM1LjEzMTYgYyAxMi4yODQ4LDQ1LjA0NDY4IDY3LjU2NTksNjYuNTM4OSAxMTUuNjgwOSw2Ni41Mzg5IDYwLjM4MzgsMCA5Ni4yMDYxLC0yMi41MjUxMyA5Ni4yMDYxLC01MC4xNTg1MSAwLC00Mi45ODg0MyAtNjIuNDM5MiwtNTMuMjM2MDcgLTE1MC40NzE1LC02MS40MjEwNyAtMTE2LjY4ODYsLTEwLjIzMTYzIC0yMTUuOTg5OSwtNDAuOTQyNTcgLTIxNS45ODk5LC0xNTkuNjg4MjEgMCwtMTEuNzkzNjMgMS4xODM3LC0yMi45MDAyNCAzLjI1NTIsLTMzLjQ1OTggLTMyLjM1MTMsMS4xNzE2OSAtNjUuNTUwNCwyLjAzMDY2IC05OS40ODUyLDIuNTMwNTIgViAxMjEwLjM5NDEgSCAxMzY4LjQ0NjkgViA5MjUuODE0NjYgYyAtNDAuOTQxLDM4LjkwMTU0IC05NC4xODI2LDYxLjQyMzQ3IC0xNTUuNTk4MSw2MS40MjM0NyAtMTM3LjE3MTEsMCAtMjUwLjc5NjQ2LC0xMTguNzM2MDQgLTI1MC43OTY0NiwtMjc1LjM2NTg4IDAsLTU4LjM0MDMgMTYuMDI3NywtMTExLjQ4MiA0My4yNDQyNiwtMTU1LjE0MjI1IC0zMy44ODY3NiwtMy43MTgyIC02Ny45MTc2NiwtNy45NjY2NiAtMTAyLjAyMDQ2LC0xMi43NDY5NyBMIDY4OC43Mzc4OCwxMTU5LjIwNzggSCA1MTcuNzkwMjkgTCAyNDUuMTYwNDYsMzc2LjcxNjA1IEMgMTY1LjUzNTQ5LDM0NS40MTE2OCA4OC45NDY0MTUsMzEwLjEzOTU3IDE2Ljc2MjcyMiwyNzAuNzEwMTggMzY2LjI0MDY3LDQyMy4xODc1NSAxMjI4LjAzNjcsNjI5LjYwMDc5IDIyMDMuNDU0MSw1MDMuMTUwODIgYyAtNC42NTQ2LDUuMTY5ODEgLTQyLjQ3NjUsMTUuMDI3MTYgLTEwNi4wMjc0LDI1Ljk2MTAyIj48L3BhdGg+CiAgICAgICAgICA8L2c+CiAgICAgICAgPC9nPgogICAgICA8L2c+CiAgICAgIDxnIGlkPSJzdmdfc3VidGV4dCIgZmlsbD0iIzExNDQ3MyIgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoMTU1LCAyMTUpIHNjYWxlKDAuNSkgIj48cGF0aCBkPSJNMTYgMEwxNi01MEw2LTUwTDYgMFpNNzEgMEw3MS01MEw2MS01MEw2MS0yN0M2MS0yMyA2MS0xNyA2MS0xMUw0MS01MEwyNy01MEwyNyAwTDM3IDBMMzctMjJDMzctMjcgMzctMzIgMzctMzhMNTcgMFpNOTktNTFDODktNTEgODEtNDUgODEtMzZDODEtMjcgODctMjMgOTctMjFDMTA1LTE5IDEwNy0xNyAxMDctMTNDMTA3LTkgMTA0LTYgOTktNkM5NC02IDkwLTkgODktMTVMNzktMTVDODAtNCA4NyAxIDk5IDFDMTA5IDEgMTE4LTQgMTE4LTE0QzExOC0yMiAxMTMtMjcgMTAyLTI5Qzk0LTMxIDkxLTMzIDkxLTM3QzkxLTQxIDk0LTQzIDk4LTQzQzEwMy00MyAxMDYtNDAgMTA3LTM1TDExNy0zNUMxMTYtNDQgMTExLTUxIDk5LTUxWk0xMzYgMEwxMzYtNTBMMTI2LTUwTDEyNiAwWk0xNzAtNDlDMTY4LTUwIDE2Ny01MCAxNjQtNTBMMTQ3LTUwTDE0NyAwTDE2NCAwQzE2NyAwIDE2OCAwIDE3MC0wQzE4Mi0yIDE5MS0xMCAxOTEtMjVDMTkxLTQxIDE4Mi00OCAxNzAtNDlaTTE2OC04QzE2Ni04IDE2NS04IDE2My04TDE1Ny04TDE1Ny00MkwxNjMtNDJDMTY1LTQyIDE2Ny00MiAxNjgtNDJDMTc2LTQxIDE4MC0zNSAxODAtMjVDMTgwLTE1IDE3NS05IDE2OC04Wk0yMzUtOEwyMTAtOEwyMTAtMjFMMjMzLTIxTDIzMy0yOUwyMTAtMjlMMjEwLTQyTDIzNS00MkwyMzUtNTBMMjAwLTUwTDIwMCAwTDIzNSAwWiI+PC9wYXRoPjwvZz4KICAgIDwvc3ZnPgogIA==)]

![](docs/tests_results/Linux/badge.svg) ![](docs/tests_results/Windows/badge.svg) ![](docs/tests_results/Darwin/badge.svg)

# `bbt` README <!-- omit from toc -->

- [Overview](#overview)
  - [What does the behavior description look like?](#what-does-the-behavior-description-look-like)
  - [Partial parsing](#partial-parsing)
  - [Step arguments](#step-arguments)
  - [One more example](#one-more-example)
- [Installation](#installation)
  - [Stable version](#stable-version)
  - [Latest version](#latest-version)
- [First use](#first-use)
- [Why should I use bbt?](#why-should-i-use-bbt)
  - [Write once](#write-once)
  - [Write a real documentation, not just a ".feature" Gherkin file](#write-a-real-documentation-not-just-a-feature-gherkin-file)
  - [Be proficient in no time](#be-proficient-in-no-time)
  - [No-fuss no-wait run](#no-fuss-no-wait-run)
  - [Ready to publish output](#ready-to-publish-output)
- [Status of the project](#status-of-the-project)
- [Help and comments](#help-and-comments)
- [Further reading](#further-reading)

## Overview

bbt is a simple tool to black box check the behavior of an executable through [Command Line Interface (CLI)](https://en.wikipedia.org/wiki/Command-line_interface).  
Hence the name: bbt stands for *Black Box Tester*.  

bbt targets both *specification of the behavior* and *end-to-end test automation* for the very common case of apps taking some input and producing some output.  
It enable developers **to write and execute comprehensive test scenarios in just a few minutes**. 

The outstanding feature of btt is that **it directly uses your behavior documentation in plain english**.  
There is no script nor other file to write.

### What does the behavior description look like?

The behavior is described in almost natural English, in Markdown, using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) / [Gherkin](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language) usual pattern *Given / When / Then*.  
Here is a minimal example:  

```md
### Scenario: I want to know gcc version

- When I run `gcc --version`
- Then the output contains `14.2.0`
```  
resulting in:

### Scenario: I want to know gcc version  <!-- omit from toc -->

- When I run `gcc --version`
- Then the output contains `14.2.0`

bbt being about documentation and simplicity, Markdown[^1] is a perfect fit.  

Let's consider a slightly more complete example:
![simple example](docs/rpl_example.png)

(Markdown source [here](docs/examples/rpl_case_insensitivity.md))

We have:

1. An "Overview" header and a first text   
   All this is ignored, because: 
   - bbt processes **only** Gherkin headers *# Features*, *# Background*, and *# Scenario* or *# Example*.   
   - bbt considers all text lines as comment, except Step lines.  
   
   bbt staying out of the way, you're free to use markdown almost without constraints to draft nice documentations. 
   
1. A "Feature" and a "Scenario" header (followed by the feature/scenario name)  
   bbt is now awake, and waiting for step lines.  
   Note that header's level is ignored (*#### Scenario*, is equal to *# Scenario* for bbt), you're free to structure the file as you want. 

2. Steps  
   Steps are lines starting with *- Given*, *- When*, *- Then*, *- And*, *- But*, that contain the things to check or do.

### Partial parsing 

A distinctive feature of bbt is that it seems to directly understand those almost normal English sentences like:  
```
- When I run `sut --quiet input.txt`
- Then there is no output
```
This is achieved thanks to a [partial parser](https://devopedia.org/natural-language-parsing). It means that there is no rigid grammar, because bbt takes into account only some keywords to recognize the skeleton of the sentence.  

So when you write:  
> - Then I should get `version 15.0.0` (Fix #2398 and #2402)    

bbt actually reads:  
> - then get `version 15.0.0`     
  
And this is what gives the ability to write steps in almost natural language. 

### Step arguments

Step's argument may be strings or multiline text.

As per [MDG](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin), bbt uses :
- multiline text (expected output, file content, etc) : [fenced code blocks](https://spec.commonmark.org/0.31.2/#fenced-code-blocks), that is a text between two "```" lines
- strings (file name, command to run, etc.) : [code span](https://spec.commonmark.org/0.31.2/#code-spans), that is a string between backticks  

It's not only to nicely highlight inputs in the doc, but also because otherwise the analysis of the steps would be too complex.  
 
### One more example

[This example](docs/examples/gcc_hello_word.md) shows how simple it is to run a `gcc` sanity test, that compiles and runs the ubiquitous *Hello Word*.

## Installation

### Stable version

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) is available on Windows, Linux and Darwin thanks to the Alire package manager:

1. Go to [Alire home](https://alire.ada.dev/) for a “one click” install.  
   
2. Run :
   > alr install bbt

   The exe will be moved in ~/.alire/bin on Linux and Darwin, or in xxxx on Windows.  
   Alternatively, you may choose another installation directory with:
   > alr install --prefix=/path/to/installation bbt  

   If needed, ensure that the installation directory is in your PATH.

### Latest version

For Linux user, an AppImage of the latest version is available [here](https://github.com/LionelDraghi/bbt/releases).  
(Thanks to @mgrojo and [Alr2AppImage](https://github.com/mgrojo/alr2appimage)).  
Download the AppImage, and:  
```sh
chmod +x bbt-0.1.0-x86_64.AppImage
ln -s bbt-0.1.0-x86_64.AppImage bbt
```

Or, to build the latest version on Windows, Darwin or Linux:
```sh
git clone https://github.com/LionelDraghi/bbt  
cd bbt  
alr build 
```

## First use

The easiest way to start is illustrated by Simon in it's [ada_caser](https://github.com/simonjwright/ada_caser/tree/main) project.  
He just created a scenario file called `tests.md`, and put in the README a reference to that file and the command line to run the tests.  
Thats'it, he didn't even need to create a "tests" directory.

## Why should I use bbt?

### Write once

Specification is the only source of truth. This is bbt most interesting feature, there is nothing else: no intermediate representation, no glue code, no scripting language, no duplication of the original source at all.  

With two main consequences: 
1. writing a test is a matter of minutes,
2. there is no more place for a discrepancy between documentation and tests.

Alternative tools exist, some are mentioned in [my quick overview of some comparable tools](docs/comparables.md).  
But as far as I know, **bbt is the only one to provide such a direct "run the doc" approach**.

### Write a real documentation, not just a ".feature" Gherkin file  

bbt effectiveness does not come at the cost of limiting documentation readability or expressiveness: 

- First, the vast majority of the file is just plain markdown : use it, structure it the way you like, give as much context as you want, and use all Markdown cool extensions (for example graphics with [Mermaid](https://mermaid.js.org/intro/));
- Second, even the part that is interpreted by bbt, the steps, is written in readable English thanks to the partial parsing.

Nice consequence, bbt scenarios may be written by non coders people.

### Be proficient in no time

bbt Steps uses a limited English subset, with a vocabulary dedicated to test with no-surprise keywords like *run*, *output*, *contains*, etc.  

Although simple, you don't have to learn this subset by heart, you may :
- ask for a template scenario by running `bbt create_template` (short form `bbt ct`), or
- ask for the complete grammar with `bbt list_grammar` (short form `bbt lg`).  

### No-fuss no-wait run

To run a scenario : `bbt my_scenario.md`  
To run all the md files in the *tests* tree `bbt -r tests`  
To run only a selection `bbt --select "Sanity check" tests`  

bbt has no dependencies on external lib or tools (diff, for example) and can be run as is on major native platforms.  

### Ready to publish output 

bbt output is in Markdown format. You can adjust the detail level with the usual "-q" and "-v" options.

The output cross-references the executed scenario files: if a test fails, just click on the link and you are in the scenario.  
You can push it on GitHub without further processing.  

To see what it looks like, consider [bbt own tests](docs/tests_results/Linux/features_results.md).  

Test results are generated when running `bbt`, by just using the `-o` option (`--output`).
  
## Status of the project

bbt is in an early stage, meaning that interface and behavior are subject to changes.  
Feel free to make features suggestions [in bbt discussions](https://github.com/LionelDraghi/bbt/discussions). 

The code has grown fast in 2024, and is far from being clean.  
Nevertheless, bbt is working, and has as a serious [test base](docs/tests_results/Linux/features_results.md).  

A very conclusive test on the effectiveness of bbt as being conducted on [the day 4 of Advent of Code 2024's challenges](https://github.com/LionelDraghi/Advent_of_code_2024/blob/main/day_04_tests.md).   
Tests where easy and fast to setup, allowing to stay most of the time focus on coding.

In real life, the [acc](https://github.com/LionelDraghi/ArchiCheck) project has largely migrated to BBT, resulting in a drastically reduced number of files and a significant gain in maintenability and readability of the tests.  
Other people are using it too.  

btt compile on Linux, Windows and Mac OS, and the test suite is run on the three platforms.  
On MacOS, it may be useful to set the environment variable GNAT_FILE_NAME_CASE_SENSITIVE to 1, cf. discussion [here](https://forum.ada-lang.io/t/name-file-casing-error-on-darwin/1795) to avoid small glitches on file names.  

## Help and comments
Comments are welcome [here](https://github.com/LionelDraghi/bbt/discussions)

## Further reading
- [User Guide](docs/UG.md): concepts, command, features...
- [Developer Guide](docs/developer_guide.md): design overview, issues, fixme...
- [References](docs/references.md): syntax, grammar, and more details on non obvious behavior
- [Project status](docs/project.md): changelog, tests, TDL...
- [Command line help](docs/bbt_help.md)

[^1]: More precisely, this is a subset of the existing [Markdown with Gherkin (MDG)](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) format.  

