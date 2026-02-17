# `bbt` README <!-- omit from toc -->

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg?style=flat-square)](https://opensource.org/licenses/Apache-2.0)

[![image](https://img.shields.io/badge/-inside-blue?logo=ada&logoColor=white&labelColor=grey&logoSize=auto&style=flat-square)](https://ada-lang.io/)
[![Awarded](https://img.shields.io/badge/Ada_Crate_of_the_Year-2024-blue?style=flat-square)](https://blog.adacore.com/ada-spark-crate-of-the-year-2024-winners-announced) 

![](docs/tests_results/Linux/badge.svg) ![](docs/tests_results/Windows/badge.svg) ![](docs/tests_results/Darwin/badge.svg)

---
- [Overview](#overview)
  - [What does the behavior description look like?](#what-does-the-behavior-description-look-like)
  - [Partial parsing](#partial-parsing)
  - [One more example](#one-more-example)
- [Installation](#installation)
  - [Stable version](#stable-version)
  - [Latest version](#latest-version)
    - [building from sources](#building-from-sources)
    - [AppImage (Linux only)](#appimage-linux-only)
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

The outstanding feature of btt is that **it directly uses your documentation in plain english**.  
There is no script nor other file to write.

bbt doesn't care about the type of document: call it Acceptance test, feature or behavior description, test scenario, README file or user guide, that's the same.  
bbt make no difference between running a scenario in a test file and checking an example given in a README file, as long as it recognizes a behavior description in it.

### What does the behavior description look like?

Here is a minimal example:  

```md
### Scenario: I want to know gcc version

- When I run `gcc --version`
- Then the output contains `14.2.0`
```  

The behavior is described in almost natural English, using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) / [Gherkin](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language) usual pattern *Given / When / Then*.  

It's in Markdown [^1], so that the text above render as:

---
### Scenario: I want to know gcc version  <!-- omit from toc -->

- When I run `gcc --version`
- Then the output contains `14.2.0`
---

bbt being about documentation and simplicity, Markdown is a perfect fit.  

Let's consider a slightly more complete example:
![simple example](docs/rpl_example.png)

(Markdown source [here](docs/examples/rpl_case_insensitivity.md))

We have:
  
1. **A *Feature* and a *Scenario* header** (followed by the feature/scenario name)  

   bbt processes **only** headers starting with Gherkin keywords:
   - *# Features* 
   - *# Background* 
   - *# Scenario* or *# Example*  
  
   In this example, the *Overview* Header is ignored.  
   Note also that the header's level doesn't matter (*#### Scenario*, is equal to *# Scenario* for bbt), so that you're free to structure the file as you want. 

2. **Steps**  

   Within Scenarios, bbt reads Steps, that is lines starting with:
   - *- Given*
   - *- When* 
   - *- Then* 
   - *- And* 
   - *- But*  
  
   Those lines contains the things to check or do.  
   Note that the only possible list marker for Steps is `-`, so that other list markers like '*' or '+' may be used for comments and will be ignored by bbt.

3. **Step arguments**

   Within or after step lines, Step's argument may be:
   - strings for file name, command to run, etc. (for example here `config.ini`) 
   - or multiline text for expected output, file content, etc. (for example here the config.ini file content).

   As per [MDG](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin), strings uses Markdown [code span](https://spec.commonmark.org/0.31.2/#code-spans) (that is a string between backticks), and multiline text uses [fenced code blocks](https://spec.commonmark.org/0.31.2/#fenced-code-blocks) (that is a text between two `` ``` `` lines).  

Everything else in the file is ignored. bbt staying out of the way, you're free to use markdown almost without constraints to draft nice documentations. 
  
### Partial parsing 

A distinctive feature of bbt is that it seems to directly understand those almost normal English sentences like:  
```
- When I run `sut --quiet input.txt`
- Then there is no output
```
This is achieved thanks to a [partial parser](https://devopedia.org/natural-language-parsing). It means that there is no rigid grammar, because bbt takes into account only some keywords to recognize the skeleton of the sentence.  

When you write:  
> - Then I should get `version 15.0.0` (Fix #2398 and #2402)    

bbt only sees two keywords and one parameter:  
> - then get `version 15.0.0`     
  
And this is what gives the ability to write steps in almost natural language. 

### One more example

[This example](docs/examples/gcc_hello_word.md) shows how simple it is to run a `gcc` sanity test, that compiles and runs the ubiquitous *Hello Word*.

## Installation

### Stable version

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) is available on Windows, Linux and Darwin thanks to the Alire package manager:

1. Install [![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/alire-badge.json)](https://alire.ada.dev/)

2. Install bbt :
   
   ```sh
   alr install bbt
   ```
   The exe should be in ~/.alire/bin.  
   
   Alternatively, you may choose another installation directory with:
   ```sh
   alr install --prefix=/path/to/installation bbt  
   ```

   Ensure that the installation directory is in your PATH.

### Latest version

#### building from sources
```sh
git clone https://github.com/LionelDraghi/bbt  
cd bbt  
alr build 
```

#### AppImage (Linux only)
Download the AppImage [here](https://github.com/LionelDraghi/bbt/releases), and:  
```sh
chmod +x bbt-0.1.0-x86_64.AppImage
ln -s bbt-0.1.0-x86_64.AppImage bbt
```

(Thanks to @mgrojo and [Alr2AppImage](https://github.com/mgrojo/alr2appimage)).  

## First use

To get started, you can either draw inspiration from one of the [examples](https://github.com/LionelDraghi/bbt/tree/main/docs/examples), or have bbt generate a template for you :
```
bbt create_template
```
This will create a `bbt_template.md` file, which is both a template and a short tutorial.

Dont' forget to rename it, by default bbt recognize it's own template and doesn't run it, so that you can keep it with your scenarios.

A good example to start simple is given by Simon in it's [ada_caser](https://github.com/simonjwright/ada_caser/tree/main) project : there is just a scenarios file called `tests.md`.

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

bbt has no dependencies on external lib or tools (diff, for example), and is tested on Linux, Darwin and Windows.  

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
Nevertheless, bbt is working, and has a serious [test base](docs/tests_results/Linux/features_results.md).  

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

[^1]: More precisely, bbt comply (mostly) with [Markdown with Gherkin (MDG)](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin), a convention to embed Gherkin scenarios in GitHub Flavored Markdown files. 
