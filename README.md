# *bbt* README <!-- omit from toc -->

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg?style=flat-square)](https://opensource.org/licenses/Apache-2.0)

[![image](https://img.shields.io/badge/-inside-blue?logo=ada&logoColor=white&labelColor=grey&logoSize=auto&style=flat-square)](https://ada-lang.io/)
[![Awarded](https://img.shields.io/badge/Ada_Crate_of_the_Year-2024-blue?style=flat-square)](https://blog.adacore.com/ada-spark-crate-of-the-year-2024-winners-announced) 

[![](docs/tests_results/Linux/badge.svg)](docs/tests_results/Linux/features_results.md) 
[![](docs/tests_results/Darwin/badge.svg)](docs/tests_results/Darwin/features_results.md)
[![](docs/tests_results/Windows/badge.svg)](docs/tests_results/Windows/features_results.md) 

---
- [Overview](#overview)
  - [What does the behavior description look like?](#what-does-the-behavior-description-look-like)
  - [Partial parsing](#partial-parsing)
- [Installation](#installation)
  - [Stable version](#stable-version)
  - [Latest version](#latest-version)
    - [Building from sources](#building-from-sources)
    - [AppImage (Linux only)](#appimage-linux-only)
- [First use](#first-use)
  - [Going further](#going-further)
- [Help and comments](#help-and-comments)
- [Further reading](#further-reading)

## Overview

*bbt* is a simple tool for black box check the behavior of an executable through the [Command Line Interface (CLI)](https://en.wikipedia.org/wiki/Command-line_interface).  
Hence the name: *bbt* stands for *Black Box Tester*.  

*bbt* targets both *behavior specification* and *end-to-end test automation* for the very common case of apps taking some input and producing some output.  
It enables developers **to write and execute comprehensive test scenarios in just a few minutes**. 

The standout feature of *btt* is that **it directly uses your documentation in plain english**.  
There is no script or other file to write.

*bbt* does not care about the type of document: call it Acceptance test, feature or behavior description, test scenario, README file or user guide, that's the same.  
*bbt* makes no distinction between running a scenario in a test file and checking an example in a README, as long as it recognizes a behavior description.

### What does the behavior description look like?

Here is a minimal example:  

```md
### Scenario: I want to know the gcc version

- When I run `gcc --version`
- Then the output contains `14.2.0`
```  

The behavior is described in almost natural English, using the usual [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) / [Gherkin](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language) pattern: *Given / When / Then*.  

It's in Markdown [^1], so that the text above render as:

---
### Scenario: I want to know gcc version  <!-- omit from toc -->

- When I run `gcc --version`
- Then the output contains `14.2.0`
---

*bbt* being about documentation and simplicity, Markdown is a perfect fit.  

Let's consider a slightly more complete example:
![simple example](docs/rpl_example.png)

(Markdown source [here](docs/examples/rpl_case_insensitivity.md))

We have:
  
1. **A *Feature* and a *Scenario* header** (followed by the feature/scenario name)  

   *bbt* processes **only** headers starting with Gherkin keywords:
   - *# Features* 
   - *# Background* 
   - *# Scenario* or *# Example*  
  
   In this example, the *Overview* Header is ignored.  
   Note also that the header's level doesn't matter (*#### Scenario*, is equal to *# Scenario* for *bbt*), so that you're free to structure the file as you want. 

2. **Steps**  

   Within Scenarios, *bbt* reads Steps—that is lines starting with:
   - *- Given*
   - *- When* 
   - *- Then* 
   - *- And* 
   - *- But*  
  
   Those lines contains the things to check or do.  
   Note that the only possible list marker for Steps is `-`, so that other list markers like '*' or '+' may be used for comments and will be ignored by *bbt*.

3. **Step arguments**

   Within or after step lines, a Step's argument may be:
   - strings for file name, command to run, etc. (for example here `config.ini`) 
   - or multiline text for expected output, file content, etc. (for example here the config.ini file content).

   As per [MDG](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin), strings uses Markdown [code span](https://spec.commonmark.org/0.31.2/#code-spans) (that is a string between backticks), and multiline text uses [fenced code blocks](https://spec.commonmark.org/0.31.2/#fenced-code-blocks) (that is a text between two `` ``` `` lines).  

Everything else in the file is ignored. *bbt* stays out of your way, so you are free to use Markdown almost without constraints to draft nice documentations.  

And in any case, to check what is really taken into account by *bbt*, just use:
```
bbt explain my_scenario.md
```   
  
### Partial parsing 

A distinctive feature of *bbt* is that it appears to directly understand those almost natural English sentences like:  
```
- When I run `sut --quiet input.txt`
- Then there is no output
```
This is achieved using a [partial parser](https://devopedia.org/natural-language-parsing). The parser ignores everything that is not relevant to it and only looks for specific keywords to recognize the skeleton of the sentence.  

When you write:  
> - Then I should get `version 15.0.0` (Fix #2398 and #2402)    

*bbt* only sees two keywords and a parameter:  
> - **Then** ~~I should~~ **get** **`version 15.0.0`** ~~(Fix #2398 and #2402)~~    

As a result, the writer enjoys a lot of flexibility and is not constrained by a rigid grammar as with a scripting language, enabling steps to be written in almost natural language.

## Installation

### Stable version

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) is available on Windows, Linux and Darwin thanks to the Alire package manager:

1. Install [![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/alire-badge.json)](https://alire.ada.dev/)

2. Install *bbt* :
   
   ```sh
   alr install bbt
   ```
   The executable will be in ~/.alire/bin.  
   
   Alternatively, you may choose another installation directory with:
   ```sh
   alr install --prefix=/path/to/installation bbt  
   ```

   Ensure that the installation directory is in your PATH.

### Latest version

#### Building from sources
```sh
git clone https://github.com/LionelDraghi/bbt  
cd bbt  
alr build 
```

#### AppImage (Linux only)
Download the AppImage [here](https://github.com/LionelDraghi/bbt/releases), and:  
```sh
chmod +x bbt-0.3.0-dev-x86_64.AppImage
ln -s bbt-0.3.0-dev-x86_64.AppImage bbt
```

(Thanks to @mgrojo and [Alr2AppImage](https://github.com/mgrojo/alr2appimage)).  

## First use

To get started, you can either draw inspiration from one of the [examples](https://github.com/LionelDraghi/bbt/tree/main/docs/examples), or have *bbt* generate a sample scenario for you :
```
bbt help example > my_scen.md
```

A short but comprehensive tutorial can be generated with 
```
bbt help tutorial 
```

### Going further 
- A very short article on the project "Why": [My dream way of testing](https://dev.to/lioneldraghi/my-dream-way-of-testing-8m9).
- The first adopter is Raffle, an Ada compiler with a LLVM backend, not yet public, by Paul Jarret
- [CoAP-SPARK]( https://github.com/mgrojo/coap_spark), by Manuel Gomez 
- [ada-caser]( https://github.com/simonjwright/ada_caser/tree/main), by Simon Wright
- [GRBL Parser]( https://github.com/RREE/grbl_parser_ada), by Rolf Ebert

Nevertheless, *bbt* is still young, meaning that :
- your features suggestions are welcomed [in *bbt* discussions](https://github.com/LionelDraghi/bbt/discussions);
- new features are added regularly: latest updates can be found in the [Changelog](docs/changelog.md);
- it is subject to interface and behavior changes, keep an eyes on the changelog before updating.   

*btt* is available and tested on Linux, Windows and Mac OS.  
On MacOS, you may need to set the environment variable GNAT_FILE_NAME_CASE_SENSITIVE to 1, cf. discussion [here](https://forum.ada-lang.io/t/name-file-casing-error-on-darwin/1795) to avoid small glitches on file names.  

## Help and comments
Comments and questions are welcome [here](https://github.com/LionelDraghi/bbt/discussions)

## Further reading
- [User Guide](docs/UG.md): concepts, commands, features...
- [References](docs/references.md): syntax, grammar, and more details on non obvious behavior
  
- [Project status](docs/project.md): changelog, tests, TDL...
- [Developer Guide](docs/developer_guide.md): design overview, issues, fixme...

- [bbt on the web](docs/bbt_on_the_net.md)
- [Why should I use bbt](docs/why_should_i_use_bbt.md)

[^1]: More precisely, *bbt* complies (mostly) with [Markdown with Gherkin (MDG)](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin), a convention to embed Gherkin scenarios in GitHub Flavored Markdown files. 
