[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) [![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html)


# `bbt` README <!-- omit from toc -->

- [Overview](#overview)
  - [A domain specific language? yes, english! :-)](#a-domain-specific-language-yes-english--)
  - [First example](#first-example)
  - [Hello Word example](#hello-word-example)
- [Main characteristics](#main-characteristics)
  - [Specification is the only source of truth](#specification-is-the-only-source-of-truth)
  - [Tests are easy to write](#tests-are-easy-to-write)
  - [Tests are easy to run](#tests-are-easy-to-run)
  - [Test Results are immediately publishable](#test-results-are-immediately-publishable)
- [Objective of the project](#objective-of-the-project)
- [Status of the project](#status-of-the-project)
- [Limitations](#limitations)
- [Installation](#installation)
- [Further reading](#further-reading)

## Overview

bbt is a simple tool to black box check the behavior of an executable (hence the name, bbt stands for *Black Box Tester*).  
**The beauty of btt is that it directly uses your behavior documentation as a the test script.**

### A domain specific language? yes, english! :-)

The behavior is described in almost natural english using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) usual pattern *Given / When / Then*, and sentences like "when I run `this command`, then I get no error, and the file `foo.ini` is created".  

A distinctive feature of bbt is that it directly understand those sentences. You dont have to learn a specific DSL syntax, nor to use a scripting language.  
This is achieved thanks to a [partial parser](https://devopedia.org/natural-language-parsing). It means that bbt take into account only some keywords to recognize the skeleton of the sentence, and is not going to fail because of an unexpected word.  

As an example, bbt will consider equivalent :  
- *then I get no error (close #2398)*
- *then I no more get this stupid error that was reported and closed already twice in issues #2398 and #2402 (mea culpa)*
- *then get no error*  
because those four words are the only it actually take into account.  

That feature gives a lot of freedom when writing scenarios. 

### First example 

```md
## Example:

In order to report a bug  
I need to get the version of the exe

### Scenario: I want to know uut version

- When I run `uut --version`
- Then the output contains `version 1.0`
```
Here we have:
1. Some description
   You can use almost all markdown nice features, including extensions, this is mostly ignored by bbt.
   Exceptions are in fact in the two following bullets.
   
2. A "scenario" header that starts a steps sequence
   Titles starting with some Gherkin keywords will wake up btt : *# Scenario*, *# Features*, *# Background*  
   The header level is ignored (*#### Scenario*, is equal to *# Scenario* for bbt), you're free to structure the file as you want. 

3. Steps
   Steps are line starting with *- Given*, *- When*, *- Then*, *- And*, *- But*, that contains the things to check or do.
   Note that bbt is case insensitive, and that other [Markdown bullet list marker](https://spec.commonmark.org/0.31.2/#bullet-list-marker) ('*' or '+') are not considered as steps, and can be used for comments.

This format is a subset of the existing [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin). 

### Hello Word example

The following [example](docs/tests/examples/hello_word.md) shows how simple it is to run a `gcc` sanity test, that compile and run an *Hello Word*.

Note that bbt is fully tested with bbt since 0.0.4 version.  
bbt own tests are based on feature descriptions available [here](docs/tests/features_results.md).

## Main characteristics

### Specification is the only source of truth

bbt most interesting feature is that the above scenario (that is *specification*) is directly executable : there is no intermediate representation, no more or less complete translation into code, no use of a shell"ish" language, no duplication of the original source at all.  

Just simple and readable English sentences, that may be written by non-coders.  

As bbt is reading only specifics line in the specification, the rest of the file is yours : you can give as much context as you want, using all Markdown (and Markdown extensions) possibilities, including graphics (Give a try to [Mermaid](https://mermaid.js.org/intro/)).

Alternative tools exists, refer to [my quick overview of some comparable tools](docs/comparables.md), but as far as i know, **bbt is the only one to provide such a direct "run the doc" approach**.

### Tests are easy to write

bbt uses a limited english subset, with a vocabulary dedicated to test with keywords like *run*, *output*, *contains*, etc.

Although simple, you don't have to learn this language by heart, you may ask for a template by running `bbt -ct` (or --create_template), and ask for the complete grammar with `bbt -lg` (or --list_grammar).

### Tests are easy to run

To run a scenario : `bbt my_scenario.md`  
Or to run all the md files in the *tests* tree `bbt -r tests`  

bbt as no dependencies on external lib or tools (diff, for example), to ensure that it can be run on all platforms without further complications.  

### Test Results are immediately publishable 

**Tests results** are also in Markdown files, and cross-references the matching scenario files : if a test fail, just click on the link and you are in the scenario.  
You can push on github without further processing.  
To see what it looks like, there is an example in [bbt own tests](docs/tests/features_results.md).  

Tests results are generated when running `bbt`, by just using the `-o` option (--output).

## Objective of the project 

bbt project aim at exploring how far we can push the "specification in natural language is the single source of truth" motto, while maintaining the main feature : ease of use. 

**If a newbie is able to use btt in a quarter of an hour, and an experienced user is able to write and run a test in less than 3 minutes, with no need to rewrite or post-process the generated documentation, I'll consider it as a great success.**    

bbt has a precise scope: it is dedicated to in line command, taking some input and producing some output.
Keep in mind that it is not meant for specifying or testing UI, complex systems, unit testing, API, etc.  
Using a natural language description for those kind of tests is a very tempting way to explore, but implementation may be an order of magnitude more complex than for bbt current scope.   

## Status of the project

As of version 0.0.x, bbt is in an early stage, meaning that his behavior is subject to changes.  
Feel free to make suggestions [in bbt discussions](https://github.com/LionelDraghi/bbt/discussions). 

The code has grown fast in the first three months, and is far from being clean.  
And there is yet no design description. 

Nevertheless, bbt is working. It has as a serious [test base](docs/tests/features_results.md).  
In real life, the [acc](https://github.com/LionelDraghi/ArchiCheck) project started the migration of its large tests base to bbt.  

## Limitations

btt compile on Windows and Mac OS, but is currently tested only on my Linux amd64 platform.

## Installation

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) is available thanks to the Alire package manager.  
1. To install Alire on your platform, refer to [Alire](https://alire.ada.dev/)  
   
2. Then to install bbt :
    > alr get bbt  
    > cd bbt  
    > alr run  

4. Move the bbt exec somewhere in your PATH

## Further reading
- [User Guide](docs/UG.md): concepts, command, line, features...
- [References](docs/references.md) : syntax, grammar, and more details on non obvious behavior
- [Project status](docs/project.md): changelog, tests, TDL...

