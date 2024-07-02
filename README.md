[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) [![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html)


# `bbt` README <!-- omit from toc -->

- [Overview](#overview)
- [Main characteristics](#main-characteristics)
  - [Specification is the test](#specification-is-the-test)
  - [Tests are easy to write](#tests-are-easy-to-write)
  - [Tests are easy to run](#tests-are-easy-to-run)
  - [Tests are Self documented](#tests-are-self-documented)
- [Objective of the project and limitations](#objective-of-the-project-and-limitations)
- [Installation](#installation)
- [Further reading](#further-reading)

## Overview

bbt is a simple tool to black box check the behavior of an executable (hence the name, bbt stands for *Black Box Tester*).  

It is dedicated to command line, taking some input and producing some output.

The expected behavior is described using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) usual pattern  
- **Given** an initial context 
- **When** some event occurs 
- **Then** there is some outcomes.   

It uses a Markdown format, compliant with the existing [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) proposal, and can be as simple as :
~~~md
### Scenario : I want to know uut version

- When I run `uut --version`
- Then the output contains `version 1.0`
~~~
A more complete example, that is a simple gcc sanity check.
~~~md
### Scenario: compiling and executing an hello word

- Given the new file `main.c`
```
#include <stdio.h>
int main() {
printf("Hello, World!");
return 0;
}

- And there is no `main` file

- When I run `/usr/bin/gcc main.c -o main`
- And I run `main`

- Then the output is `Hello, World!`
~~~


## Main characteristics

bbt is by nature limited : it aims at providing a **simple** solution to **simple common cases**.  
Limiting bbt ambition provides substantial benefits :

### Specification is the test

bbt most interesting feature is that the above scenario (that is *specification*) is directly executable : there is no intermediate code generation, no use of a shell"ish" language, no glue code, no configuration file.  

Just simple and readable English sentences, that may be written by non-coders.  

### Tests are easy to write

bbt understand a (very) limited english subset, with a vocabulary dedicated to test with keywords like *run*, *output*, *contains*, etc.

Although simple, you don't have to learn this language by heart, you may ask for a template file with :  
> bbt -ct (or --create_template)  

or ask for the complete grammar with :  
> bbt -lg (or --list_grammar)

### Tests are easy to run

To run the test :  
> bbt my_scenario.md

Or to run all the tests files in the `tests` tree :
> bbt -r tests

That's it : no cascading Makefile, no script.

bbt as no dependencies on external lib or tools (diff, for example), and aims at reducing uses of other tools, and platform dependency.  

*Describe behavior once, verify everywhere!*

### Tests are Self documented

As seen above, **tests scenarios** are already documented, using a simple Markdown format that will be nicely presented on github or wherever out of the box.  
And bbt is reading only specifics line in the file, meaning that the rest of the file is yours : you can give as much context as you want, using almost all Markdown possibilities (with very few limitations), and even Markdown extensions.  
If you haven't yet experienced how easy it is to create graphics with a simple text description using [Mermaid](https://mermaid.js.org/intro/), give it a try.

**Tests results** are generated when running `bbt`, by just using the `-o` option (--output). It's also a Markdown file.  
The tests results file mainly contains... the tests results :-).  
(It could be handy to add also a few information like the date or platform. Not sure this is is needed, but feel free to say what could fit for you [here](https://github.com/LionelDraghi/bbt/discussions)).  
Each result has a link to the matching scenario file : if a test fail, just click on the link and you are in the scenario.  
To see what it looks like, there is an example in [bbt own tests](docs/pass_tests.md).

## Objective of the project and limitations

bbt project aim at exploring how far we can push the "specification is the test" assertion, while maintaining the main feature : ease of use. 
> [!NOTE]
> If a newbie is able to use btt in a quarter of an hour, and an experienced user is able to write and run a test in less than 3 minutes, with no need to rewrite or post-process the generated documentation, I'll consider it as a great success.    

bbt is targeting command line programs, and aims at being an easy and obvious way to run 90% of the boring usual tests on simples input / output, command line error, environment variable, etc.  
It is not meant for :
- UI testing or Web interaction 
- Complex file system stuffs
- White box testing (obviously :-)), checking internal states, or extensive API testing.

It probably won't be the only test tool of your project, but that's highly dependant on the nature of your application.  
Alternative tools exists, with more or less different objectives (Refer to [My quick overview of some comparable tools](docs/comparable.md)).  

## Installation

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/list_image.json)](https://alire.ada.dev/crates/list_image.html) is available thanks to the Alire package manager.  
1. To install Alire on your platform, refer to [Alire](https://alire.ada.dev/)  
   
2. Then to install bbt :
    > cd bbt  
    > alr build  

3. Move the bbt exec somewhere in your PATH

## Further reading
- [User Guide](docs/UG.md) 
- [References](docs/references.md) 
- [Project objectives, design and status](docs/project.md)

