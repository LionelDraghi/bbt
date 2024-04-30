# `bbt` README

`bbt` is a simple tool to black box check the behavior of an executable (hence the name, `bbt` stands for `Black Box Tester`).  
The expected behavior is described using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) *Given* / *When* / *Then* usual pattern, in a simple Markdown format. 

It can be as simple as :
```md
## Scenario : Command line version option

- When I run `uut --version`
- Then the output contains `version 1.0`
```

Here is the magic : `bbt` understand some of the words in the step description (that is words afters `When`, `Then`, etc.). It has it's own simple DSL, and interpret `run`, `output`, `contains`, etc. 

`bbt` as not dependencies on external lib or tools (diff tools, for example), and therefore is platform independent. You can execute it transparently on Windows, Linux, etc.  
*Describe behavior once, verify everywhere!*

And because it is dedicated to black box testing, it is completely programming language independent.

## Basic Concepts

Basic concepts of `bbt` file are illustrated in the previous example :

1. **the BDD usual keywords** : `Scenario`, `When`, `Then`, etc.  
   `bbt` use a subset of the [Gerkhin language](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language).  
   For the Markdown format, `bbd` uses (a subset of) [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) format.

2. [**`bbt` keywords**](#Keywords) : `run`, `output`, `contains`, etc.  
  This is where `bbt` magic is, it as it's own DSL.
  
3. **glue word** : *I*, *the*
  Glue word are ignored by `bbt`. Their only aim is to give users a more flexible way to write. This is an important `bbt` feature, not to be rigid like a compiler, as long as this is not creating ambiguity.
   
4. [**code span** (in Markdown parlance)](https://spec.commonmark.org/0.31.2/#code-spans), that is text surrounded by backticks : `uut --version`, `version 1.0`  
  `bbt` uses code span to express a command, a file name or some expected out.

1. [**Fenced code block** (in Markdown parlance)](https://spec.commonmark.org/0.31.2/#fenced-code-blocks), that is lines between ``` or ~~~  
Fenced code block are used to specify multiline output or file content, as in 

~~~md
## [Scenario]: : Command line help

- When I run `uut -h`
- Then the output is
```
uut [options] [-I directory]
options :
-h : help
-r : recurse
```
~~~


## Keywords 

### [Gerkhin language subset](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language)
- Feature
- Scenario or Example
- Given
- When
- Then

### `bbt` own DSL 
- file
- no
- running
- created
- contains
- ignoring blanks
- returned
- output
- error
- is / should be
- diff
- environment

## Cookbook 

- **When** I **run** `cmd`  
  Return *success* if `cmd` was run
- **then** I **get** **no** **error**  
  Return *success* if `cmd` returned no error code 
- **then** I **get** **error**  
  Return *success** if `cmd` returned an error code 
- **When** I **successfully** **run** `cmd`  
  Return *success* if both `cmd` was run and returned no error  
  (This is a shortcut to avoid the usual line **Then** I **get** **no** **error**)
- **then** **output** **contains** `text`  
  Return *success* if `cmd` output contains `text` 
- **then** **output** **is** `text`  
  Return *success* if `cmd` output is exactly `text`  
- **then** I **get** `text`  
  is a synonym of **then** **output** **is** `text`  
- **then** **error output** **contains** `text`  
  Return *success* if `cmd` error output contains `text` 
- **then** **error output** **is** `text`  
  Return *success* if `cmd` error output is only `text`
- **then** `file` **contains** `text`  
  Return *success* if `file` contains `text` 
- **then** `file` **is** `text`  
  Return *success* if `file` contains only `text`

## TDL

- file
- no
- running
- created
- contains
- ignoring blanks
- returned
- output
- error
- is / should be
- diff
- environment

> [!NOTE] 
> `bbt` objective is command line tools testing.  
> For GUI testing, or complex configurations, move on! (and don't expect to find something as simple as `bbt`) :-)

