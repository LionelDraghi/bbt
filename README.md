<!-- omit from toc -->
# `bbt` README

 [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) [![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html)

bbt is a simple tool to black box check the behavior of an executable (hence the name, bbt stands for *Black Box Tester*).  
The expected behavior is described using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) *Given* / *When* / *Then* usual pattern, in a simple Markdown format. 

It can be as simple as :
```md
## Scenario : Command line version option

- When I run `uut --version`
- Then the output contains `version 1.0`
```

And, to run the test :  
`bbt my_test.md`

That's it.

Here is the magic : bbt understand some of the words in the step description (that is words afters *When*, *Then*, etc.). It has it's own simple [DSL](https://en.wikipedia.org/wiki/Domain-specific_language), and interpret *run*, *output*, *contains*, etc. 

bbt as no dependencies on external lib or tools (diff tools, for example), and therefore is platform independent. You can execute it transparently on Windows, Linux, etc.  
*Describe behavior once, verify everywhere!*

And because it is dedicated to black box testing, it stays simple and programming or scripting language independent.

---------------------------------------------------------------------

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Basic Concepts](#basic-concepts)
  - [Simplicity](#simplicity)
  - [Portability](#portability)
- [Syntax and Keywords](#syntax-and-keywords)
  - [Gerkhin language subset](#gerkhin-language-subset)
  - [bbt own DSL](#bbt-own-dsl)
- [Syntax](#syntax)
- [More advanced feature](#more-advanced-feature)
- [Background](#background)
- [Behavior](#behavior)
  - [Blank lines and Case sensitivity](#blank-lines-and-case-sensitivity)
  - [Execution](#execution)
- [Tips](#tips)
  - [Understanding what he doesn't understand](#understanding-what-he-doesnt-understand)
  - [Comments](#comments)
- [TDL](#tdl)
- [Help and comments](#help-and-comments)

---------------------------------------------------------------------

## Basic Concepts

Basic concepts of bbt files are illustrated in the previous example :

1. **the BDD usual keywords** : `Scenario`, *When*, *Then*, etc.  
bbt use a subset of the [Gerkhin language](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language), in the [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) format.

2. [**bbt keywords**](#Keywords) : *run*, *output*, *contains*, etc.  
This is where bbt magic stands.
  
3. **glue word** : *I*, *the*
Glue word are ignored by bbt. Their only aim is to give users a way to write more natural english. This semi-formal language is an important bbt feature. As long as the language remains simple, the risk of ambiguity is low. (Describing behavior is specifying, and you certainly don't want ambiguity when writing specifications).
   
4. [**code span** (in Markdown parlance)](https://spec.commonmark.org/0.31.2/#code-spans), that is text surrounded by backticks : `` `uut --version` ``, `` `version 1.0` ``  
bbt uses code span to express a command, a file name or some expected output.

5. [**Fenced code block** (in Markdown parlance)](https://spec.commonmark.org/0.31.2/#fenced-code-blocks), that is lines between ``` or ~~~  
Fenced code block are used to specify multiline output or file content, as in: 

    ~~~md
    ## Scenario: Command line help

    - When I run `uut -h`
    - Then the output is
    ```
    uut [options] [-I directory]
    options :
    -h : help
    -r : recurse
    ```
    ~~~

### Simplicity
An important feature is the low effort needed to have documented and executable specification and tests.
The markdown format of the scenarii is nicely rendered on most platform like github.  
bbt is not going to modify those files, for example to insert the test results.

Instead, a test report is output by the tools (currently on the standard output), with a md format, provided no option modifying the verbosity like `-v` is used.  

Just run :  
`bbt -r tests > report.md`

### Portability
Several tools dedicated to black box testing exists, but they all uses as their description language either a programming language, or some shell scripting.
This is a powerful way to go, and you may consider worthwhile tho invest time doing so. 

On the other hand, bbt aims at immediate use, with almost no learning curve.
One of the goal of bbt is to do 90% of the job that others do, with a simple and natural language, independent of external tools or interpreters. For this reason, bbt DSL provide ways to create or compare files, that is things usually done in more or less portable manner with makefiles.   

---------------------------------------------------------------------

## Syntax and Keywords 

### [Gerkhin language subset](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language)
- *Feature*
- *Scenario* or *Example*
- *Given*
- *When*
- *Then*
- *And* or *But*

### bbt own DSL 

bbt keywords, including both the Gerkhin subset and bbt specifics keywords (generated with `bbt -lk`):
- given
- when
- then
- and
- but
- run
- running
- get
- existing
- no
- not
- dont
- error
- is
- output
- contains
- successfully
- directory

## Syntax 

**1. Given**  

  *Given* is used to setup the run.

  - ``Given the file `file_name` ``  
    ~~~
    ```
    line 1
    line 2
    line 3
    ```
    ~~~
    Return *success* if bbt could create a file *file_name* containing the text in the code fenced lines.
    This command overwrite an existing *file*, and as this is the normal behavior, there is no error here.  
    If you want to add something to an existing file, wait, there's a ``When I append `text` to `file` `` in the TDL :-)

    Note that there is no other keyword than *Given* on this line.  
    You could write a minimalist  
    `` Given `file_name` ``  
    or  
    ``Given my favorite and so useful file `file_name` ``  

    That's the same. But don't do that, keep it simple. 
   
**2. When**  

  *When* has two main functions : checking that a file with some contents is available before run, and running  a command.

  - `` When I run `cmd` ``  
    Return *success* if *cmd* was run.  
    *failed* usually means that bbt couldn't find the cmd to run.

  - `` When I successfully run `cmd` ``  
    Return *success* if *cmd* was run **and** returned no error.  
  (This is a shortcut to avoid the usual following line `Then I get no error`).

  - `` When `file` is `text` ``  
    Return *success* if *file* contains **only** *text*  

  - `` when `file` contains `Text` ``  
    Return *success* if *file* contains *text*, but not necessarily only that text. 

  Note that both *is* and *contains* may be followed by a multiline text :
  ~~~
  When `file` contains 
  ```
    line 1
    line 2
    line 3
  ```
~~~

**3. Then**  

  Then express the check to be done. It's either the run return code, the run output or a file content. Like for the Given steps, 

- `then I get error` or `no error`  
  Return *success* if the previously run command returned an error or no error code. 

- `` then output contains `text` ``  or `then error output contains ...`  
  Return *success* if *cmd* output contains `text`, on standard or on error output. 

- `` then output is `text` ``  or `then error output is ...`  
  Return *success* if *cmd* output is only `text``, on standard or on error output.   

- `` then I get `text` ``  
  This is a synonym of ``then output is `text` ``  

- `` then `file` contains `text` ``  
  Return *success* if *file* contains `text`, but not necessarily only that text. 
  
- `` then `file` is `text` ``  
  Return *success* if *file* contains **only** `text`. 
  
---------------------------------------------------------------------

## More advanced feature

## Background
*bbt* supports a Background scenario, that is a special scenario that will be executed before the start of each following scenario.

```md
### Background :
  - Given there is no `config.ini` file

### Scenario : normal use cas
  - When I run `uut create config.ini` 
  - When I run `uut append "size=80x40" config.ini` 
  - Then `config.ini` should contains `"size=80x40"`

### Scenario : the last command does not meet expectation (test should fail)
  - When I run `uut -v` 
  - Then I should get no error
```

Note that in this case, the second scenario will fail because of the background (the first create a `config.ini` file), and would not fail without the background. 


Background may appears at the beginning, at document level, or at feature level, or both.
Before each scenario, the document background will be executed, and then the feature background.



---------------------------------------------------------------------

## Behavior

### Blank lines and Case sensitivity

bbt ignore blank lines and casing when comparing actual with expected output.
This is the expected behavior in most case, but not always. 
Refer to the TDL.

### Execution

bbt is executed when you launch the command. All output files will be created here, and input file are expected here. 

bbt scenarii are Markdown files. So if you don't specify the precise file names, bbt will try to execute all md file.  
To see what files, use `bbt --list_files`, possibly with `--recurse`.  
(or the short form `bbt -lf -r`).

But if you specify the files, even using wildcards, like in `bbt tests/robustness*`, then bbt will consider that you know what you do, maybe you have a different naming convention, and will try to run each of them. So that you can name your file `.bbt`, or `.gmd` as you which.

---------------------------------------------------------------------

## Tips

### Understanding what he doesn't understand

Error messages provided by the lexer are not bullet proof (and it is likely that no special effort will be put on improving error messages in the future...).

For example, if you forget backticks on dir1 in :  
``- Given the directory dir1 ``  
It wont tells you "didn't you forget to "code fence" dir1?".  
It will just says :  
`Unrecognized step "Given the directory dir1"`

A good reflex in such a case is to ask *bbt* what did he understand from the test file, thanks to the -e (--explain) option.  
It will tell you something like :  
`GIVEN_STEP, UNKNOWN, Text = "Given the directory dir1"`  
meaning that the only thing he was able to conclude is that it's "Given" step.  
On the the adjusted version :  
``GIVEN_STEP, FILE_CREATION, Text = "Given the directory `dir1`", File_Name = "dir1"``  
you see that the (internal) action field has changed from UNKNOWN to FILE_CREATION, and that the File_Name field has now a value.

### Comments

Because the lexer is not able to make a difference between bullet lines in steps definition or bullet lines in normal text, there is limitations on where you can 
use it.
- *bbt* will consider bullet line starting with `-` as comment before the first "scenario" header. Don't use it after that.
- *bbt* will also consider line starting with the other official bullet marker (`*`and`+`) as comment, and **not steps line marker**, so that you can use those markers where you want without interfering with the lexer.
  

---------------------------------------------------------------------

## TDL

**near future**
- given file doest not exists
- "should be" as "is" synonym?
  
**distant future or low priority**
- environment
- diff 
- append 
- implement "case insensitive" and "ignore blank lines" 
- clean function : bbt delete all files created during execution
- interactive exec
- explore the possibility to run multiple exe asynchronously, while staying simple.
  Maybe by using the AdaCore spawn lib.
    
---------------------------------------------------------------------

## Help and comments

Comments are welcome [here](https://github.com/LionelDraghi/bbt/discussions)

> [!NOTE] 
> *bbt* objective is command line tools testing.  
> For GUI testing, or complex configurations, there are other solutions.  
Just don't expect to find something as simple as bbt! :-)

