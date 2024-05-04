# `bbt` README

bbt is a simple tool to black box check the behavior of an executable (hence the name, bbt stands for *Black Box Tester*).  
The expected behavior is described using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) *Given* / *When* / *Then* usual pattern, in a simple Markdown format. 

It can be as simple as :
```md
## Scenario : Command line version option

- When I run `uut --version`
- Then the output contains `version 1.0`
```

Here is the magic : bbt understand some of the words in the step description (that is words afters *When*, *Then*, etc.). It has it's own simple DSL, and interpret *run*, *output*, *contains*, etc. 

bbt as no dependencies on external lib or tools (diff tools, for example), and therefore is platform independent. You can execute it transparently on Windows, Linux, etc.  
*Describe behavior once, verify everywhere!*

And because it is dedicated to black box testing, it is completely programming language independent.

## Basic Concepts

Basic concepts of bbt file are illustrated in the previous example :

1. **the BDD usual keywords** : `Scenario`, *When*, *Then*, etc.  
bbt use a subset of the [Gerkhin language](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language).  
For the Markdown format, *bbd* uses (a subset of) [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) format.

2. [**bbt keywords**](#Keywords) : *run*, *output*, *contains*, etc.  
This is where bbt magic is, it as it's own DSL.
  
3. **glue word** : *I*, *the*
Glue word are ignored by bbt. Their only aim is to give users a more flexible way to write. This is an important bbt feature, not to be rigid like a compiler, as long as this is not creating ambiguity.
   
4. [**code span** (in Markdown parlance)](https://spec.commonmark.org/0.31.2/#code-spans), that is text surrounded by backticks : `` `uut --version` ``, `` `version 1.0` ``  
bbt uses code span to express a command, a file name or some expected out.

5. [**Fenced code block** (in Markdown parlance)](https://spec.commonmark.org/0.31.2/#fenced-code-blocks), that is lines between ``` or ~~~  
Fenced code block are used to specify multiline output or file content, as in: 

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
- get
- existing
- no
- not
- dont
- error
- is
- output
- contains
- Successfully

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

    That's the same. 
   
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

- `` then output contains `text` ``  or `then error output ...`
  Return *success* if *cmd* output contains `text`, on standard or on error output. 

- `` then output is `text` ``  or `then error output ...`
  Return *success* if *cmd* output is only `text``, on standard or on error output.   

- `` then I get `text` ``  
  This is a synonym of the previous form ``then output is `text` ``  

- `` then `file` contains `text` ``  
  Return *success* if *file* contains `text`, but not necessarily only that text. 
  
- `` then `file` is `text` ``  
  Return *success* if *file* contains **only** `text`. 
  


## TDL

- given file doest not exists
- created
- ignoring blanks
- is / should be
- diff
- environment
- append 
  
> [!NOTE] 
> bbt objective is command line tools testing.  
> For GUI testing, or complex configurations, move on, 
and don't expect to find something as simple as bbt! :-)

