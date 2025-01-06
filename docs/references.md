
# References  <!-- omit from toc -->

- [Syntax](#syntax)
  - [Gherkin language subset](#gherkin-language-subset)
  - [bbt own DSL](#bbt-own-dsl)
- [Grammar](#grammar)
  - [References](#references)
- [Behavior](#behavior)
  - [Blank lines and Case sensitivity](#blank-lines-and-case-sensitivity)
  - [Execution](#execution)
  - [Scenario files format](#scenario-files-format)
    - [Steps](#steps)
    - [Headings](#headings)
    - [Fenced Code blocks](#fenced-code-blocks)

## Syntax

### Gherkin language subset

- *Feature*
- *Scenario* or *Example*
- *Background*
- *Given*
- *When*
- *Then*
- *And* or *But*

([Cf. Gherkin language reference](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language))

### bbt own DSL 

bbt keywords, including both the Gherkin subset and bbt specifics keywords may be obtained with `bbt -lk` (`--list_keywords`).  
But more interesting, the grammar can be obtained through the `-lg` (`--list_grammar`) option. 

Each Step is a one line sentence, with a basic "subject verb object" structure, starting with the preposition/adverb/conjunction (*Given*, *When*, *And*, etc.). 
Add some attribute adjectives (e.g. *empty*), and here we are.

Here is an excerpt from the grammar :
```
| When  |     |        | run              | `text` | RUN_CMD                 |
```
- First, the keywords : here, `When` and `run`. 
- Then, some text or file name, between backtick.
- And, at the end, the resulting action.

Note that all other tokens will be ignored.  
You can write
```
When run `my_command -r`
```
or 
```
When I once more try to run something like `my_command -r`
```
that's the same.  

Note : neither is recommended.  
The former isn't easy to read, and the latter is misleading by introducing nuances that bbt ignore!  
This is specification, make short sentences and go straight to the point.

## Grammar 

**1. Given**  

  *Given* is used to check or create initial conditions.

  - ``Given the directory `dir1` ``  
  - ``Given the file `file_name` ``  
    ~~~
    ```
    line 1
    line 2
    line 3
    ```
    ~~~
    Return *success* if bbt could create the dir1 file or the file *file_name* containing the text in the code fenced lines.

    If there is already a *file_name*, the Step will fail. To get it overwritten with a new file or directory, add the `new` adverb :  
    ``Given the new directory `dir1` ``  
    `` Given the new file `file_name` ``
    ~~~
    ```
    line 1
    line 2
    line 3
    ```
    ~~~
    Note that unless using the --yes option, user will be prompted to confirm deletions.

  - ``Given there is no directory `dir1` ``  
    Return *success* if there is no directory dir1, or if dir1 was successfully deleted.
    
   
**2. When**  

  *When* has two main functions : checking that a file with some contents is available before run, and running  a command.

  - `` When I run `cmd` ``  
    Return *success* if *cmd* was run.  
    *failed* usually means that bbt couldn't find the cmd to run.

  - `` When I successfully run `cmd` ``  
    Return *success* if *cmd* was run **and** returned no error.  
  (This is a shortcut to avoid the usual following line `Then I get no error`).

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
  Note that both *is* and *contains* may be followed by a multiline text :
  ~~~
  When `file` contains 
  ```
    line 1
    line 2
    line 3
  ```

### References

- [The grammar](grammar.md) Extracted with `bbt -lg` (--list_grammar)
- [Keywords](keywords.md) Extracted with `bbt -lk` (--list_keywords)

---------------------------------------------------------------------

## Behavior

### Blank lines and Case sensitivity

bbt is designed for human, and is not going to trap users with blank line, white space or casing differences, meaning that :
- keyword are case insensitive, `- When` or `- when`, it's as you want.
- blank lines and casing are ignored when comparing actual with expected output. I suppose that if you expect `version 1.0` and the actual output is `Version 1.0` the test is OK.

This is not always the expected behavior, you may want to check the exact output.  
This is in the TDL, not yet implemented.  
The preferred implementation is an explicit mention in the scenario, it will not be an option on command line.

On the other hand, the order of lines in files is generally meaningful.
But here the alternative behavior is already implemented through the `unordered` keyword.  
(cf. [feature `unordered`](features/A140_Unordered_Keyword.md) for an example).


### Execution

bbt is executed where you launch the command. All output files will be created here, and input file are expected here. 

bbt scenarii are Markdown files. So if you don't specify the precise file names, bbt will try to execute all md file.  
To see what files, use `bbt --list_files`, possibly with `--recurse`.  
(or the short form `bbt -lf -r`).

But if you specify the files, even using wildcards, like in `bbt tests/robustness*`, then bbt will consider that you know what you do, maybe you have a different naming convention, and will try to run each of them. So that you can name your file `.bbt`, or `.gmd` as you which.

As a special rule, two file will be ignored even if they are in the search path : the template file (bbt_template.md), and the output file if the -o option is used. The first is not supposed to be run, and the second is probably a consequence of a previous run. 

### Scenario files format

The BBT Markdown subset try to comply with [CommonMark Spec](https://spec.commonmark.org/), meaning that bbt always generate Common Mark compliant Markdown.
On the other hand, restrictions apply when writing bbt scenario.

#### Steps
Because the lexer is not able to make a difference between bullet lines in steps definition or bullet lines in normal text, there is limitations on where you can use it.
- *bbt* will consider bullet line starting with `-` as comment before the first "scenario" header. 
- *bbt* will consider all lines starting with `-` as Step within a scenario. As a consequence, **Don't use `-` in comments within a Scenario.**
- *bbt* will also consider line starting with the other official bullet markers (`*`and`+`) as comment, and **not steps line marker**, so that you can use those markers where you want without interfering with the lexer.  
Our simple suggestion : uses `-` for Steps and `*` for all bullet list in comments.

#### Headings
Only [ATX headings](https://spec.commonmark.org/0.31.2/#atx-headings) are supported, not [Setext headings](https://spec.commonmark.org/0.31.2/#setext-headings), meaning that you can write :
```
## Feature: My_Feature
```
but
```
Feature: My_Feature
-------------------
```
won't be recognized as a Feature.

####  Fenced Code blocks

Both ``` and ~~~ code fence mark are recognized by bbt, but only in steps, and only the first block.
Meaning that code blocks can be used within the documentation without interfering with bbt.

And, as per Markdown rules, the closing mark should the same as the opening one.
So that code blocks may include code block marks of the other type.  
For example, you can have :
> - Given the file `foo.md` 
> ```md
>
> This md file contains a code fenced block :
> ~~~
> here it is
> ~~~
> 
> ```
