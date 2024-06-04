User Guide  <!-- omit from toc -->
==========

- [Basic Concepts](#basic-concepts)
- [Basic Features](#basic-features)
- [More advanced feature and cool stuff](#more-advanced-feature-and-cool-stuff)
  - [Background](#background)
  - [No more tests directory?](#no-more-tests-directory)
  - [Removing Files and directories](#removing-files-and-directories)
    - [using the negative form](#using-the-negative-form)
    - [using the positive form](#using-the-positive-form)
- [Tips](#tips)
  - [Understanding what bbt doesn't understand](#understanding-what-bbt-doesnt-understand)


## Basic Concepts

Basic concepts of bbt files are illustrated in this basic example :

```md
## Scenario : Command line version option

- When I run `uut --version`
- Then the output contains `version 1.0`
```

1. **the BDD usual keywords** : `Scenario`, *When*, *Then*, etc.  
bbt use a subset of the [Gherkin language](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language), in the [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) format.

2. [**bbt specifics  keywords**](#Keywords) : *run*, *output*, *contains*, etc.  
Here is an example with keywords in bold :  
**Given** there **is** **no** `.utt` **directory**  
**When** I **run** `uut --init`  
**Then** there **is** **no** **error**  
**And** **file** `.uut/config.ini` **contains** `lang=de`  
  
3. **glue word** : *I*, *the*  
As illustrated in the previous example, some words are ignored by bbt. Their only aim is to give users a way to read and write more natural english. This semi-formal language is an important bbt feature. As long as the language remains simple, the risk of ambiguity is low (Describing behavior is specifying, and you certainly don't want ambiguity when writing specifications).
   
4. [**code span** (in Markdown parlance)](https://spec.commonmark.org/0.31.2/#code-spans), that is text surrounded by backticks : `` `uut --version` ``, `` `version 1.0` ``  
bbt uses code span to express a command, a file or directory name or some expected output.

> [!WARNING]
> Till now, there is no ambiguity in the grammar between file and string : bbt is always able to understand what it's about.  
> To avoid ambiguity between file and directory, I had to take into account both `file` and `directory` keywords.  
> It is not excluded in the future that the Markdown syntax for files becomes mandatory instead of code span for the same reason : backtick would then be reserved to command or other strings, and File or dir would be written ``[my_file](my_file.md)``.  
> I'll try to avoid this evolution as it would be less natural to write, and this goes against project objectives.  
 
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


## Basic Features

TBD

## More advanced feature and cool stuff

### Background
*bbt* supports a Background scenario, that is a special scenario that will be executed before the start of each following scenario.

```md
### Background :
  - Given there is no `config.ini` file

### Scenario : normal use cas
  - When I run `uut create config.ini` 
  - When I run `uut append "size=80x40" config.ini` 
  - Then `config.ini` should contains `"size=80x40"`

### Scenario : the last command does not meet expectation (test should fail)
  - When I run `uut append "lang=fr" config.ini` 
  - Then I should get no error
```

In this case, the Background will erase the `config.ini` file created by the first scenario before executing the second. This should cause the scenario to fail, if uut is not able to add something to the `config.ini` file without creating it first.

Background may appears at the beginning, at document level, or at feature level, or both.
Before each scenario, the document background will be executed, and then the feature background.

### No more tests directory?
Think about it : tests description are in the docs/tests/ directory, and you want the result in the docs/ directory.  
You just have to run 
```
bbt -o docs/tests_results.md docs/tests/*.md
```
The title is kind of a provocation. Obviously, you need to run the tests somewhere and there will be residue. But as there is no input or output to keep in that directory (unless debugging the test himself), you can as well delete the execution dir after each run.

### Removing Files and directories

#### using the negative form

The two lines below looks very close :
> ``Then  there is no `.config` file``  
> ``Given there is no `.config` file``  
 
And indeed, bbt default behavior will be the same : if there is a `.config` file, the assertion will fail.  

But while in the former case, bbt is supposed to checks that there is no such file, in the later case it is supposed to make so that there is no such file.  
Simply stated, it is supposed to erase the file.

To get this more handy (and logical) behavior, just call bbt with the `--auto_delete` (or `-ad`) option.

Same apply to directories. When using `-ad` options
> ``Given there is no directory `./src` ``  

will cause the whole tree rooted at ./src to be destroyed.

>[!WARNING]
>Use with caution : you could as well delete /home (provided you have the privilege)!

#### using the positive form

When using the *there is no* form, the meaning is pretty obvious.

But what is the expected behavior of the line  
``Given the directory `dir1`,`` if dir1 exists?  
The intent of the user to erase the directory is less obvious.

To avoid any unwanted recursive deletion, in that case bbt will create a `dir1` directory only if there is none.
If the intent is to start from a white page and erase an existing homonym, the "new" keyword should be used.

So, if you want to start with a possibly existing dir1, use :  
``Given the directory `dir1` ``  
If you want to start with a brand new one whatever is the situation, use :  
``Given the new directory `dir1` `` **and** uses the `-ad` option.


## Tips

### Understanding what bbt doesn't understand

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
On the the fixed version :  
``GIVEN_STEP, FILE_CREATION, Text = "Given the directory `dir1`", File_Name = "dir1"``  
you see that the (internal) action field has changed from UNKNOWN to FILE_CREATION, and that the File_Name field has now a value.


