User Guide  <!-- omit from toc -->
==========

- [Basic Concepts](#basic-concepts)
- [Command line examples](#command-line-examples)
- [Features](#features)
  - [Cleanup](#cleanup)
  - [Background](#background)
  - [No more tests directory?](#no-more-tests-directory)
  - [Removing Files and directories](#removing-files-and-directories)
    - [using the negative form](#using-the-negative-form)
    - [using the positive form](#using-the-positive-form)
- [Tips](#tips)
  - [Understanding what bbt doesn't understand](#understanding-what-bbt-doesnt-understand)
  - [Test in place](#test-in-place)


## Basic Concepts

Basic concepts of bbt files are illustrated in this basic example :

```md
## Scenario : Command line version option

- When I run `uut --version`
- Then the output contains `version 1.0`
```

1. **the BDD usual keywords** : *Scenario*, *When*, *Then*, etc.  
bbt uses a subset of the [Gherkin language](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language), in the [Markdown with Gherkin](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) format.

2. [**bbt specifics  keywords**](keywords.md) : *run*, *output*, *contains*, etc.  
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

## Command line examples

> bbt --exec_dir /tmp --output docs/results.md tests/scenario.md 

  This example illustrate the control you have on execution, by choosing the scenario in one directory, executing in another, and putting the results in a third.

> bbt --yes tests/scenario.md 
 
  When your scenario is fine-tuned, use that option to avoid the interactive confirmation of every dir or file deleted or overwritten by the scenario.

## Features

### Cleanup

For test purpose, scenarios may create multiples files and dir, and it is tedious to cleanup all those files.  
bbt provides two solutions to help :
1. for the files created by bbt, the `--cleanup` option  
   bbt records all files it creates and try to delete them.  
   Note that this is done document per document (file per file), meaning that if a scenario in *y.md* rely on things created in *x.md* (bad practice), it will fail, because they will be deleted at the end of *x.md* run, before *y.md* start. 
2. for the files created by the software under test, the `--exec_dir` option  
   You can specify a directory where the scenario will be run. If your scenario is not creating files somewhere else, then you will have only one dir to remove.

### Background
*bbt* supports a Background scenario, that is a special scenario that will be executed before the start of each following scenario.

```md
### Background :
  - Given the new `config.ini` file

### Scenario : normal use cas
  - When I run `uut append "size=80x40" config.ini` 
  - Then `config.ini` should contains `"size=80x40"`

### Scenario : same check, but after Background execution (should fail)
  - Then `config.ini` should contains `"size=80x40"`
```

In this case, the Background will prompt (unless `--yes` option) to erase any existing `config.ini` file .
Meaning that the second scenario should fail.

Background may appears at the beginning, at document level, or at feature level, or both.
Before each scenario, the document background will be executed, and then the feature background.

### No more tests directory?
It's a common practice to have a dev tree structured like that:
- src/
- docs/
- obj/
- tests/  
etc.

And usually the tests directory contains the tests definition, is the place to run them, and the place where results are produced.

But bbt goals is to have a specification, that normally reside in docs/, directly executable.  
Test results are also part of the documentation. 
And execution may be done in /tmp, why not, thanks to the --exec_dir option.
The title sounds kind of a provocation, it is not, unless you have other kind of tests to put there. 

### Removing Files and directories

When setting up a test, we often need to check that there is no pre-existing file or directory.  
To avoid the burden of deleting those files in a Makefile or a script, bbt interpret a line like :
> ``Given there is no `.config` file``  
as :
*if there is, delete it*  
and offer to delete it, or even delete it automatcaly. To avoid any unwanted deletion, it is important to understand the following behavior.

#### using the negative form

The two lines below looks very close :
> ``Then  there is no `.config` file``  
> ``Given there is no `.config` file``  
 
But while in the "Then" step, bbt is supposed to checks that there is no such file, in the "Given" step it is supposed to make so that there is no such file.  
This is why the default behavior is not the same :
- When Step : if there is a `.config` file, the assertion will fail.  
- Given Step : if there is a `.config`file, bbt will prompt the user to confirm the deletion. If the user deny the deletion, or if the deletion fail, the assertion will fail.

Note that the usual `--yes` option is available, for batch running.

>[!WARNING]
> Use `--yes`option with caution : 
> ``Given there is no directory `./src` ``  
> will cause the whole tree rooted at ./src to be silently destroyed when used with `--yes`.

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
``Given the new directory `dir1` `` **and** confirm deletion when prompted, or use the `--yes` option.

Fixme: as of 0.0.5, bbt is not able to simulate interactive behavior, and so there is no test of this behavior implemented yet.
If there is no test, don't trust the doc :-)

## Tips

### Understanding what bbt doesn't understand

Error messages provided by the lexer are not bullet proof (and it is likely that no special effort will be put on improving error messages in the future...).

For example, if you forget backticks on dir1 in :  
``- Given the directory dir1 ``  
It wont tells you *didn't you forget to "code fence" dir1?*.  
It will just says :  
*Unrecognized step "Given the directory dir1"*

A good reflex in such a case is to ask *bbt* what did he understand from the test file, thanks to the -e (--explain) option.  
It will tell you something like :  
`GIVEN_STEP, UNKNOWN, Text = "Given the directory dir1"`  
meaning that the only thing he was able to conclude from the Text is that it's "Given" step.

But on the fixed version :  
``GIVEN_STEP, FILE_CREATION, Text = "Given the directory `dir1`", File_Name = "dir1"``  
you'll see that the second field has changed from UNKNOWN to FILE_CREATION, and that there is a new field is displayed, the File_Name : bbt knows what to do.

### Test in place

The `--output` create a test results file, in Markdown format, that cross-references all executed files.
Meaning that if the result file is in the `web` sub-directory, and the scenarios in the `tests` sub-directory, link will be for example `../tests/scenrio_1.md`. 
Be aware of that if you move the file in another dir after the run : move both scenarios and results file in a coherent way.

Or use the following best pratice : produce results files directly where you expect them, run the scenarios directly from the doc directory, and just use the "--exec_dir" to run the tests somewhere else to avoid producing crap within the docs tree.

For example, if you have : 
```
├── docs
│   ├── test_run
│   │   └── results.md
│   └── features
│       ├── feature_1.md
│       ├── feature_2.md
│       └── feature_3.md
├── tests
```

the command could be :
> cd tests  
> bbt --output ../docs/test_run/results.md ../docs/features/*.md


