User Guide  <!-- omit from toc -->
==========

- [Where to start](#where-to-start)
- [Features](#features)
  - [Cleanup](#cleanup)
  - [Background](#background)
  - [Selection](#selection)
  - [No more tests directory?](#no-more-tests-directory)
  - [Removing Files and directories](#removing-files-and-directories)
    - [using the negative form](#using-the-negative-form)
    - [using the positive form](#using-the-positive-form)
- [Behavior](#behavior)
  - [Blank lines and Case sensitivity and line order](#blank-lines-and-case-sensitivity-and-line-order)
  - [Execution](#execution)
- [Tips](#tips)
  - [Filtering, tags and Conditional execution](#filtering-tags-and-conditional-execution)
  - [Understanding what bbt doesn't understand](#understanding-what-bbt-doesnt-understand)
  - [Test in place](#test-in-place)
  - [Avoiding ambiguities](#avoiding-ambiguities)
  - [Command line examples](#command-line-examples)
  - [Makefile snippet](#makefile-snippet)

## Where to start

  To understand the basics of `bbt`, read first the [README](https://github.com/LionelDraghi/bbt/blob/main/README.md).
  That's enough to start playing.
  
  To get more Steps examples, read the [template of scenario](https://github.com/LionelDraghi/bbt/blob/main/docs/bbt_template.md) generated with `bbt create_template`.

  And to have an overview of command line options, run `bbt` without arguments.

## Features

### Cleanup

For test purpose, scenarios may create multiple files and dir, and it is tedious to cleanup all those files.  
bbt provides two solutions to help:
1. for the files created by bbt, the `--cleanup` option  
   bbt records all files it creates and try to delete them.  
   Note that this is done document per document (file per file), meaning that if a scenario in *y.md* relies on things created in *x.md* (bad practice), it will fail, because they will be deleted at the end of *x.md* run, before *y.md* start. 
2. for the files created by the software under test, the `--exec_dir` option  
   You can specify a directory where the scenario will be run. If your scenario is not creating files somewhere else, then you will have only one dir to remove.

### Background
*bbt* supports a Background scenario, that is, a special scenario that will be executed before the start of each following scenario.

```md
### Background :
  - Given there is no `config.ini` file

### Scenario : normal use cas
  - When I run `sut append "size=80x40" config.ini` 
  - Then `config.ini` should contains `"size=80x40"`

### Scenario : same check, but after Background execution (should fail)
  - Then `config.ini` should contains `"size=80x40"`
```

In this case, the Background will prompt (unless `--yes` option) to erase any existing `config.ini` file.
Meaning that the second scenario should fail.

Background may appear at the beginning, at document level, or at feature level, or both.
Before each scenario, the document background will be executed, and then the feature background.

Background, like Scenarios, may contain Given, When and Then Steps. 

Note that bbt is more flexible than Cucumber, for example, where Background should contain only Given Steps, and where there is only one Background per file.

### Selection 

You may exclude or select specific steps, scenarios, features or document.  
`--select` allows you to run only the specified items.  
`--exclude` allows you to run all but the specified items.  
`--include` add items excluded with the two others options.  

Those options are followed by a string (not a regexp).  
The string is searched in file name, Feature, Scenario, Background title, or Step line.  
Matching is case insensitive. `Windows` will match all `WindowsOnly` and `not on windows`.  

Note that there is no specific syntax for tags. You can use `@Tag`, `#tag` or your own convention.
But unlike with Gherkin tags are not searched on the line preceding the title, but on same line.

The selection is smart enough to auto-extend to parents and needed Background : if you select a specific Step, the enclosing Scenario will be also selected, the Background if any, the Feature enclosing the Scenario, and the document enclosing the Feature.

Examples :

```sh
bbt --exclude "For Windows only" tests
```

Run all Scenarios/Steps/Features except those for Windows.

```sh
bbt list --Select "For Windows only" --exclude "@long_test" tests
```

List scenarios for Windows, unless too long.

### No more tests directory?
It's a common practice to have a dev tree structured like this:
- src/
- docs/
- obj/
- tests/  
etc.

And usually the tests directory contains the tests definition, is the place to run them, and the place where results are produced.

But bbt goal is to have a specification, that normally resides in docs/, directly executable.  
Test results are also part of the documentation. 
And execution may be done in /tmp, why not, thanks to the `--exec_dir` option.
The title sounds kind of a provocation, it is not, unless you have other kind of tests to put there. 

### Removing Files and directories

When setting up a test, we often need to check that there is no pre-existing file or directory.  
To avoid the burden of deleting those files in a Makefile or a script, bbt interprets a line like:
```md
- Given there is no `.config` file
```
  
as:
*if there is, delete it*  
and offer to delete it, or even delete it automatically. To avoid any unwanted deletion, it is important to understand the following behavior.

#### using the negative form

The two lines below looks very close:
```md
- Then  there is no `.config` file  
- Given there is no `.config` file  
```
 
But while in the "Then" step, bbt is supposed to check that there is no such file, in the "Given" step, it is supposed to make so that there is no such file.  
This is why the default behavior is not the same:
- When Step: if there is a `.config` file, the assertion will fail.  
- Given Step: if there is a `.config`file, bbt will prompt the user to confirm the deletion. If the user deny the deletion, or if the deletion fails, the assertion will fail.

Note that the usual `--yes` option is available, for batch run.

>[!WARNING]
> Use `--yes` option with caution: 
> ``Given there is no directory `./src` ``  
> will cause the whole tree rooted at ./src to be silently destroyed when used with `--yes`.

#### using the positive form

When using the *there is no* form, the meaning is pretty obvious.

But what is the expected behavior of the line  
```md
- Given the directory `dir1`
```
if dir1 exists?  
The intent of the user to erase the directory is less obvious.

To avoid any unwanted recursive deletion, in that case bbt will create a `dir1` directory only if there is none.
If the intent is to start from a white page and erase an existing homonym, the "new" keyword should be used.

So, if you want to start with a possibly existing dir1, use:  
```md
- Given the directory `dir1`  
```
If you want to start with a brand new one whatever is the situation, use:  
```md
- Given the new directory `dir1`
```
**and** confirm deletion when prompted, or use the `--yes` option.

> [!WARNING] 
> Fixme: as of 0.0.6, bbt is not able to simulate interactive behavior, and so this behavior is only partially tested.  
> And if there is no test, don't trust the doc :-)


## Behavior

### Blank lines and Case sensitivity and line order

bbt is designed for humans, and is not going to trap users with blank line, white space or casing differences, meaning that :
- keywords are case insensitive, `- When` or `- when`, it's as you want.
- blank lines and casing are ignored when comparing actual with expected output. I suppose that if you expect `version 1.0` and the actual output is `Version 1.0` the test is OK.

This is not always the expected behavior, you may want to check the exact output. Use then the `--exact_match` option on the command line.  
Actually, there is more options to control individually casing, whitespaces and blank lines, refer to the [feature file](features/A210_Exact_Match.md) for more details.

The implementation of an explicit mention in the scenario is still in the TDL, not yet implemented.  

On the other hand, the order of lines in files is generally meaningful.
But not always. If you want to check the presence of some files in a directory, you don't care in which order they are listed, and you don't want your test to fail on another platform because the `ls` command behavior may be different.
The "I don't care the line order" behavior is implemented through the `unordered` keyword.  
(cf. [feature `unordered`](features/A140_Unordered_Keyword.md) for an example).

### Execution

bbt is executed where you launch the command. All output files will be created here, and input file are expected here. 

bbt scenarii are Markdown files. So if you don't specify the precise file names, bbt will try to execute all md files.  
To see what files, use `bbt --list_files`, possibly with `--recurse`.  
(or the short form `bbt -lf -r`).

But if you specify the files, even using wildcards, like in `bbt tests/robustness*`, then bbt will consider that you know what you do, maybe you have a different naming convention, and will try to run each of them. So that you can name your file `.bbt`, or `.gmd` as you wish.

As a special rule, two specific files will be ignored even if they are in the search path: the template file (`bbt_template.md`), and the output file if the `-o` option is used. The first is not supposed to be run, and the second is probably a consequence of a previous run. 

## Tips

### Filtering, tags and Conditional execution 

Due to different conventions for file paths between Unix and Windows, the same test may have `/home/username/Documents` as expected output on the first and `c:\users\username\documents` on the later.

An example of such a tags use may be found in bbt own tests.  
(Refer to [this scenario](features/B040_Find_scenarios.md) and the corresponding [Makefile](../tests/Makefile))

Steps or Scenarios contains *Unix_Only* or *Windows_Only* accordingly :
~~~md
- Then the output is on Unix_Only    `dir1/scen1.md`
- And  the output is on Windows_Only `dir1\scen1.md`
~~~

And then bbt is run with `--exclude Windows_Only` or `--exclude Unix_Only` depending on the platform.

Actually, there is no notion of tag for bbt: whatever string in the header or step line may be used.
For example, to run only the scenario *test_1*, just use the option `--select test_1`.  

**Be careful not to use a string that is too general, as it could appear in other headers or steps.**

Note that there is a smart selection of what is run : if *test_1*  depends on a background, it will be also run. 

### Understanding what bbt doesn't understand

Error and warning messages provided by the lexer are not bullet proof.

For example, if you forget backticks on dir1 in:  
```md
- Given the directory dir1
```

It won't tell you *didn't you forget to "code fence" dir1?*.  
It will just say:  
*Unrecognized step "Given the directory dir1"*

A good approach in such a case is to ask *bbt* what did it understand from the test file, thanks to the `-e` (`--explain`) option.  
It will tell you something like:  
`GIVEN_STEP, UNKNOWN, Text = "Given the directory dir1"`  
meaning that the only thing it was able to conclude from the Text is that it's a "Given" step.

But on the fixed version:  
``GIVEN_STEP, FILE_CREATION, Text = "Given the directory `dir1`", File_Name = "dir1"``  
you'll see that the second field has changed from UNKNOWN to FILE_CREATION, and that there is a new field is displayed, the File_Name: bbt knows what to do.

### Test in place

The `--output` option creates a test results file, in Markdown format, that cross-references all executed files.
Meaning that if the result file is in the `web` sub-directory, and the scenarios in the `tests` sub-directory, link will be for example `../tests/scenario_1.md`. 
Be aware of that if you move the files into another dir after the run: move both scenarios and results files in a coherent way.

Or use the following best practice: produce results files directly where you expect them, run the scenarios directly from the doc directory, and just use the `--exec_dir` option to run the tests somewhere else to avoid producing crap within the docs tree.

For example, if you have: 
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

the command could be:
```sh
cd tests  
bbt --output ../docs/test_run/results.md ../docs/features/*.md
```

### Avoiding ambiguities

You can write
```md
- When run `my_command -r`
```
or 
```md
- When I once more try to run something like `my_command -r`
```

but neither is recommended.  
The former is not easy to read, and the latter is misleading by introducing nuances that bbt ignores!  

Moreover, you could change the test semantic by using inadvertently some bbt's keyword. And if not now, your test could fail in the future because of a new keyword in a new bbt version, causing the Step to become incomprehensible.  
If you have long explanation do provide, do it on the next line.

More generally, this is specification, make short sentences and go straight to the point.

### Command line examples

```sh
bbt --exec_dir /tmp --output docs/results.md tests/scenario.md 
```

This example illustrates the control you have on execution, by choosing the scenario in one directory, executing in another, and putting the results in a third one.

```sh
bbt --yes tests/scenario.md 
```
 
When your scenario is fine-tuned, use that option to avoid the interactive confirmation of every dir or file deleted or overwritten by the scenario.

### Makefile snippet 

~~~Makefile
results.md : bin/sut tests/scenario.md
  bbt tests/scenario.md | tee $@
~~~
 
This Makefile rule generate or update the file `results.md`.

If either the software under test (sut) or the scenario is newer than `results.md`, the bbt command will be executed.  

`tee` writes the output to both the console and the file `results.md`. 
(`$@` is a Makefile automatic variable that represents the target of the rule, which in this case is `results.md`).

Kudos to [Manuel](https://github.com/mgrojo/coap_spark/blob/main/client/tests/Makefile)
