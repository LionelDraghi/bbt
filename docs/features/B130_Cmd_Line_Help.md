<!-- omit from toc -->
# Feature: Clear command line help 

bbt goal to have an almost zero effort learning curve rely on a clear command line help.

_Table of Contents:_
- [Background:](#background)
- [Scenario: calling bbt without parameter or with -h put the normal help](#scenario-calling-bbt-without-parameter-or-with--h-put-the-normal-help)
- [Scenario: filtering help](#scenario-filtering-help)
- [Scenario: matching help](#scenario-matching-help)
- [Scenario: others help](#scenario-others-help)
- [Scenario: On\_All help](#scenario-on_all-help)
- [Scenario: tutorial generation](#scenario-tutorial-generation)
- [Scenario: generated example is OK](#scenario-generated-example-is-ok)

## Background:
- Given the file `base_help.txt` 
~~~
Usage : bbt [Options]* [Command] file*  
  
  The default command is 'run'  
  If no file is provided, reads *.md files  
  
Basic options:  
       --yes        : do not prompt if deletion is needed in  
                      "Given" steps, silently answer yes  
  -c | --cleanup    : after run, remove every file and dir  
                      created by bbt in "Given" steps  
  -r | --recursive  : search scenarios in subdirs  
  -k | --keep_going : do as much work as possible  
       --Werror     : treat warnings as errors  
  -v | --verbose  
  -q | --quiet      : no message unless error,  
                      Warnings are also ignored  
  
Basic commands:  
       run               : the default command  
  ls | list              : list selected items  
  he | help [topic]      : base help, or more on one of the topic listed below  
  he | help on_all       : help on all topics  
  he | help tutorial     : create a tutorial  
  he | help example      : create an example scenario   
  
Help topics:  
  filtering : --select --exclude --include  
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines  
  other     : list_files list_keywords list_grammar explain --strict  
              --index file.md --junit file.xml --exec_dir --tmp_dir --generate_badge  
  debug     : -d tt -ls -t  
  
bbt version 0.3.0-dev  
https://github.com/LionelDraghi/bbt/
~~~

- Given the file `filtering.txt`
~~~
Filtering:
  Features, Scenarios and Steps may be selected or filtered.
  By default, every item is selected.
  -s | --select 'string'  : only items containing 'string' are selected
  -e | --exclude 'string' : remove from selection items containing 'string'
  -i | --include 'string' : include in selection items containing 'string'
  Multiple occurrences are processed in order, meaning that you can exclude
  a whole Feature and then re-include a Scenario belonging to this feature.
~~~

- Given the file `matching.txt`
~~~
Human vs exact matching:  
  bbt default behavior is "human match", that is ignoring differences  
  in casing, ignoring consecutive spaces, and ignoring blank lines.  
  The opposite behavior, to make strict compare, is set with:  
  -em  | --exact_match  
  exact_match may be altered if **followed** by one or more of:  
  -iw  | --ignore_whitespaces (default)  
  -ic  | --ignore_casing      (default)  
  -ibl | --ignore_blank_lines (default)  
  For example, "-em -iw" will take into account blank lines and  
  casing but ignore whitespaces  
  Note that -iw, -ic, and -ibl are useless if not preceded by -em,   
  because they are the default setting.  
  There is also a  
  -hm  | --human_match  
  option, equivalent to defaults "-iw -ic -ibl", if you want to  
  assert on the command line that this is the required behavior.  
~~~

- Given the file `tutorial.txt`
~~~
## Introduction  

This is a bbt tutorial, generated with BBT 0.3.0-dev  

A bbt file contains:  
1. text that is ignored
2. scenarios that are interpreted
 
A Scenario minimal structure is:  
1. a Scenario header, that is a line starting with "# Scenario : "  
2. One or more Steps, that is lines starting with "- Given", "- When" or "- Then"  

**Example :**  
    ## Scenario : get gcc version  
    - When I run `gcc --version`  
    - Then I get `14.2.0`  

## Scenarios structure   

The complete scenarios structure is heavily inspired by Gherkin files, with a few nuances. 

    [# Background] (at most one per file))

    [# Feature] (any number of features per file)

    [# Background] (at most    per Feature)

    # Scenario 1 (any number of scenarios per feature)
    - Given/When/Then step
    [- Given/When/Then/And/But step] (any number of steps per scenario)

**Example :**  

    # Feature : Case sensitivity control 

    ## Scenario : default behavior, no option  
    - When I run `grep xyz input.txt`  
    - Then ...  

    ## Scenario : case insensitive search  
    - When I run `grep -i xyz input.txt`  
    - Then ...  

The only headers reserved for bbt uses are "Feature", "Scenario" or "Example", and "Background"  
(Example is a synonym for Scenario).  
Header level is not taken into account : `# Scenario` is equivalent to `#### Scenario`.  

### Non interpreted content

Outside previously mentionned headers and steps, lines are ignored by bbt, that is considered as comments.
Meanning that you can interleave Scenarios with comments as you want: comments may appear between Header and Steps or even between Steps and code blocks.  
In case of doubt, just run `bbt explain` on your scenario to ensure that the file is understud the way you want.  

### Background  

Preconditions common to several scenarios may be put in a Background section, before scenarios :  

    ### Background:  
    - Given there is no `input.txt` file  
    - Given there is a `tmp` dir  

Background scope is logical : if it appears at the beginning of the file, it applies to all  
scenario in the file, if it appears at the beginning of a feature, it apply only  
to the scenarios of this feature.  
If there is both, Backgrounds are run in appearance order.  

**Example :**  

    ## Background 1 
    - Given there is no `config.ini` file  
    - Given ...  

    # Feature A  

    ## Scenario A.1  
    Background 1 will run here  
    - When I run `grep -i xyz input.txt`  
    - Then ...  

    # Feature B  

    ## Background 2 
    - Given ...  

    ## Scenario B.1  
    Background 1 run here  
    Background 2 run here  
    - When ...  

### Steps  

Steps are the most important part of bbt files, they perform the actions and checks.  
- Given [setup condition]  
- When  [action to perform]  
- Then  [expected result]  

**Examples of steps:**  

    - Given there is no `.config` dir
    - Given the `config.ini` file
      ```
      verbose=false
      lang=am
      ```
    - Given the executable file `command.sh`
      ```
      #!/bin/bash
      echo "bbt rules!"
      ```
    - When I successfully run `xxx`
      (Equivalent to both lines "- When I run `xxx`" and "- Then I Get No Error")
    - Then there is no output
    - Then I get no error
    - Then output is `sut v0.1.0` (Equivalent "Then I get...")

You can continue a list of Given / When / Then with "And" or "But":  

    - Then output contains `234 processed data`
    - And  output contains `result = 29580`
    - But  output doesn't contain `Warning:`
    - And  output does not contain `Error:`  

*And* and *But* are synonymous of the *Given* / *When* / *Then* that preceedes.  

### Parameters  

Parameters are given in three possible ways :  
  1. as a string:

    - Then I get `string`

  2. as a code fenced block:

    - Then I get
    ```
    This is my multi-line
    file content
    ```

  3. in an external file:

    - Then I get the content of file `expected.txt`  

     Note in that case the mandatory "file" keyword  

### Matching level  

Above forms test that the output is exactly what is given.  
If what you want is just test that the output contains something, then use the "contains" keyword:  

    - Then output contains `sut version v0.1.0`  

If what you want is search for some pattern, then use the "matches" keyword, followed by a regexp :  

    - Then output matches `sut version v[0-9]+\.[0-9]+\.[0-9]+`  

Note that the regexp must match the entire line,
don't forget to put ".*" at the beginning or at the end if necessary.  

## Help  

To get a complete (although less friendly) view on the grammar:  

    bbt list_grammar  

To check your scenario with a dry run:  

    bbt explain scenario.md  

More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-
~~~

- Given the file `other.txt`
~~~
Other commands:
  lf | list_files      : list Scenario files found
  lk | list_keywords   : list Step keywords
  lg | list_grammar    : list possible Steps syntax and associated action
  ex | explain         : explain what bbt understands from Scenarios files
                         (do not run the scenarios)

Other options:
        --strict         : warn when not strictly following Gherkin common guidelines
        --index file.md  : create an md file with test results
                           that indexes all scenarios run.
                           This file will contain the normal bbt output,
                           whatever are the verbosity settings (-q, -v, etc.)
                           for standard output.
        --junit file.xml : generate a JUnit XML report file
  -ed | --exec_dir 'dir' : run command in dir instead of current dir
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir
  -sb | --status_bar     : enable a progress bar in the terminal (WIP!!)  
  -gb | --generate_badge badge.url : create a text file containing
                           a shields.io URL to get a svg badge
                           with tests results summary.
~~~


## Scenario: calling bbt without parameter or with -h put the normal help
- When I run `./bbt` 
- then the output contains file `base_help.txt`
- And the output contains 
~~~
https://github.com/LionelDraghi/bbt/  
~~~

- When I run `./bbt help` 
- then the output contains file `base_help.txt`
- And the output contains 
~~~
https://github.com/LionelDraghi/bbt/  
~~~

- When I run `./bbt he` 
- then the output contains file `base_help.txt`
- And the output contains 
~~~
https://github.com/LionelDraghi/bbt/  
~~~

## Scenario: filtering help
- When I run `./bbt he filtering` 
- then the output is file `filtering.txt`

## Scenario: matching help
- When I run `./bbt help matching` 
- then the output is file `matching.txt`

## Scenario: others help
- When I run `./bbt help other` 
- then the output is file `other.txt`

## Scenario: On_All help
- When I run `./bbt help on_all` 
- then the output contains file `base.txt`
- and  the output contains file `filtering.txt`
- and  the output contains file `matching.txt`
- and  the output contains file `other.txt`

## Scenario: tutorial generation

> - When I run `./bbt help tutorial` 
> - then the output contains file `tutorial.txt`

## Scenario: generated example is OK

Testing that the generated example works is the bare minimum

> - When I run `./bbt help example` 
