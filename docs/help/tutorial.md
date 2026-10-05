
## Introduction  

This is a bbt tutorial, generated with BBT 0.4.1  

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

    [# Background] (at most one per Feature)

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

Outside previously mentioned headers and steps, lines are ignored by bbt, that is considered as comments.
Meaning that you can interleave Scenarios with comments as you want: comments may appear between Header and Steps or even between Steps and code blocks.  
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

A few examples, to get the feeling:  

    - Given there is no `config.ini` file
    - When I successfully run `sut`
      (equivalent to "- When I run `sut`" followed by "- Then I get no error")
    - Then the output is `sut version 1.0`

Steps can be continued with "And" or "But", synonymous of the *Given* /
*When* / *Then* that precedes:  

    - Then output contains `234 processed data`
    - And  output contains `result = 29580`
    - But  output does not contain `Warning:`  

Step parameters (the command to run, the expected output, the file names...)
are given between backticks, in a code fenced block, or in an external
file, as detailed in the following sections. A complete reference of all
the steps, classified by category, ends this chapter.

#### Parameters  

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

#### Matching level  

Above forms test that the output is exactly what is given.  
If what you want is just test that the output contains something, then use the "contains" keyword:  

    - Then output contains `sut version v0.1.0`  

If what you want is search for some pattern, then use the "matches" keyword, followed by a regexp :  

    - Then output matches `sut version v[0-9]+\.[0-9]+\.[0-9]+`  

Note that the regexp must match the entire line,
don't forget to put ".*" at the beginning or at the end if necessary.  

#### Steps examples, by category  

All the available steps, classified by what they set up, run or check:  

**Setup: files and directories**  

    - Given there is no `config.ini` file
    - Given there is a `config.ini` file
    - Given there is no `dir1` directory
    - Given the directory `dir1`
    - Given the new directory `dir1`
    - Given the new file `config.ini`
      ```
      verbose=false
      lang=am
      ```
    - Given the `config.ini` file containing `lang=am`
    - Given the executable file `command.sh`
      ```
      #!/bin/bash
      echo "bbt rules!"
      ```

**Setup: environment**  

    - Given the environment variable `LC_ALL` is `C`
    - Given the environment variable `NO_COLOR` is not set

**Running commands**  

    - When I run `gcc --version`
    - When I successfully run `make`
    - When `grep pattern missing_file.txt` fails
    - When I run `grep pattern file1.txt` or `grep pattern file2.txt`
      (the scenario is run once per command of the or list)

**Interacting with a running command**  

    - When I run `./program`
    - Then the output is `Continue? [y/n]`
    - When I type `y`
      (single key press, no Enter, for a program reading a key)
    - Then the output is `Continuing`
    - When I enter `some text`
      (a whole line, followed by Enter, for a program reading a line)

**Checking the output**  

    - Then the output is `sut version 1.0`
    - Then I get `sut version 1.0` (equivalent form)
    - Then the output is
      ```
      a multi-line
      expected output
      ```
    - Then the output is equal to file `expected.txt`
    - Then the output contains `3 matches replaced`
    - Then the output does not contain `Warning:`
    - Then the output matches `sut version v[0-9]+\.[0-9]+\.[0-9]+`
    - Then I get file (unordered) `flowers2.txt`
    - Then there is no output

**Checking the error output and the exit code**  

    - Then the error output contains `No such file or directory`
    - Then there is no error output
    - Then I get an error
    - Then I get no error
    - Then the exit code is `2`

**Checking files and directories**  

    - Then there is a `config.ini` file
    - Then there is no `config.ini` file
    - Then there is a `dir1` directory
    - Then the file `config.ini` is `lang=am`
    - Then the file `config.ini` is equal to file `expected.ini`
    - Then the file `config.ini` is no more equal to file `expected.ini`
    - Then the file `config.ini` contains `lang=am`
    - Then the file `config.ini` does not contain `secret`
    - Then the file `list` matches `.*string.*`

## Help  

To get a complete view on step's grammar, with examples:  

    bbt help grammar  

To check your scenario with a dry run:  

    bbt explain scenario.md  

More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-
