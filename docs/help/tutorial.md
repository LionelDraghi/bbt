
## Introduction  

This is a bbt tutorial, generated with BBT 0.6.1  

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

If the expected output contains a part that legitimately changes at each
run — a time tag, a version number, a machine dependent path — then state
it with an "ignoring" clause instead of rewriting the whole line as a
regexp: the volatile parts are masked on both sides before comparing,
and the rest of the line keeps its precise check.

    - Then the output contains `sut version 1.0` (ignoring version)

    - Then I get (ignoring time)
      ```
      [12:34:56] Building foo
      [12:34:57] Done
      ```

The named patterns are "time", "dir" and "version", and any other word
of the clause is a regexp masking what it matches, as in
`(ignoring [0-9.]+)`. Patterns may be combined as in
`(ignoring time and dir)`.
Cf. `bbt help matching` for details.  

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
    - When I successfully run shell `cut -f1 list.txt | sort > sorted.txt`
      (commands are not run through a shell: to get pipes, redirections
      or command substitutions interpreted, a "run shell" step passes
      the whole command to a shell; the shell is chosen with the
      --shell option or the BBT_SHELL environment variable)
    - When I run shell `echo a > a.txt` or `echo b > b.txt`
      (shell steps accept or alternatives too: the shell qualifier
      applies to all the alternatives)
      (the "successfully" and "shell" qualifiers of "run" are
      recognized whatever their position around the command)

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
    - Then the output contains `sut version 1.0` (ignoring version)
    - Then the output does not contain `Warning:`
    - Then the output is not `Error:`
    - Then the output matches `sut version v[0-9]+\.[0-9]+\.[0-9]+`
    - Then I get file `flowers2.txt` (ignoring order)
    - Then there is no output

**Checking the error output and the exit code**  

    - Then the error output contains `No such file or directory`
    - Then the error output is not `No such file or directory`
    - Then there is no error output
    - Then I get an error
    - Then I get no error
    - Then the exit code is `2`
    - Then the exit code is not `2`

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

**Negating a check**  

Any comparison step can be negated with the negation words - `no`,
`not`, `do not`, `doesn't` - whatever their position in the sentence,
as in `the output is not`, `the error output does not contain` or
`the exit code is not`. A negation word on a step form that cannot be
negated, like the run steps, is ignored with a warning: the step keeps
its positive meaning.

## GUI testing

bbt can also drive and observe graphical applications, with the same
scenarios and the same Given / When / Then grammar: the GUI steps
assert on the interface the way the user perceives it, designating
widgets by **role** and **accessible name** (the WAI-ARIA concepts),
whatever the toolkit behind. What you type or press is not injected
input: it is the semantic action, as announced by the accessibility
API. There is no backend reference in the scenarios: the GUI backend
is selected on the command line, `bbt --gui=<kind>` (currently `gtk4`
on Linux), so that the same scenario runs on several backends
unchanged.

**Testing a GUI application**

    - When I run `./todomvc`
    - Then a window appears
    - Then a `To do` window appears (variant with the window title)
    - Then the window contains `What needs to be done?` (the placeholder hint)
    - Then there is a textbox named `New task`
    - Then the `Create` button is enabled / disabled
    - Then the `To do` list is empty
    - Then the `To do` list contains `Buy milk`
    - When I press the `Create` button
    - When I type `Buy milk` into the `New task` textbox

(The roles are the frozen MVP set — `button`, `textbox`,
`dialog`, `list`, ... — cf. `bbt help grammar` for the recognized
forms. All the comparison forms above are negatable, as the CLI
checks.)

A GUI scenario looks like any other scenario:

    ## Scenario: The user adds a task

    - Given there is no file `todos.state`
    - When I run `./todomvc`
    - Then a window appears
    - And there is a textbox named `New task`
    - When I type `Buy milk` into the `New task` textbox
    - And I press the `Create` button
    - Then the `To do` list contains `Buy milk`

Cf. the GUI vocabulary specifications (docs/specifications.md) and
the supported backends (docs/backends.md).

## Help  

To get a complete view on step's grammar, with examples:  

    bbt help grammar  

To check your scenario with a dry run:  

    bbt explain scenario.md  

More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-
