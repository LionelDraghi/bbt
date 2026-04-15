
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
