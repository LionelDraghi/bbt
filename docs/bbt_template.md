
This is a template bbt file, generated with BBT 0.0.6  
(It's also the shortest possible bbt tutorial!)

A bbt file contains at least :  
1. a Scenario header, that is a line starting with "# Scenario : "  
2. some Steps, that is lines starting with "- Given", "- When" or "- Then"  

Minimal example :  
## Scenario : getting gcc version  
- When I run `gcc --version`  
- Then I get `14.2.0`  

Several scenarios may be organized in "Feature".  
Note that the only headers reserved for bbt uses are "Feature", "Scenario" or "Example", and "Background"  
Other header will be ignored by bbt  

  Steps are the most important part of bbt files.  
  "Given" steps setup preconditions  
  "When"  steps run what should be tested  
  "Then"  steps check the results  

  Examples of steps:  
  - Given there is no `.config` dir
  - Given the `config.ini` file
    ```
    verbose=false
    lang=am
    ```
  - When I successfully run `xxx` (Equivalent to both lines "- When I run `xxx`" and "- Then I Get No Error"
  - Then there is no output
  - Then I get no error
  - Then I get an error
  - Then output is `sut v0.1.0` (Equivalent "Then I get")

  Expected output is given in three possible ways :  
  1. as a string:
     > - Then I get `string`
  2. as a code fenced block:
     > - Then I get
     ```
     This is my multi-line
     file content
     ```
  3. in an external file:
     > - Then I get file `expected.txt`  

     Note in that case the mandatory "file" keyword  

  Above forms test that the output is exactly what is given.  
  If what you want is just test that the output contains something, then use:  
  - Then output contains `sut v0.1.0`

  You can continue a list of Given / When / Then with "And" or "But":  
  - Then output contains `234 processed data`
  - And  output contains `result = 29580`
  - But  output doesn't contain `Warning:`
  - And  output does not contain `Error:`  

Preconditions common to several scenarios may be put in a Background section, before scenarios :  
### Background:  
  - Given there is no `input.txt` file  
  - Given there is a `tmp` dir  

To get a complete (although less friendly) view on the grammar : bbt -lg  
And the to get the list of keywords : bbt -lk  

More features here : https://github.com/LionelDraghi/bbt/tree/main
