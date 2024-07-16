# gcc simple tests

## Background:
- Given there is no `main` file

## Scenario: gcc version?
- When I run `/usr/bin/gcc -v`

- Then the output contains `gcc version`


## Scenario: compiling and executing an hello word
- Given the new file `main.c`
```c
#include <stdio.h>
int main() {
printf("Hello, World!");
return 0;
}
```
- And given there is no `main` file

- When I run `/usr/bin/gcc main.c -o main`
- And  I run `main`

- Then the output is `Hello, World!`
- And I get no error
  
---
### Structure

Each file is composed of:
+ an optional *Background* section, that is a special Scenario that will be run before each following scenario
+ one or more optional *Feature* section
+ each Feature may contain one or more Scenarios
+ Scenarios may appear without Feature (as in the example above)

### Step Examples:
   
**1. Given**
   
+ Given there is no `dir1`       directory  
+ Given there is a  `config.ini` file  
+ Given the new file `config.ini` containing `lang=it`  
`new` specify that I want a brand new one, meaning delete the existing one  
+ Given the new `config.ini` file 
```
line 1 
line 2
```
+ Given the new `dir1` directory

**2. When**
   
+ when I run `cmd`
+ when I successfully run `cmd`  
`Successfully` add a check on the return code (equivalent to `Then I get no error`)  

**3. Then**
 
+ Then there is a  `config.ini` file
+ Then there is no `dir1` directory
+ Then I get some error
+ Then there is no error
+ Then I get no output
+ Then output is
```
line 1
line 2
```
+ Then output contains `msg`
+ Then `config.ini` contains `mode=verbose`

  