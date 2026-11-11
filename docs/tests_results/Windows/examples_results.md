
# Document: [gcc_hello_world.md](../../examples/gcc_hello_world.md)  
   ### Scenario: [1: Checking installed gcc version](../../examples/gcc_hello_world.md): 
   - OK : When I run `gcc --version`  
   - OK : Then the output matches `(gcc|.* clang).*`  
   - [X] scenario   [1: Checking installed gcc version](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `main.c`  
   - OK : And given there is no `./main` file  
   - OK : When I successfully run `gcc main.c -o main`  
   - OK : And  I run `./main`  
   - OK : Then the output is `Hello, World!`  
   - [X] scenario   [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [3 : get gcc version](../../examples/gcc_hello_world.md): 
   - OK : When I run `gcc -v`  
   - OK : Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
   - [X] scenario   [3 : get gcc version](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [4 : checking error output and return code](../../examples/gcc_hello_world.md): 
   - OK : Given there is no `missing.c` file  
   - OK : Given the environment variable `LC_ALL` is set to `C`  
   - OK : When I run `gcc missing.c -o missing`  
   - OK : Then the exit code is `1`  
   - OK : And the error output contains `No such file or directory`  
   - [X] scenario   [4 : checking error output and return code](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [5 : checking environment variables handling](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `inc/myheader.h` containing  
   - OK : Given the new file `cp.c` containing  
   - OK : Given the environment variable `CPATH` is set to `inc`  
   - OK : When I successfully run `gcc cp.c -o cp`  
   - OK : When I run `./cp`  
   - OK : Then the output is `Hello from CPATH`  
   - [X] scenario   [5 : checking environment variables handling](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [6 : interacting with a program prompt](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `ask.c` containing  
   - OK : And given there is no `./ask` file  
   - **NOK** : When I run `./ask` (..\docs\examples\gcc_hello_world.md:106:)  
..\docs\examples\gcc_hello_world.md:106: Error: the command is still running
   - **NOK** : Then the output is `What is your name?` (..\docs\examples\gcc_hello_world.md:107:)  
..\docs\examples\gcc_hello_world.md:107: Error: Output not equal to expected:
   - **NOK** : When I enter `bbt` (..\docs\examples\gcc_hello_world.md:108:)  
..\docs\examples\gcc_hello_world.md:108: Error: cannot send input to the command
   - OK : When I successfully run `gcc ask.c -o ask`  
   - **NOK** : Then the output is (..\docs\examples\gcc_hello_world.md:109:)  
..\docs\examples\gcc_hello_world.md:109: Error: Output not equal to expected:
   - **NOK** : When I type `y` (..\docs\examples\gcc_hello_world.md:114:)  
..\docs\examples\gcc_hello_world.md:114: Error: no command is running when reaching this step
   - **NOK** : Then the output is `Continuing!` (..\docs\examples\gcc_hello_world.md:115:)  
..\docs\examples\gcc_hello_world.md:115: Error: Output not equal to expected:
   - OK : And the exit code is `0`  
   - [ ] scenario   [6 : interacting with a program prompt](../../examples/gcc_hello_world.md) **fails**  

   ### Scenario: [7 : ignoring a volatile part of the output](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `timed.c` containing  
   - OK : And given there is no `./timed` file  
   - OK : When I successfully run `gcc timed.c -o timed`  
   - OK : And  I run `./timed`  
   - OK : Then I get (ignoring time)  
   - [X] scenario   [7 : ignoring a volatile part of the output](../../examples/gcc_hello_world.md) pass  


# Document: [rpl_case_insensitivity.md](../../examples/rpl_case_insensitivity.md)  
  ## Feature: 1 : Case insensitivity  
   ### Scenario: [1.1 : simple use (single file, no globbing)](../../examples/rpl_case_insensitivity.md): 
   - OK : Given the new file `config.ini` :  
   - OK : When I run `./rpl -i FR UK config.ini`    
   - OK : Then the `config.ini` file contains   
   - [X] scenario   [1.1 : simple use (single file, no globbing)](../../examples/rpl_case_insensitivity.md) pass  


# Document: [sut_version.md](../../examples/sut_version.md)  
   ### Scenario: [I want to know sut version 1/2](../../examples/sut_version.md): 
   - OK : When I run `./sut --version`  
   - OK : Then the output contains `version 1.0`  
   - [X] scenario   [I want to know sut version 1/2](../../examples/sut_version.md) pass  

   ### Scenario: [I want to know sut version 2/2](../../examples/sut_version.md): 
   - OK : When I run `./sut -v`  
   - OK : Then the output contains `version 1.0`  
   - [X] scenario   [I want to know sut version 2/2](../../examples/sut_version.md) pass  


# Document: [gcc_hello_world.md](../../examples/gcc_hello_world.md)  
   ### Scenario: [1: Checking installed gcc version](../../examples/gcc_hello_world.md): 
   - OK : When I run `gcc --version`  
   - OK : Then the output matches `(gcc|.* clang).*`  
   - [X] scenario   [1: Checking installed gcc version](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `main.c`  
   - OK : And given there is no `./main` file  
   - OK : When I successfully run `gcc main.c -o main`  
   - OK : And  I run `./main`  
   - OK : Then the output is `Hello, World!`  
   - [X] scenario   [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [3 : get gcc version](../../examples/gcc_hello_world.md): 
   - OK : When I run `gcc -v`  
   - OK : Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
   - [X] scenario   [3 : get gcc version](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [4 : checking error output and return code](../../examples/gcc_hello_world.md): 
   - OK : Given there is no `missing.c` file  
   - OK : Given the environment variable `LC_ALL` is set to `C`  
   - OK : When I run `gcc missing.c -o missing`  
   - OK : Then the exit code is `1`  
   - OK : And the error output contains `No such file or directory`  
   - [X] scenario   [4 : checking error output and return code](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [5 : checking environment variables handling](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `inc/myheader.h` containing  
   - OK : Given the new file `cp.c` containing  
   - OK : Given the environment variable `CPATH` is set to `inc`  
   - OK : When I successfully run `gcc cp.c -o cp`  
   - OK : When I run `./cp`  
   - OK : Then the output is `Hello from CPATH`  
   - [X] scenario   [5 : checking environment variables handling](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [6 : interacting with a program prompt](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `ask.c` containing  
   - OK : And given there is no `./ask` file  
   - OK : When I successfully run `gcc ask.c -o ask`  
   - OK : When I run `./ask`  
   - OK : Then the output is `What is your name?`  
   - OK : Then the output is  
   - OK : Then the output is `Continuing!`  
   - OK : And the exit code is `0`  
   - [X] scenario   [6 : interacting with a program prompt](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [7 : ignoring a volatile part of the output](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `timed.c` containing  
   - OK : And given there is no `./timed` file  
   - OK : When I successfully run `gcc timed.c -o timed`  
   - OK : And  I run `./timed`  
   - OK : Then I get (ignoring time)  
   - [X] scenario   [7 : ignoring a volatile part of the output](../../examples/gcc_hello_world.md) pass  


## Summary : **Fail**

| Status     | Count |
|------------|-------|
| Failed     | 1     |
| Successful | 16    |
| Empty      | 0     |
| Not Run    | 0     |

