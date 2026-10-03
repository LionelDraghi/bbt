
# Document: [gcc_hello_world.md](../../examples/gcc_hello_world.md)  
   ### Scenario: [1: Checking installed gcc version](../../examples/gcc_hello_world.md): 
   - OK : When I run `gcc --version`  
   - OK : Then the output contains `Free Software Foundation, Inc.`  
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


# Document: [rpl_case_insensitivity.md](../../examples/rpl_case_insensitivity.md)  
  ## Feature: 1 : Case insensitivity  
   ### Scenario: [1.1 : simple use (single file, no globbing)](../../examples/rpl_case_insensitivity.md): 
   - OK : Given the new file `config.ini` :  
   - OK : When I run `rpl -i FR UK config.ini`    
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
   - OK : Then the output contains `Free Software Foundation, Inc.`  
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


## Summary : **Success**, 13 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 13    |
| Empty      | 0     |
| Not Run    | 0     |

