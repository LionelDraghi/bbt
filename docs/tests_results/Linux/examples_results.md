
# Document: [gcc_hello_world.md](../../examples/gcc_hello_world.md)  
   ### Scenario: [1 : get gcc version](../../examples/gcc_hello_world.md): 
   - OK : When I run `gcc -v`  
   - OK : Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
   - [X] scenario   [1 : get gcc version](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `main.c`  
   - OK : And given there is no `./main` file  
   - OK : When I successfully run `gcc main.c -o main`  
   - OK : And  I run `./main`  
   - OK : Then the output is `Hello, World!`  
   - [X] scenario   [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md) pass  


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
   ### Scenario: [1 : get gcc version](../../examples/gcc_hello_world.md): 
   - OK : When I run `gcc -v`  
   - OK : Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
   - [X] scenario   [1 : get gcc version](../../examples/gcc_hello_world.md) pass  

   ### Scenario: [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md): 
   - OK : Given the new file `main.c`  
   - OK : And given there is no `./main` file  
   - OK : When I successfully run `gcc main.c -o main`  
   - OK : And  I run `./main`  
   - OK : Then the output is `Hello, World!`  
   - [X] scenario   [2 : compiling and executing an hello word](../../examples/gcc_hello_world.md) pass  


## Summary : **Success**, 7 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 7     |
| Empty      | 0     |
| Not Run    | 0     |

