
# Document: [gcc_hello_word.md](..\..\examples\gcc_hello_word.md)  
   ### Scenario: [gcc version?](..\..\examples\gcc_hello_word.md): 
   - OK : When I run `gcc -v`  
   - OK : Then the output contains `version `  
   - OK : Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
   - [X] scenario   [gcc version?](..\..\examples\gcc_hello_word.md) pass  

   ### Scenario: [compiling and executing an hello word](..\..\examples\gcc_hello_word.md): 
   - OK : Given the new file `main.c`  
   - OK : And given there is no `./main` file  
   - OK : When I successfully run `gcc main.c -o main`  
   - OK : And  I run `./main`  
   - OK : Then the output is `Hello, World!`  
   - [X] scenario   [compiling and executing an hello word](..\..\examples\gcc_hello_word.md) pass  


# Document: [rpl_case_insensitivity.md](..\..\examples\rpl_case_insensitivity.md)  
  ## Feature: 1 : Case insensitivity  
   ### Scenario: [1.1 : simple use (single file, no globbing)](..\..\examples\rpl_case_insensitivity.md): 
   - OK : Given the new file `config.ini` :  
   - OK : When I run `rpl -i FR UK config.ini`    
   - OK : Then the `config.ini` file contains   
   - [X] scenario   [1.1 : simple use (single file, no globbing)](..\..\examples\rpl_case_insensitivity.md) pass  


# Document: [sut_version.md](..\..\examples\sut_version.md)  
   ### Scenario: [I want to know sut version](..\..\examples\sut_version.md): 
   - OK : When I run `./sut --version`  
   - OK : Then the output contains `version 1.0`  
   - [X] scenario   [I want to know sut version](..\..\examples\sut_version.md) pass  


# Document: [example.md](..\..\..\src\help\example.md)  
  ## Feature: grep matching may be case insensitive  
   ### Background: [](..\..\..\src\help\example.md): 
   - OK : Given the new file `flowers.lst`  
   - [X] background [](..\..\..\src\help\example.md) pass  

   ### Scenario: [default case sensitive matching](..\..\..\src\help\example.md): 
   - OK : When I run `grep rose flowers.lst`  
   - OK : Then I get   
   - [X] scenario   [default case sensitive matching](..\..\..\src\help\example.md) pass  

   ### Background: [](..\..\..\src\help\example.md): 
   - OK : Given the new file `flowers.lst`  
   - [X] background [](..\..\..\src\help\example.md) pass  

   ### Scenario: [case insensitive matching](..\..\..\src\help\example.md): 
   - OK : When I run `grep -i rose flowers.lst`  
   - OK : Then I get   
   - [X] scenario   [case insensitive matching](..\..\..\src\help\example.md) pass  


## Summary : **Success**, 6 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 6     |
| Empty      | 0     |
| Not Run    | 0     |

