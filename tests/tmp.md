
# [gcc_hello_word.md](../docs/examples/gcc_hello_word.md)  
    OK  : When I run `gcc -v`
    OK  : Then the output contains `version `

  ### scenario [gcc version?](../docs/examples/gcc_hello_word.md)  
  - [X] Given the new file `main.c`
  - [X] And given there is no `main` file
  - [X] When I successfully run `gcc main.c -o main`
  - [ ] When I successfully run `gcc main.c -o main`
  - [X] And  I run `./main`
  - [ ] Then the output is `Hello, World!`
  ### **[gcc version?](../docs/examples/gcc_hello_word.md) fail**

  - [X] scenario [compiling and executing an hello word](../docs/examples/gcc_hello_word.md) pass  

## [rpl_case_insensitivity.md](../docs/examples/rpl_case_insensitivity.md)  

  ### Feature: 1 : Case insensitivity  

    OK  : Given the new file `config.ini` :
    OK  : When I run `rpl -i FR UK config.ini`
    OK  : Then the `config.ini` file contains 
  - [X] scenario [1.1 : simple use (single file, no globbing)](../docs/examples/rpl_case_insensitivity.md) pass  

## [sut_version.md](../docs/examples/sut_version.md)  

    OK  : When I run `./sut --version`
    OK  : Then the output contains `version 1.0`
  - [X] scenario [I want to know sut version](../docs/examples/sut_version.md) pass  

# Summary
| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 4     |
| Empty      | 0     |
