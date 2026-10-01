# gcc simple sanity tests

### Scenario 1: Checking installed gcc version

> Let's start with the simpliest possible scenario, running a command and checking the output

- When I run `gcc --version`
- Then the output contains `14.2.0`

## Scenario 2 : compiling and executing an hello word

> This scenario illustrate basic bbt's features, creating an input file, checking that some file doesn't exist before running a command, checking that the command exit with an OK status, checking the output.

Sanity check of a complete compile / link / run sequence :

- Given the new file `main.c`
  ```c
  #include <stdio.h>
  int main() {
  printf("Hello, World!");
  return 0;
  }
  ```
- And given there is no `./main` file

- When I successfully run `gcc main.c -o main`
- And  I run `./main`

- Then the output is `Hello, World!`

## Scenario 2 : get gcc version
  
> This scenario illustrate the use of pattern matching


  On Linux or Windows, `gcc -v` output contains something like: 
  > gcc version 14.2.0 (Debian 14.2.0-16)  

  On Darwin:  
  > Apple clang version 12.0.0 (clang-1200.0.32.29)  

Let's use a regexp to test both.

- When I run `gcc -v`
- Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`

## Scenario 3 : checking error output and return code

> This scenario illustrates how to verify both the exit code and stderr output of a command.

- Given there is no `missing.c` file
- When I run `gcc missing.c -o missing`
- Then the exit code is 1
- And the error output contains `No such file or directory`

## Scenario 4 : checking environment variables handling

> This scenario illustrates how to set environment variables and verify localized error messages.

- Given there is no `missing.c` file
  
- Given the environment variable `LC_ALL` is set to `en_UK.UTF-8`
- When I run `gcc missing.c -o missing`
- Then the error output contains `No such file or directory`

- Given the environment variable `LC_ALL` is set to `fr_FR.UTF-8`
- When I run `gcc missing.c -o missing`
- Then the error output contains `Aucun fichier ou dossier de ce type`
