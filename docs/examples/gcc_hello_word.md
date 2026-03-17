# gcc simple sanity tests

## Scenario: gcc version?
  
  On Linux or Windows, the output is something like: 
  > gcc version 14.2.0 (Debian 14.2.0-16)  

  On Darwin:  
  > Apple clang version 12.0.0 (clang-1200.0.32.29)  

Let's use a regexp to test both:  

- When I run `gcc -v`
- Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`

## Scenario: compiling and executing an hello word

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
