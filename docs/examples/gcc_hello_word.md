# gcc simple sanity tests

## Scenario: gcc version?
  
Let start with something easy :

- When I run `gcc -v`

- Then the output contains `gcc version `

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
- And given there is no `main` file

- When I successfully run `gcc main.c -o main`
- And  I run `./main`

- Then the output is `Hello, World!`
