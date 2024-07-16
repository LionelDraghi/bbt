# gcc simple tests

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
