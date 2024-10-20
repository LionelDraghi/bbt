## Feature : checking a message line on stdout

First thing to test, a single line message on standard output!

### Scenario : asking for uut version

  - When I run `uut -v`
  - Then I get `uut version 1.0`