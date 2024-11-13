## Feature : checking a message line on stdout

First thing to test, a single line message on standard output!

### Scenario : asking for sut version

  - When I run `sut -v`
  - Then I get `sut version 1.0`