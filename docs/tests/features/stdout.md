## Feature : checking a message line on stdout

Test an app returning a single line message

### Scenario : asking for uut version

  - When I run `uut -v`
  - Then I get `uut version 1.0`