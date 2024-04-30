## Feature : checking a message line on stdout

Test an app returning a single line message

### Scenario outline : asking for uut version

  - When I run `uut -v`
  - Then I get `version 1.0`