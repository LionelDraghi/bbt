## Feature : stderr test

Test expected msg on the error output.  

### Scenario : unknown option

  - When I run `uut -qsd`
  - Then I get `unknown option -qsd` on stderr