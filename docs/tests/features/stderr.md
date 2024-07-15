## Feature : stderr test

Test an app returning an error message on stderr

### Scenario outline : unknown option

  - When I run `uut -qsd`
  - Then I get `unknown option -qsd` on stderr