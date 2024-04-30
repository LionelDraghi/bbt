## Feature : test the “successfully” shortcut

  `when I run successfully 'X'`

should be equivalent to

`When I run 'X'`

`Then I get no error`

### Scenario outline : successfully

Test the *when I successfully run* command with a wrong command line
  - When I successfully run `uut -vza`
  - Then I get `unknown option -vza`

 