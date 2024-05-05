## Feature : test the “successfully” shortcut

`when I run successfully 'X'`

should be equivalent to

`When I run 'X'`
`Then I get no error`

### Scenario : Test the *when I successfully run* command with successful run

- When I successfully run `uut --version`
- Then I get `version 1.0`

### Scenario : Test the *when I successfully run* command with a wrong command line (should fail)

- When I successfully run `uut -vza`
- Then I get `unknown option -vza`
 
This should fail, even if the post condition is satisfied, because of "successfully"

### Scenario : Test the *when I run* command with a wrong command line
- When I run `uut -vza`
- Then I get `unknown option -vza`

Same test without "successfully" should pass

