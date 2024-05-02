## Feature : test the “successfully” shortcut

`when I run successfully 'X'`

should be equivalent to

`When I run 'X'`
`Then I get no error`



### Scenario outline : successfully

Test the *when I successfully run* command with successful run
- When I successfully run `uut --version`
- Then I get `version v1.9`

Test the *when I successfully run* command with a wrong command line
- When I successfully run `uut -vza`
- Then I get `unknown option -vza`
 
This should fail, even if the post condition is satisfied, because of "successfully"
Same test without "successfully" should pass :

Test the *when I run* command with a wrong command line
- When I run `uut -vza`
- Then I get `unknown option -vza`
 
