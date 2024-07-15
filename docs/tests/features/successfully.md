## Feature : The “successfully” shortcut

`when I successfully run 'X'`

is a shortcut to :

`When I run 'X'`
`Then I get no error`

### Scenario : *when I successfully run* a command with successful run

- When I successfully run `uut --version`
- Then I get `uut version 1.0`

### Scenario : *when I successfully run* a command with a wrong command line, returns an error status

This should fail, even if the post condition is satisfied, because of "successfully"

- Given the `vza.input` file
```md
# Scenario: Wrong command line
- When I successfully run `uut -vza`
- Then I get `unknown option -vza`
```
- When I run `bbt vza.input`
- Then I get an error

### Scenario : *when I run* a command with a wrong command line

Same test without "successfully" should pass

- When I run `uut -vza`
- Then I get `unknown option -vza`


