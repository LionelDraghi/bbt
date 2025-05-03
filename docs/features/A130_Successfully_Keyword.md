<!-- omit from toc -->
## Feature : The “successfully” shortcut

`when I successfully run 'X'`

is a handy shortcut to :

`When I run 'X'`  
`Then I get no error`

Credit : I don't know who invented this, but I borrowed the idea from [Aruba](https://github.com/cucumber/aruba/tree/main/features/).

_Table of Contents:_
- [Scenario : *when I successfully run* a command with successful run](#scenario--when-i-successfully-run-a-command-with-successful-run)
- [Scenario : *when I successfully run* a command with a wrong command line, returns an error status](#scenario--when-i-successfully-run-a-command-with-a-wrong-command-line-returns-an-error-status)
- [Scenario : *when I run* a command with a wrong command line](#scenario--when-i-run-a-command-with-a-wrong-command-line)

### Scenario : *when I successfully run* a command with successful run

- When I successfully run `./sut --version`
- Then I get `sut version 1.0`

### Scenario : *when I successfully run* a command with a wrong command line, returns an error status

This should fail, even if the post condition is satisfied, because of "successfully"

- Given the `vza.input` file
```md
# Scenario: Wrong command line
- When I successfully run `./sut -vza`
- Then I get `unknown option -vza`
```
- When I run `./bbt vza.input`
- Then I get an error

### Scenario : *when I run* a command with a wrong command line

Same test without "successfully" should pass

- When I run `./sut -vza`
- Then I get `unknown option -vza`


