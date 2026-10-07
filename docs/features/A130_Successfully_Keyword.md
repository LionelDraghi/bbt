<!-- omit from toc -->
## Feature : The “successfully” shortcut

`when I successfully run 'X'`

is a handy shortcut to :

`When I run 'X'`  
`Then I get no error`

Credit : I don't know who invented this, but I borrowed the idea from [Aruba](https://github.com/cucumber/aruba/tree/main/features/).

When the command is fed interactively ([A290](A290_When_I_Type_Or_Enter.md)),
it may still be running long after its `run` step, and the shortcut
keeps its exact meaning: the exit status check is deferred to the
next synchronization point, or to the end of the scenario, where it
waits for the command termination. A failure detected later is
reported on the `successfully run` step line, in the temporal order
of its detection, and the step being run when the failure is detected
is not executed, instead of a misleading `no command is running`
message (cf. the design discussion D2).

_Table of Contents:_
- [Scenario : *when I successfully run* a command with successful run](#scenario--when-i-successfully-run-a-command-with-successful-run)
- [Scenario : *when I successfully run* a command with a wrong command line, returns an error status](#scenario--when-i-successfully-run-a-command-with-a-wrong-command-line-returns-an-error-status)
- [Scenario : *when I run* a command with a wrong command line](#scenario--when-i-run-a-command-with-a-wrong-command-line)
- [Scenario : *when I successfully run* an interactive command that finally exits cleanly](#scenario--when-i-successfully-run-an-interactive-command-that-finally-exits-cleanly)
- [Scenario : *when I successfully run* an interactive command that finally fails](#scenario--when-i-successfully-run-an-interactive-command-that-finally-fails)
- [Scenario : the deferred failure is reported before the next step](#scenario-the-deferred-failure-is-reported-before-the-next-step)
- [Scenario : successfully run, then an explicit no error check](#scenario-successfully-run-then-an-explicit-no-error-check)

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

### Scenario : *when I successfully run* an interactive command that finally exits cleanly

The command is still running at the `successfully run` step: the
exit status check is deferred, and resolved at the next step, once
the command has terminated.

- Given the new file `to_delete.txt` containing `some data`
- When I successfully run `./sut delete to_delete.txt`
- When I type `Y`
- Then the output is `Deleting to_delete.txt`
- And there is no file `to_delete.txt`

### Scenario : *when I successfully run* an interactive command that finally fails

The command fails once the answer is sent: the deferred exit status
check fails, and the failure references the `successfully run` step
line, not the last step of the scenario.

- Given the new `interactive_fail.md` file
~~~md
# Scenario: interactive command finally fails

- Given the new file `to_keep.txt` containing `some data`
- When I successfully run `./sut delete to_keep.txt`
- When I type `N`
~~~
- When I run `./bbt -q -c --yes interactive_fail.md`
- Then I get an error
- And the output contains `Unsuccessfully run "./sut delete to_keep.txt"`

### Scenario : the deferred failure is reported before the next step

The command exits with an error between two steps: the failure is
reported on the `successfully run` step, the current step is not
executed, and no misleading `no command is running` message is
displayed.

- Given the new `deferred_fail.md` file
~~~md
# Scenario: the command fails before the next step

- Given the new file `to_keep.txt` containing `some data`
- When I successfully run `./sut delete to_keep.txt`
- When I type `N`
- When I type `Y`
~~~
- When I run `./bbt -q -c --yes deferred_fail.md`
- Then I get an error
- And the output contains `Unsuccessfully run "./sut delete to_keep.txt"`
- And the output does not contain `no command is running`

### Scenario : successfully run, then an explicit no error check

`successfully run` is the shortcut for the run followed by
``Then I get no error ``: after the interactive steps, the explicit
check waits for the command termination before being evaluated.

- Given the new file `to_delete.txt` containing `some data`
- When I successfully run `./sut delete to_delete.txt`
- When I type `Y`
- Then I get no error


