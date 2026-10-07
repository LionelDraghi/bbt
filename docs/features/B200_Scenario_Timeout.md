<!-- omit from toc -->
## Feature: bounding the tests execution time

A test suite must stay CI friendly: a regression that makes the
software under test hang, loop forever, or wait for an input that no
step provides should fail fast with an explicit message, not block
the whole run.

The `--scenario_timeout <duration>` option bounds the time spent on
each scenario, backgrounds included. On expiry, the still running
command is killed, so that no process is left behind, and the
scenario fails: the failure is reported on the hanging step line.
The duration is a number of seconds, with an optional unit: `s`,
`m` or `h` (for instance `30`, `10s`, `2m`, `1m30s`). There is no
timeout by default: bbt's behavior is unchanged unless the option is
explicitly given.

_Table of Contents:_
- [Scenario: a hanging command fails the scenario on timeout](#scenario-a-hanging-command-fails-the-scenario-on-timeout)
- [Scenario: the deferred exit status check waits no more than the timeout](#scenario-the-deferred-exit-status-check-waits-no-more-than-the-timeout)
- [Scenario: the duration is a number of seconds, with an optional unit](#scenario-the-duration-is-a-number-of-seconds-with-an-optional-unit)

### Scenario: a hanging command fails the scenario on timeout

The nested bbt runs a command that never terminates: at the expiry
of the scenario timeout, the command is killed and the scenario
fails, the failing step naming the timeout.

- Given the new `hang.md` file
~~~md
# Scenario: hang

- When I run `./sut delay 10`
- Then I get no error
~~~
- When I run `./bbt -q -c --scenario_timeout 1 hang.md`
- Then I get an error
- And the output contains `the scenario timeout of 1s expired`

### Scenario: the deferred exit status check waits no more than the timeout

The command is started by a `successfully run` step, and no step
answers its prompt: the deferred exit status check waits for its
termination at the end of the scenario, no more than the scenario
timeout. The command is killed, and the failure is reported on the
`successfully run` step line.

- Given the new file `to_keep.txt` containing `some data`
- Given the new `deferred_hang.md` file
~~~md
# Scenario: the command never answers

- Given the new file `to_keep.txt` containing `some data`
- When I successfully run `./sut delete to_keep.txt`
- When I type `X`
~~~
- When I run `./bbt -q -c --yes --scenario_timeout 1 deferred_hang.md`
- Then I get an error
- And the output contains `the scenario timeout of 1s expired before the command termination`

### Scenario: the duration is a number of seconds, with an optional unit

- Given the new `quick.md` file
~~~md
# Scenario: quick

- When I run `./sut --version`
~~~
- When I run `./bbt -q -c --scenario_timeout 1m30s quick.md`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- When I run `./bbt -q --scenario_timeout xyz quick.md`
- Then I get an error
- And the output contains `invalid duration`
