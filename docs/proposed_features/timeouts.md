## Feature: bounding the tests execution time

Status: parked. The need is recorded here so that it is not lost;
the design will be arbitrated later. The prerequisite is in place:
the interactive steps (`type`, `enter`) and the deferred exit status
checks (cf. the design discussion D2) are implemented. The
granularity is already arbitrated (October 2026): per scenario,
see below.

### Motivations

A test suite must stay CI friendly: a regression that makes the
software under test hang, loop forever, or wait for an input that no
step provides should fail fast with an explicit message, not block
the whole pipeline.

Two recent evolutions make the risk more acute:

- a command started by a `run` step is no more guaranteed to be
  finished when the following steps execute: a command waiting for an
  input that no `type` or `enter` step provides blocks the scenario
  forever;
- an exit status check (`successfully`, `Then I get no error`) may
  have to wait for the command termination, possibly at the end of
  the scenario: a command taking a long time to exit stretches the
  whole test run.

### Arbitrated (October 2026)

- the timeout is per scenario, for instance
  `--scenario-timeout <duration>`. Rejected alternatives: a timer
  per step, which does not fit a command spanning several steps, and
  where a hang may only show on the deferred checks; a timer for the
  whole run, which stops bbt after the damage is done, instead of
  failing at the place where the hang blocks, where bbt can name the
  culprit.

### Proposals to arbitrate

- on expiry, the scenario fails with a message naming the hanging step;
- on expiry, the running child process is killed before bbt reports
  the failure, so that no process is left behind;
- the default is no timeout at all, so that bbt's behavior is
  unchanged unless the option is explicitly given.

### Open questions

- is the child process killed, or just reported and left to the
  operating system?
- should the timer cover the whole scenario, including the `Given`
  steps and the cleanup?
- how is a duration expressed (`5s`, `1m30s`, milliseconds)?
