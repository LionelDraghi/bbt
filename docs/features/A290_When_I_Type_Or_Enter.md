<!-- omit from toc -->
## Feature: interaction with the command under test

Some programs wait for user input before terminating: a confirmation
answered with a single key, a question answered with a whole line.
A scenario needs to provide this input, and to check the behavior of
the program resulting from this input.

Two `When` steps send text to the standard input of the command
started by the last `When I run` step:

- **When I type** sends the text without a trailing newline, as if the
  user had just pressed the keys. For instance, When I type `Y`
  sends `Y`: this is for programs expecting a single key press.

- **When I enter** sends the text followed by a newline, as if the
  user had typed it then pressed `Enter`. For instance, When I enter
  `Y` sends `Y` followed by the line terminator: this is for programs
  reading a whole line.

  The `enter` step is thus equivalent to the `type` step, followed by
  the line terminator.

The output of the program is a consequence of its input: the output
checks following a `type` or `enter` step apply only to the output
produced after this step. The prompt displayed before the input is
ignored: each `type` or `enter` step resets the checked output.

The command input and output are pipes: the program under test is
expected to flush its prompt before waiting for input (this is the
default with `Ada.Text_IO`; a C program needs `fflush`).

_Table of Contents:_
- [Scenario: answering a key prompt with type](#scenario-answering-a-key-prompt-with-type)
- [Scenario: refusing with type](#scenario-refusing-with-type)
- [Scenario: several inputs in a row](#scenario-several-inputs-in-a-row)
- [Scenario: answering a line prompt with enter](#scenario-answering-a-line-prompt-with-enter)
- [Scenario: enter without a running command](#scenario-enter-without-a-running-command)

### Scenario: answering a key prompt with type

`sut delete` asks for a confirmation, and waits for a single key:
no `Enter` is expected, and the prompt is not part of the checked
output.

- Given the new file `to_delete.txt` containing `some data`
- When I run `./sut delete to_delete.txt`
- Then the output is
```
Delete to_delete.txt? [Y]es/[N]o
```
- When I type `Y`
- Then the output is `Deleting to_delete.txt`
- And there is no file `to_delete.txt`

### Scenario: refusing with type

The output produced after the refusal is checked: nothing is expected.

- Given the new file `to_keep.txt` containing `some data`
- When I run `./sut delete to_keep.txt`
- When I type `N`
- Then there is no output
- And there is a file `to_keep.txt`

### Scenario: several inputs in a row

Several inputs may be sent to the same running command. An invalid
key makes `sut delete` ask again: only the output produced after the
last input step is checked.

- Given the new file `to_delete.txt` containing `some data`
- When I run `./sut delete to_delete.txt`
- When I type `X`
- When I type `Y`
- Then the output is `Deleting to_delete.txt`
- And there is no file `to_delete.txt`

### Scenario: answering a line prompt with enter

`sut rename` asks for the new file name, and waits for a whole
line. The question is displayed before the input: it is ignored by
the output check following the `enter` step.

The new file name is created by the command, not by a `Given` step:
it is not tracked by the cleanup, so let's make sure it does not
exist, even after a previous run.

- Given there is no `new_name.txt` file
- Given the new file `old_name.txt` containing `some data`
- When I run `./sut rename old_name.txt`
- Then the output is
```
Rename old_name.txt to:
```
- When I enter `new_name.txt`
- Then the output is `Renamed to new_name.txt`
- And there is no file `old_name.txt`
- And there is a file `new_name.txt`

### Scenario: enter without a running command

Using `enter` or `type` when no command is running is an error.

- Given the `no_input.md` file
~~~md
# Scenario:
- When I enter `Y`
~~~
- When I run `./bbt -c no_input.md`
- Then I get an error
- And the output contains `no_input.md:2: Error : no command is running when reaching this step`
