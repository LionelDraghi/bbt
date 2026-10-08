<!-- omit from toc -->
## Feature: transient status bar and progress counter

`-sb | --status_bar` displays a transient status line at the current
terminal line: it is erased before any normal output line, and redrawn
after it, so that no residue is left over the normal output.

During a run, the bar shows a progress counter `<done>/<total>`,
counting scenarios the same way as the final counters: backgrounds
are not counted, as their results are folded into each scenario,
and filtered scenarios are counted, as they end up Not Run.

The bar is displayed only when the standard output is a terminal:
on a redirected output (pipe, file, CI log), it would only fill
the output with escape sequences. The undocumented `--force_status_bar`
debugging option restores it in that case.

As soon as a scenario fails, the bar turns red, and displays the
number of failed scenarios until the end of the run: the following
ticks, and the following scenarios, cannot erase the failure from
the user's view.

_Table of Contents:_
- [Scenario: backgrounds are not counted](#scenario-backgrounds-are-not-counted)
- [Scenario: filtered scenarios are counted](#scenario-filtered-scenarios-are-counted)
- [Scenario: the normal output is preserved](#scenario-the-normal-output-is-preserved)
- [Scenario: the bar is disabled when the standard output is not a terminal](#scenario-the-bar-is-disabled-when-the-standard-output-is-not-a-terminal)
- [Scenario: the failure stays visible](#scenario-the-failure-stays-visible)

### Scenario: backgrounds are not counted

The nested bbt runs a file made of a background and one scenario
with the status bar: the counter counts the scenario only, and
reaches 1/1.

- Given the new `sb.md` file
~~~md
# Background: setup

- Given the file `input.txt` containing `sb`

# Scenario: one

- When I run `./sut --version`
- Then I get no error
~~~
- When I run `./bbt -q -c -sb --force_status_bar sb.md`
- Then I get no error
- And the output contains `1/1`
- And the output does not contain `2/2`

### Scenario: filtered scenarios are counted

The nested bbt runs a two scenarios file with the status bar and
a filter selecting only one scenario: the filtered scenario is
visited, and the counter reaches 2/2, while the final counters
report one scenario OK and one Not Run.

- Given the new `sb_filtered.md` file
~~~md
# Scenario: one

- When I run `./sut --version`
- Then I get no error

# Scenario: two

- When I run `./sut --version`
- Then I get no error
~~~
- When I run `./bbt -q -c -sb --force_status_bar -s one sb_filtered.md`
- Then I get no error
- And the output contains `2/2`
- And the output contains `1 scenarios OK`

### Scenario: the normal output is preserved

With the status bar, the activity in progress is displayed while
analyzing the documents, and the summary is left unaltered by the
bar.

- Given the new `sb.md` file
~~~md
# Background: setup

- Given the file `input.txt` containing `sb`

# Scenario: one

- When I run `./sut --version`
- Then I get no error
~~~
- When I run `./bbt -q -c -sb --force_status_bar sb.md`
- Then I get no error
- And the output contains `Analyzing documents`
- And the output contains `## Summary : **Success**, 1 scenarios OK`

### Scenario: the bar is disabled when the standard output is not a terminal

The nested bbt runs with the status bar, but its standard output is
a pipe: the bar is not displayed, and the output contains only the
normal results.

- Given the new `sb.md` file
~~~md
# Background: setup

- Given the file `input.txt` containing `sb`

# Scenario: one

- When I run `./sut --version`
- Then I get no error
~~~
- When I run `./bbt -q -c -sb sb.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`
- And the output does not contain `1/1`
- And the output does not contain `Analyzing documents`

### Scenario: the failure stays visible

The nested bbt runs a three scenarios file with the status bar, the
second scenario failing: from the failure on, the bar displays the
failure count, and keeps displaying it after the third scenario
runs OK, until the end of the run.

- Given the new `sb_failed.md` file
~~~md
# Scenario: ok1

- When I run `./sut --version`
- Then I get no error

# Scenario: ko

- When I run `./sut --version`
- Then I get an error

# Scenario: ok2

- When I run `./sut --version`
- Then I get no error
~~~
- When I run `./bbt -q -k -c -sb --force_status_bar sb_failed.md`
- Then I get an error
- And the output contains `## Summary : **Fail**`
- And the output contains `1 failed`
