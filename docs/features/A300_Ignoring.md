<!-- omit from toc -->
# Feature: Ignoring volatile parts in comparisons

Expected and actual texts frequently differ on parts that are *volatile*: a
time tag that changes at each run, an absolute path that depends on the
machine. The workarounds — rewriting the whole line as a `matches` regexp,
filtering the actual output through `sed` before comparing — are heavier than
the need and weaken the check on the stable part.

The comparison steps accept an **ignoring clause** that states which parts of
the comparison are volatile:

```
- Then I get (ignoring time)
  ```
  [12:34:56] Building foo
  [12:34:57] Done
  ```
```

This feature file specifies the first increment of the
[ignoring proposal](../proposed_features/ignore.md): the `time` and `dir`
patterns, on the step clause only. The wider scopes (a `Given I ignore ...`
step, a command line option), the `path syntax` and `<regexp>` patterns and
the precedence between scopes are deferred, as is the homogenization with
the existing `ignore_casing` and co. settings — which, when resumed, should
bring their behavior explicitly into the scenario instead of leaving it on
the command line, as wished when
[issue #18](https://github.com/LionelDraghi/bbt/issues/18) was closed. The
colored diff display of the ignored segments pairs with the
[LCS alignment proposal](../proposed_features/LCS_diff_alignment.md).

## Clause syntax

The ignoring clause comes at the end of a comparison step, between
parentheses:

- after the expected code span: ``- Then I get `sut home is C:\prj\bbt` (ignoring dir) ``
- before the expected code block:

  ```
  - Then I get (ignoring time)
  ```

- after the operands of a file comparison: ``- Then file `a.txt` is equal to file `b.txt` (ignoring time) ``

Several patterns combine on one clause, separated by commas, spaces or
`and`, in any mix: `(ignoring time, dir)`, `(ignoring time and dir)`,
and, once the deferred `casing` pattern lands,
`(ignoring dir, time and casing)`. The patterns of this increment are
`time` and `dir`.

The clause applies to the steps that compare a text with an expected text:
`I get`, `I get on stderr`, the `output is` / `output contains` family, and
the `file is equal to` / `file contains` family. The negated forms
(`does not contain`...) do not take the clause: ignoring a pattern in a
non-containment check is meaningless. The `matches` steps do not take it
either: the expected text is already a pattern there.

## Semantics: masking on both sides

Before the comparison proper, every substring matching an ignore pattern is
replaced by a fixed marker, **in the expected text and in the actual one**.
The comparison then proceeds on the masked texts, with the current match
settings (`human match`, `ignore_casing`...): masking happens first, the two
mechanisms are orthogonal.

The expected text keeps a sample of the volatile value: the author writes
what was actually printed, and flags the part that will vary. The step still
checks that something *was* printed where the volatile value stands: an
expected line reduced to nothing but the marker fails, and an actual line
missing the marker left by an expected time tag fails too. Only the exact
value of the volatile part is not checked.

A pattern that matches nothing on the actual side raises a warning: most
probably the scenario declares volatile something that the program no longer
prints, and the neutralization would silently weaken the test.

## `ignoring time`

`ignoring time` masks the common clock, date and date-time formats:

- a clock time: `HH:MM`, `HH:MM:SS`, `HH:MM:SS.S...` (one or more
  fractional digits);
- a date: `YYYY-MM-DD`;
- a date-time: a date immediately followed by `T` and a clock time,
  e.g. `2026-10-13T12:34:56`.

The surrounding text — brackets, the `at` of a sentence — is not masked,
only the value itself.

## `ignoring dir`

`ignoring dir` masks the directory part of a path. A path is a maximal
sequence of non blank characters containing at least one path separator,
`/` or `\`. Everything up to and including the last separator is masked;
the final component — the file or directory name — stays checked:

- `/home/lionel/prj/bbt` masks to a checked `bbt`;
- `C:\prj\bbt` masks to the same checked `bbt`, the drive letter being part
  of the masked prefix;
- `tests\results.md` masks to a checked `results.md`.

The rule is purely lexical: any non blank run containing a separator is
treated as a path, so a URL such as `https://example.net/page` masks to a
checked `page` as well. This consequence is accepted for now; it may be
refined together with the `path syntax` pattern.

Two subjects are entangled in a path: the *location* it designates, and
the *syntax* of its writing (`\` vs `/`, drive letter). `ignoring dir`
neutralizes both at once: the location is masked, and the masked prefix
takes its syntax away with it — a Unix and a Windows print of the same
place compare equal. A later, more specific pattern — the deferred
`path syntax` — will do the converse: require the *same* path on both
sides, and neutralize only the syntax (cf. the
[ignoring proposal](../proposed_features/ignore.md)).

A run without any separator (`bbt`) is not a path occurrence and is not
masked.

_Table of Contents:_
- [Background](#background)
- [Scenario: a time tag in the output does not fail the comparison](#scenario-a-time-tag-in-the-output-does-not-fail-the-comparison)
- [Scenario: the common clock, date and date-time formats are ignored](#scenario-the-common-clock-date-and-date-time-formats-are-ignored)
- [Scenario: only the file name is checked, not the directory](#scenario-only-the-file-name-is-checked-not-the-directory)
- [Scenario: a differing file name fails, differing directories do not](#scenario-a-differing-file-name-fails-differing-directories-do-not)
- [Scenario: a differing stable part still fails](#scenario-a-differing-stable-part-still-fails)
- [Scenario: time and dir combine on one step](#scenario-time-and-dir-combine-on-one-step)
- [Scenario: a time tag does not fail a contains check](#scenario-a-time-tag-does-not-fail-a-contains-check)
- [Scenario: a time tag missing on the actual side fails and warns](#scenario-a-time-tag-missing-on-the-actual-side-fails-and-warns)
- [Scenario: the clause on a non comparison step is an error](#scenario-the-clause-on-a-non-comparison-step-is-an-error)

## Background

- Given the file `build_ref.txt`
  ```
  [12:34:56] Building foo
  [12:34:57] Done
  ```

- Given the file `build_wrong.txt`
  ```
  [13:45:12] Building bar
  [13:45:14] Done
  ```

- Given the file `build_no_time.txt`
  ```
  Building foo
  Done
  ```

- Given the file `where_ref.txt`
  ```
  sut home is /home/lionel/prj/bbt
  ```

- Given the file `where_other_name.txt`
  ```
  sut home is /home/lionel/prj/other
  ```

### Scenario: a time tag in the output does not fail the comparison

- Given the file `log.txt`
  ```
  [13:45:12] Building foo
  [13:45:14] Done
  ```

- When I run `./sut read log.txt`
- Then I get (ignoring time)
  ```
  [12:34:56] Building foo
  [12:34:57] Done
  ```

The time tags differ between the actual output and the expected text, and
the same scenario passes whatever the run hour is.

### Scenario: the common clock, date and date-time formats are ignored

- Given the file `run_a.txt`
  ```
  Build started at 12:34, finished at 12:34:56.78 on 2026-10-13
  all logged 2026-10-13T12:34:56
  ```

- Given the file `run_b.txt`
  ```
  Build started at 23:45, finished at 23:45:01.12 on 2027-01-02
  all logged 2027-01-02T23:45:01
  ```

- Then file `run_a.txt` is equal to file `run_b.txt` (ignoring time)

### Scenario: only the file name is checked, not the directory

- Given the file `where.txt`
  ```
  sut home is /home/lionel/prj/bbt
  ```

- When I run `./sut read where.txt`
- Then I get `sut home is C:\prj\bbt` (ignoring dir)

The same scenario passes on Unix and on Windows: the directory parts
`/home/lionel/prj/` and `C:\prj\` are masked, the `bbt` name is checked.

### Scenario: a differing file name fails, differing directories do not

- Given the `where_compare.md` file
  ~~~
  # Scenario:
  - Then file `where_ref.txt` is equal to file `where_other_name.txt` (ignoring dir)
  ~~~

- When I run `./bbt where_compare.md`
- Then I get an error
- And output contains `Error : where_ref.txt not equal to expected:`

The directories `/home/lionel/prj/` are identical here, but the final names
`bbt` and `other` differ: unlike `ignoring path syntax`, the name after the
masked directory stays checked.

### Scenario: a differing stable part still fails

- Given the `time_compare.md` file
  ~~~
  # Scenario:
  - Then file `build_ref.txt` is equal to file `build_wrong.txt` (ignoring time)
  ~~~

- When I run `./bbt time_compare.md`
- Then I get an error
- And output contains `Error : build_ref.txt not equal to expected:`

The time tags are neutralized, but `Building foo` and `Building bar` differ:
the ignoring clause never weakens the stable part of the check.

### Scenario: time and dir combine on one step

- Given the file `build_log.txt`
  ```
  [13:45:12] Building /home/lionel/prj/bbt/foo.o
  [13:45:14] Done
  ```

- When I run `./sut read build_log.txt`
- Then I get (ignoring time, dir)
  ```
  [12:34:56] Building C:\prj\bbt\foo.o
  [12:34:57] Done
  ```
- And I get (ignoring time and dir)
  ```
  [12:34:56] Building C:\prj\bbt\foo.o
  [12:34:57] Done
  ```

### Scenario: a time tag does not fail a contains check

- Given the file `log.txt`
  ```
  [13:45:12] Building foo
  [13:45:14] Done
  ```

- When I run `./sut read log.txt`
- Then the output contains (ignoring time)
  ```
  [12:34:56] Building foo
  ```

### Scenario: a time tag missing on the actual side fails and warns

- Given the `warn_compare.md` file
  ~~~
  # Scenario:
  - Then file `build_no_time.txt` is equal to file `build_ref.txt` (ignoring time)
  ~~~

- When I run `./bbt warn_compare.md`
- Then I get an error
- And output contains `Warning`

The expected text has time tags, the actual one has none: the comparison
fails — something should have been printed where the marker stands — and a
warning draws the attention on the pattern that matched nothing on the
actual side.

### Scenario: the clause on a non comparison step is an error

- Given the `bad_clause.md` file
  ~~~
  # Scenario:
  - Given the file `foo.txt` (ignoring time)
    ```
    hello
    ```
  ~~~

- When I run `./bbt bad_clause.md`
- Then I get an error
- And output contains `the (ignoring ...) clause is only allowed on the`
- And output contains `comparison steps`

The clause tunes a comparison: on any other step, it is a scenario error,
not a silently ignored trailing text.
