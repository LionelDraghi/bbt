<!-- omit from toc -->
# Feature: Ignoring volatile parts in comparisons

Expected and actual texts frequently differ on parts that are *volatile*: a
time tag that changes at each run, an absolute path that depends on the
machine. The workarounds — rewriting the whole line as a `matches` regexp,
filtering the actual output through `sed` before comparing — are heavier than
the need and weaken the check on the stable part.

The comparison steps accept an **ignoring clause** that states which parts of
the comparison are volatile:

~~~
- Then I get (ignoring time)
  ```
  [12:34:56] Building foo
  [12:34:57] Done
  ```
~~~

This feature file specifies the step clause increments
(cf. [D9](../dev/design_discussions.md)): the `time` and `dir`
patterns, then the `version` pattern and the `<regexp>` escape hatch, then
the `order`, `casing`, `whitespaces` and `blank_lines` settings patterns,
on the step clause only. The wider scopes (a `Given I ignore ...`
step, a command line option), the `path syntax` pattern and
the precedence between scopes are deferred, as is the homogenization of
the clause and option naming with the existing `ignore_casing` and co.
keywords — the settings themselves are now stateable in the scenario
through the clause (cf. `ignoring order, casing, whitespaces and
blank_lines` below), closing the in-scenario part wished when
[issue #18](https://github.com/LionelDraghi/bbt/issues/18) was closed.
The vocabulary is settled meanwhile: the step construct is a
**clause** — the `(ignoring ...)` clause —, the words it states are
**patterns** (the named and regexp masking patterns, the settings
patterns), and *option* stays reserved for the command line flags
(`--ignore_casing` and co). The
colored diff display of the ignored segments pairs with the
[diff display alignment](../dev/design_discussions.md).

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
`and`, in any mix: `(ignoring time, dir)`, `(ignoring dir, time and casing)`.
The masking patterns are the named `time`, `dir` and `version`, and any
other word of the clause is a custom `regexp` pattern (cf. `ignoring <regexp>`
below). Four further named patterns state the comparison settings instead
of masking: `order`, `casing`, `whitespaces` and `blank_lines` (cf.
`ignoring order, casing, whitespaces and blank_lines` below).

The clause applies to the comparison steps that compare a text with an
expected text: `I get`, `I get on stderr`, the `output is` / `output
contains` family, and the `file is equal to` / `file contains` family —
their negated forms included: the clause masks the volatile parts on
both sides before the (inverted) comparison. The `matches` / `does not
match` steps do not take it yet: when allowed, the clause will mask the
volatile parts of the actual line outside of what the regexp already
matches, the regexp itself being applied as is (cf. the TDL in
[project.md](../dev/project.md)).

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
sides, and neutralize only the syntax (cf. [D9](../dev/design_discussions.md)).

A run without any separator (`bbt`) is not a path occurrence and is not
masked.

## `ignoring version`

`ignoring version` masks version numbers: one, two or three dot separated
numeric components, optionally followed by a pre-release or build suffix
starting with `-` or `+`, as in `1.0`, `0.5.1`, `1.2.3-dev`,
`2.0.0-rc.1+meta`.

The rule is purely lexical, like `ignoring dir`: any isolated `D+.D+(.D+)?`
is treated as a version, so a decimal number such as `3.2` is masked too.
A duration in seconds is better handled by the `ignoring <regexp>` pattern.

## `ignoring location`

`ignoring location` masks source locations, as formatted by bbt and the
tools following the GNU Standard (cf. `BBT.IO.Image` and
[the GNU guidelines](https://www.gnu.org/prep/standards/html_node/Errors.html)):
a colon followed by a line number, and an optional column, either in
the GNU `:line:column` style or in the GNAT/bbt `line.column` style.
The locator must be closed by the colon that separates it from the
message, so that a bare `file:12` reference, a duration as in
`elapsed:3.2 s` or a port number as in `host:8080` are not masked.

Two guards keep the pattern narrow: the colon group is not preceded
by a digit, so that a clock time as in `12:34:56` is not cut in the
middle, and the match is closed by a colon, so that only a line/column
prefix is masked.

## `ignoring <regexp>`

The named patterns cover the common cases; any other volatile value is
stated with a regexp: **a word of the clause that is not a pattern name is
a custom pattern**, masking every substring that matches it, like the named
patterns do:

- ``- Then I get `Build finished in 3.2 s` (ignoring [0-9.]+) ``

The regexp follows the usual Perl-like syntax: character classes,
`.`, the `*` `+` `?` quantifiers, alternation... Being a single word of
the clause, it cannot contain blanks, commas or parenthesis, as they
separate or end the patterns.

Custom patterns combine with the named ones as the other patterns do:
`(ignoring time and [0-9]+)`.

A word that is not a legal regexp is a scenario error, reported at
parsing time. A misspelled pattern name is not an error though: it is a
legal regexp, matching only itself — most probably nothing on the actual
side, which the matches-nothing warning makes visible.

## `ignoring order`, `ignoring casing`, `ignoring whitespaces`, `ignoring blank_lines`

Four named patterns do not mask anything: they state, in the scenario,
how the comparison handles the line order and the text presentation.

- `ignoring order` compares the lines without regard to their order;
  it supersedes the historical `unordered` adjective, that remains
  legal but deprecated (cf. [feature `ignoring order`](A140_Unordered_Keyword.md));
- `ignoring casing`, `ignoring whitespaces` and `ignoring blank_lines`
  state that the comparison ignores the casing, the white spaces or
  the blank lines, as the human match default does: they bring the
  matching settings — previously reachable only through the
  `--exact_match`, `--ignore_casing`, ... command line options —
  explicitly into the scenario
  (cf. [feature exact match](A210_Exact_Match.md)).

Those patterns state the settings, the command line options set them:
**an option takes priority over a clause**. When the run checks what a
clause asks to ignore — that is, when `--exact_match` requested the
exact comparison — the clause is ignored and a warning raises, so that
the divergence is visible instead of silent. When they agree — the
human match default, or an `--ignore_casing` and co. option — the clause
changes nothing: it makes the step self-describing, the reader seeing
in the scenario itself that, say, the casing is not checked on that
step.

_Table of Contents:_
- [Clause syntax](#clause-syntax)
- [Semantics: masking on both sides](#semantics-masking-on-both-sides)
- [`ignoring time`](#ignoring-time)
- [`ignoring dir`](#ignoring-dir)
- [`ignoring version`](#ignoring-version)
- [`ignoring location`](#ignoring-location)
- [`ignoring <regexp>`](#ignoring-regexp)
- [`ignoring order`, `ignoring casing`, `ignoring whitespaces`, `ignoring blank_lines`](#ignoring-order-ignoring-casing-ignoring-whitespaces-ignoring-blank_lines)
- [Background](#background)
  - [Scenario: a time tag in the output does not fail the comparison](#scenario-a-time-tag-in-the-output-does-not-fail-the-comparison)
  - [Scenario: a time tag in the error output does not fail the comparison](#scenario-a-time-tag-in-the-error-output-does-not-fail-the-comparison)
  - [Scenario: the common clock, date and date-time formats are ignored](#scenario-the-common-clock-date-and-date-time-formats-are-ignored)
  - [Scenario: only the file name is checked, not the directory](#scenario-only-the-file-name-is-checked-not-the-directory)
  - [Scenario: a differing file name fails, differing directories do not](#scenario-a-differing-file-name-fails-differing-directories-do-not)
  - [Scenario: a differing stable part still fails](#scenario-a-differing-stable-part-still-fails)
  - [Scenario: time and dir combine on one step](#scenario-time-and-dir-combine-on-one-step)
  - [Scenario: a time tag does not fail a contains check](#scenario-a-time-tag-does-not-fail-a-contains-check)
  - [Scenario: version number does not fail the comparison](#scenario-version-number-does-not-fail-the-comparison)
  - [Scenario: a time tag missing on the actual side fails and warns](#scenario-a-time-tag-missing-on-the-actual-side-fails-and-warns)
  - [Scenario: the clause on a non comparison step is an error](#scenario-the-clause-on-a-non-comparison-step-is-an-error)
  - [Scenario: a duration may vary](#scenario-a-duration-may-vary)
  - [Scenario: a source location in the output does not fail the comparison](#scenario-a-source-location-in-the-output-does-not-fail-the-comparison)
  - [Scenario: a locator in the GNAT style is masked as well](#scenario-a-locator-in-the-gnat-style-is-masked-as-well)
  - [Scenario: a duration is not mistaken for a location](#scenario-a-duration-is-not-mistaken-for-a-location)
  - [Scenario: a regexp combines with a named pattern](#scenario-a-regexp-combines-with-a-named-pattern)
  - [Scenario: a regexp matching nothing on the actual side fails and warns](#scenario-a-regexp-matching-nothing-on-the-actual-side-fails-and-warns)
  - [Scenario: an illegal regexp in the clause is an error](#scenario-an-illegal-regexp-in-the-clause-is-an-error)

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
- But the output does not contain
  ```
  [12:34:56] Building foo
  [12:34:57] Done
  ```

The time tags differ between the actual output and the expected text, and
the same scenario passes whatever the run hour is.

### Scenario: a time tag in the error output does not fail the comparison

- When I run shell `echo 13:45:12 Building foo 1>&2`
- Then the error output is (ignoring time)
  ```
  12:34:56 Building foo
  ```
- But the error output is not (ignoring time)
  ```
  12:34:56 Building bar
  ```
- And the error output does not contain (ignoring time)
  ```
  12:34:56 Building bar
  ```

The negated comparison steps on the error output accept the clause like
their positive twin: the tags are masked on both sides before the
comparison. No quotes nor brackets in the echoed line: the command runs
through the platform shell, that does not strip them the same way
(cmd.exe on Windows).

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
- But file `run_a.txt` is not equal to file `run_b.txt`

### Scenario: only the file name is checked, not the directory

- Given the file `where.txt`
  ```
  sut home is /home/lionel/prj/bbt
  ```

- When I run `./sut read where.txt`
- Then I get `sut home is C:\prj\bbt` (ignoring dir)
- But the output does not contain `sut home is C:\prj\bbt`

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
- But the output does not contain
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
- But the output does not contain
  ```
  [12:34:56] Building foo
  ```

### Scenario: version number does not fail the comparison

- When I run `./bbt help`
- Then the output contains `bbt version 1.2.3` (ignoring version)

The expected `1.2.3` is a sample: the actual `bbt version 0.5.1-dev`
masks to the same marker, whatever the current version is.

- But the output does not contain `bbt version 1.2.3`

Without the clause, the literal comparison fails: there is no `1.2.3` in
the actual help output, hence the interest of masking instead of editing
the expected text at each version bump.

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

### Scenario: a duration may vary

- Given the file `perf.txt`
  ```
  Build finished in 12.76 s
  ```

- When I run `./sut read perf.txt`
- Then I get `Build finished in 3.2 s` (ignoring [0-9.]+)
- But the output does not contain `Build finished in 3.2 s`

The durations differ, but both mask to the same marker: the step still
checks that a number was printed, and the rest of the line stays checked.

### Scenario: a source location in the output does not fail the comparison

- Given the file `warn_ref.txt`
  ```
  t1.md:12: Warning: Multiple When in the same Scenario
  ```

- Given the file `warn_other.txt`
  ```
  t1.md:7: Warning: Multiple When in the same Scenario
  ```

- Then file `warn_ref.txt` is not equal to file `warn_other.txt`
- But file `warn_ref.txt` is equal to file `warn_other.txt` (ignoring location)

The line numbers differ, the file names are identical: the locations
are masked, the file names are not.

### Scenario: a locator in the GNAT style is masked as well

- Given the file `gnu_loc.txt`
  ```
  t1.md:12.34: Warning: Multiple When in the same Scenario
  ```

- Given the file `gnu_other.txt`
  ```
  t1.md:7.56: Warning: Multiple When in the same Scenario
  ```

- Then file `gnu_loc.txt` is equal to file `gnu_other.txt` (ignoring location)
- But file `gnu_loc.txt` is not equal to file `gnu_other.txt`

### Scenario: a duration is not mistaken for a location

- Given the file `perf_ref.txt`
  ```
  elapsed:3.2 s
  ```

- Given the file `perf_other.txt`
  ```
  elapsed:4.1 s
  ```

- Then file `perf_ref.txt` is not equal to file `perf_other.txt`
- And  file `perf_ref.txt` is not equal to file `perf_other.txt` even when (ignoring location)

The `3.2` is preceded by `:`, but the locator pattern requires a
trailing colon: the duration is not masked. The `time` pattern covers
durations; the `location` pattern covers source positions.

### Scenario: a regexp combines with a named pattern

- Given the file `count.txt`
  ```
  [13:45:12] 42 files processed
  ```

- When I run `./sut read count.txt`
- Then I get (ignoring time and [0-9]+)
  ```
  [12:34:56] 7 files processed
  ```
- But the output does not contain
  ```
  [12:34:56] 7 files processed
  ```

The time tag is masked by the `time` pattern, the file count by the
custom regexp: the clause mixes named and custom patterns freely.

### Scenario: a regexp matching nothing on the actual side fails and warns

- Given the file `build_words.txt`
  ```
  Build finished in twelve s
  ```

- Given the file `build_secs.txt`
  ```
  Build finished in 12.76 s
  ```

- Given the `regexp_compare.md` file
  ~~~
  # Scenario:
  - Then file `build_words.txt` is equal to file `build_secs.txt` (ignoring [0-9.]+)
  ~~~

- When I run `./bbt regexp_compare.md`
- Then I get an error
- And output contains `Error : build_words.txt not equal to expected:`
- And output contains `pattern matches nothing`

The expected text masks its `12.76` to a marker, the actual one holds
`twelve` and keeps no marker: the comparison fails, and the warning
draws the attention on the custom pattern that matched nothing — the
same warning as the named patterns raise.

### Scenario: an illegal regexp in the clause is an error

- Given the `bad_regexp.md` file
  ~~~
  # Scenario:
  - When I run `./sut`
  - Then I get `sut version 1.0` (ignoring [z-a])
  ~~~

- When I run `./bbt bad_regexp.md`
- Then I get an error
- And output contains `invalid regexp in the ignoring clause`

The clause is parsed with the step: an invalid character range is
rejected at parsing time, not at comparison time, and the scenario is
not run.
