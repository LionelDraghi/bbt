<!-- omit from toc -->
# Ignoring volatile parts in comparisons

## Problem

Comparing an expected output with an actual one frequently fails on parts
of the text that are *volatile*: they change from run to run, or from
machine to machine, although what the scenario really wants to check is
the stable part. Recurring cases:

- **time tags**: the tested program prints the hour, the date or a
  duration (`[12:34:56] Running foo`, `Build finished in 3.2 s`), and the
  value changes at each run;
- **absolute paths**: the tested program prints the full path of a file,
  which is `/home/lionel/...` on the development machine and
  `/home/runner/work/...` in the CI; no fixed expected text can match
  both;
- **path syntax**: the same directory is `C:\prj\bbt\tests` on Windows and
  `/home/lionel/prj/bbt/tests` on Unix; one expected output per platform
  would be needed to test the same behavior;
- and more generally any environment dependent value: version numbers,
  process ids, user names, locale dependent formats...

The workarounds are all heavier than the need:

- switch the step from `is` to `matches` and rewrite the whole line as a
  regexp embedding the volatile part: it works, but the regexp must
  cover the whole line (cf. [A200](../features/A200_Regexp.md)), so the
  stable part of the line loses its literal, precise check, and the
  scenario becomes barely readable;
- filter the actual output before comparing, which is what the smk and
  acc scenarios do today to neutralize their time tags, at the cost of
  a shell and two seds in the step:

  ```
  - And I run `sh -c "smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
  ```

  The `sh -c` is there only to host the pipe, the seds are there only
  to rewrite what bbt should not look at, the expected text then has to
  be written with the `YYYY:MM:DD` and `HH:MM:SS.SS` stand-ins instead
  of what the program really prints, and the exit status checked is
  sed's, not the tested program's;
- drop the volatile lines from the expected text, which silently weakens
  the test.

The matching settings (`ignore_casing`, `ignore_whitespaces`,
`ignore_blank_lines`, cf. [A210](../features/A210_Exact_Match.md)) already
say that some differences are legitimately neutral. Time tags and paths
are exactly that kind of difference, but today there is no way to tell bbt.

## Proposed solution

An explicit **ignoring clause** on the comparison steps: the scenario
states which parts of the comparison are volatile, instead of the author
working around them. The smk step above becomes:

~~~
- When I run `./smk -e`
- Then the output is (ignoring time)
  ```
  [12:34:56.78] some output line
  [12:34:56.78] some other output line
  ```
~~~

No more `sh -c`, no more sed: the command run is the command tested (its
exit status is the checked one), the expected text is what the program
really printed (the author copies an actual output, not the
`YYYY:MM:DD` stand-ins), and the volatile parts are neutralized where
the comparison happens, not by rewriting the output beforehand.

~~~
- Then the output is (ignoring time)
  ```
  [12:34:56] Building foo
  [12:34:57] Done
  ```
~~~

- `ignoring time` neutralizes the common time and date formats
  (`12:34:56`, `12:34`, `2026-10-13`, `2026-10-13T12:34:56`...);
- `ignoring dir` neutralizes the directory part of a path: on
  `/home/lionel/prj/bbt`, `/home/lionel/prj/` is masked and `bbt` is
  still checked. This is the machine dependent location case: the
  program prints a place that depends on where it was installed, but
  the relevant information is the file or directory name. On Windows,
  `C:\prj\bbt` masks to the same checked `bbt`, the drive letter being
  part of the masked prefix. Note that location and syntax are two
  subjects, and that `ignoring dir` neutralizes both at once — a Unix
  and a Windows print of the same place compare equal; requiring the
  same path while neutralizing only the syntax is the
  `ignoring path syntax` job, below;
- `ignoring version` masks semver-like version numbers — one to three
  dot separated numeric components, optionally followed by a pre-release
  or build suffix (`1.0`, `0.5.1`, `1.2.3-dev`, `2.0.0-rc.1+meta`) —
  the common case of a version tag printed by the tested tool and
  bumped between two runs;
- `ignoring path syntax` does not mask anything: it only normalizes the
  way paths are written — `\` to `/`, drive letter prefixes — so that
  the same location, printed as `tests\results.md` on Windows and
  `tests/results.md` on Unix, compares equal. The name `ignoring path`
  is avoided on purpose: it would read as "ignore the path", which is
  the `ignoring dir` job; here the location is still fully checked,
  only its writing is normalized;
- `ignoring <regexp>` is the general escape hatch for any other volatile
  value:

```
- Then I get `Build finished in 3.2 seconds` (ignoring `[0-9.]+`)
```

Several patterns may be combined on one step, separated by commas,
spaces or `and` (`ignoring time, dir`, `ignoring dir, time and casing`).

### Semantics

Masking, on both sides: before the comparison proper, every substring
matching one of the ignore patterns — in the expected text *and* in the
actual output — is replaced by a fixed marker. The comparison
(`is`, `contains`, `file is equal to`...) then proceeds on the masked
texts with the current match settings, as today.

The expected text keeps a sample of the volatile value: the author writes
"what I saw", and flags the pattern that will vary. This differs from
`matches`, where the expected text is a pattern to be verified; here the
step still checks that something *was* printed where the volatile value
stands (a line reduced to nothing but the marker would fail), only its
exact value is not checked.

A pattern that never matches on the actual side should raise a warning:
most probably the scenario declares volatile something that the program
no longer prints, and the neutralization would silently weaken the test.

### Scope

Repeating the clause on every comparison step of a document would be as
tedious as today's workarounds. The clause should therefore also be
settable at a wider scope:

- a `Given` step (`- Given I ignore time and dir in the following steps`),
  Background-able for a whole document;
- a command line option (`--ignoring dir`) for a whole run, for
  instance to neutralize the location prefixes in a CI environment
  only.

Precedence to be arbitrated (see open questions).

The step clause increments, specified and implemented in
[A300_Ignoring.md](../features/A300_Ignoring.md), cover the step clause,
with the `time`, `dir`, `version` and `<regexp>` patterns. The `Given` step
and the
command line option — and with them the precedence question — are
deferred to a later increment, as is the homogenization of the clause and
option naming with the existing `ignore_casing`, `ignore_whitespaces` and
`ignore_blank_lines` settings (cf. the [ignoring design
discussion](../dev/design_discussions.md)). When that homogenization is
resumed, the goal will be to bring those settings explicitly into the
scenario instead of leaving them on the command line, as wished when
[issue #18](https://github.com/LionelDraghi/bbt/issues/18) was closed.

### Is such a Given in the Gherkin spirit?

The wider scope `Given` step could be objected to: in BDD doctrine,
a `Given` brings the *system under test* to a known initial state,
whereas an ignoring pattern tunes the *checker*, and would belong to
a command line option or a tag. The answer is that bbt is not BDD
Gherkin (cf. [B050](../features/B050_Strict_gherkin.md): the strict
rules target communication, and bbt is deliberately more flexible in a
test context), and that its `Given` steps already arrange the test
bench rather than the software under test (`Given the new file`,
`Given exec_dir`, `Given the environment variable`): the harness is
part of the scenario world. A doctrinal purist could also prefer a
state form (`Given time and dir are ignored`) over the habitual
present, cosmetic in bbt where steps use the simple present
throughout. The real, practical objection is scope leakage: a
`Given` in a Background silently weakens every comparison of the
document, invisible to the reader of a single scenario — mitigated by
keeping the step clause visible for local cases, and by the
warning when a pattern matches nothing on the actual side.

## Diff display

When the comparison fails, the sdiff display (cf.
[B105](../features/B105_Expected_vs_Actual_Output.md)) must show what was
neutralized.

The current first increment applies the mask to the display: the
neutralized segments appear as the constant marker `***`, in the tty and
non-tty outputs alike, so that the shown diff is exactly the tested diff.

The target display, once the
[LCS alignment proposal](LCS_diff_alignment.md) lands, is:

- on a color terminal, the original, **unmasked** text: the ignored
  segments are displayed as they were printed, in a distinct color (for
  instance yellow), so that the eye separates "the scenario ignores
  this" from "this is the failure" by color only, and the real output of
  the command stays visible;
- additionally, on differing lines, the differing characters themselves
  should be highlighted in red instead of the whole line, so that a one
  character difference in a long line draws the eye. This pairs naturally
  with the LCS alignment: the alignment identifies the differing line
  pairs, an intra-line comparison identifies the differing characters
  inside a pair.

Colors are emitted only when the standard output is a terminal, with the
same rules as the status bar (`--no_tty` wins). For the non-tty outputs
(Markdown logs, JUnit), where no color is available, three textual
conventions were discussed:

- format stand-ins, in the smk convention: display `HH:MM:SS` for a
  masked clock value — the data is not visible any more, but its format
  is explicit;
- a repeated character, as long as the neutralized field, so that the
  text alignment and the size of the volatile value are preserved;
- a constant marker, independent of the masked value.

The constant marker `***` is kept for now, in the tty and non-tty
outputs: it is simple, and the shown diff is exactly the tested diff;
the choice will be revisited with the colored display.

## Impacted code

- grammar: new optional clause on the comparison steps
  (`bbt-scenarios-steps-initialize_grammar`);
- settings: the active ignore patterns
  (`bbt-settings`, the step and command line scopes);
- comparison: a masking function applied before the comparison
  (`text_utilities`, `bbt-tests-actions-output_checks`), the patterns
  being also passed to `Side_By_Side` so that the display can color the
  ignored segments;
- writers: the colored diff segments, tty gated as the status bar is.

## Open questions

1. **Keyword**: `(ignoring time)` vs `(ignore time)`. **Arbitrated
   (October 2026)**: the step clause uses the participle `ignoring` — it
   reads as a modifier of the comparison ("the output is X, ignoring
   time"), is consistent with the existing "Ignoring Blank Lines"
   naming (A160), and keeps the step a natural English sentence; the
   imperative in parentheses would make sense only as a bare
   instruction to the tool, outside the sentence. The imperative stays
   the natural form in the wider scope `Given` step
   (`Given I ignore time and dir in the following steps`), where the
   subject "I" makes it a plain declarative sentence. The
   homogenization of this `ignoring` clause and `ignore` imperative with
   the existing `ignore_casing` and co. keywords and options is
   deferred, cf. the [ignoring design
   discussion](../dev/design_discussions.md);
2. **Scope precedence**: step clause, `Given` step, command line option —
   which wins, and does a narrower clause replace the inherited patterns
   or add to them? **Deferred**: to be arbitrated when the `Given` step
   and the command line option are implemented;
3. **Predefined patterns**: which time and date formats exactly, and is
   there a need for a separate `ignoring date` or `ignoring duration`;
   **settled for the first increment**: the time formats are listed in
   [A300_Ignoring.md](../features/A300_Ignoring.md), `ignoring version`
   is added for semver-like numbers, and a separate `ignoring date` or
   `ignoring duration` is deferred — no other predefined pattern was
   obvious enough, the `ignoring <regexp>` escape hatch covering the
   remaining cases;
4. **Path clauses**: what counts as a path for `ignoring dir` (leading
   `/`, drive letter, UNC `\\` prefix); and for the format
   normalization, is `ignoring path syntax` the right term
   (`ignoring separators`? a non-clause setting such as
   `--normalize_paths`, since it normalizes rather than ignores?), and
   is it combined with `ignoring dir` automatically, the masked prefix
   making the separator normalization moot on the masked part;
   **settled for the first increment**: the lexical path rule of
   `ignoring dir` (including its URL consequence) is specified in
   [A300_Ignoring.md](../features/A300_Ignoring.md), `ignoring path
   syntax` is deferred;
5. **Interaction with the matching modes**: masking happens before the
   `--exact_match` / `ignore_casing` / ... handling (proposal: before,
   the two mechanisms are orthogonal); **settled**: masking first,
   orthogonal, as specified in
   [A300_Ignoring.md](../features/A300_Ignoring.md);
6. **`matches`**: should it accept the clause too, or is it redundant
   there (the whole line is already a pattern)? **settled for the first
   increment**: the `matches` steps do not take the clause;
7. **Warning on a pattern matching nothing** on the actual side:
   **settled**: yes (see semantics), as specified in
   [A300_Ignoring.md](../features/A300_Ignoring.md).

## Scenarios

Illustrative: those scenarios describe the target behavior. The step
clause part (`ignoring time`, `ignoring dir`, `ignoring <regexp>`) is
specified and tested in
[A300_Ignoring.md](../features/A300_Ignoring.md); the scenarios below
also cover the parts not yet implemented (`ignoring path syntax`,
the wider scopes).

### Scenario: a time tag in the output does not fail the comparison

- When I run `./sut build`
- Then the output is (ignoring time)
  ```
  [12:34:56] Building foo
  [12:34:57] Done
  ```

The same scenario passes whatever the run hour is.

### Scenario: the same scenario runs on Windows and Unix

- When I run `./sut list`
- Then I get `tests/results.md and tests/results.xml`
  (ignoring path syntax)

The same scenario passes when the actual output is
`tests\results.md and tests\results.xml`: the locations are still
checked, only their writing is normalized.

### Scenario: only the file name is checked, not the directory

- When I run `./sut where`
- Then I get `sut home is /home/lionel/prj/bbt` (ignoring dir)

The same scenario passes when the actual output is
`sut home is C:\prj\bbt`: `/home/lionel/prj/` is ignored, `bbt` is
checked. A program printing `sut home is /opt/bbt-daily` would fail:
unlike `ignoring path syntax`, the directory part is masked, but the
name stays checked.

### Scenario: a duration may vary

- When I run `./sut build`
- Then I get `Build finished in 3.2 seconds` (ignoring `[0-9.]+`)

### Scenario: the diff display shows what is ignored

- Given the new file `bad.md`
  ~~~
  # Scenario:
  - When I run `./sut build`
  - Then the output is (ignoring time)
    ```
    [12:34:56] Building foo
    [12:34:57] Done
    ```
  ~~~
- When I run `./bbt bad.md`
- Then I get an error
- and the diff shows the time tag segments in the ignore color,
  and the really differing characters in red
