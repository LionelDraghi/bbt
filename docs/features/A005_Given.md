<!-- omit from toc -->
## Feature

Given steps are the first leg of the three-legged Steps stool.
It is meant to check and setup the execution environment, by:
- checking some dir or file exists or doesn't exists, or by
- running something that should be run first.

When the file to create already exists, ``Given the file `X` `` does not
blindly keep it nor blindly erase it: the existing content is compared
to the expected one in the current match mode (`--human_match` or
`--exact_match`). On a match, the step succeeds without any
confirmation, and the file is kept as is. On a difference, bbt proposes
to replace the file, and does it on a `Y` answer (or directly with the
`--yes` option); on a `n` answer, the file is kept as is and the step
fails.

_Table of Contents:_
- [Scenario: Checking that there is no file or dir](#scenario-checking-that-there-is-no-file-or-dir)
- [Scenario: Checking that there is some dir](#scenario-checking-that-there-is-some-dir)
- [Scenario: An existing directory is kept as is](#scenario-an-existing-directory-is-kept-as-is)
- [Scenario: Checking that there is a file with some content](#scenario-checking-that-there-is-a-file-with-some-content)
- [Scenario: Creating a file with some content](#scenario-creating-a-file-with-some-content)
- [Scenario: An existing file with the expected content is kept as is](#scenario-an-existing-file-with-the-expected-content-is-kept-as-is)
- [Scenario: An existing file with a different content is replaced with --yes](#scenario-an-existing-file-with-a-different-content-is-replaced-with---yes)
- [Scenario: Replacing confirmed by a typed key](#scenario-replacing-confirmed-by-a-typed-key)
- [Scenario: Replacing refused by a typed key](#scenario-replacing-refused-by-a-typed-key)
- [Scenario: The current match mode drives the comparison](#scenario-the-current-match-mode-drives-the-comparison)

### Scenario: Checking that there is no file or dir
- Given there is no dir `dir1`
- Given there is no file `file1`

- Then there is no dir `dir1`
- Then there is no file `file1`

### Scenario: Checking that there is some dir
- Given the directory `dir2`
- Then there is a dir `dir2`

### Scenario: An existing directory is kept as is

``Given the directory `` only creates the directory if it does not
exist: an existing directory, and its content, are kept as is.

- Given the new directory `dir6`
- Given the new file `dir6/f1` containing `some data`
- Given the directory `dir6`
- Then there is a `dir6` directory
- And there is a `dir6/f1` file

### Scenario: Checking that there is a file with some content
- Given there is no file `file3`
- Given the file `file3`
~~~
Hello world!
~~~

- Then file `file3` is `Hello World!`

### Scenario: Creating a file with some content
- Given the file `file4` containing `alpha`
- Given the file `file5` containing 
~~~
beta
zeta
~~~

- Then file `file4` is `alpha`
- Then file `file5` is
~~~
beta
zeta
~~~

### Scenario: An existing file with the expected content is kept as is

The nested bbt is run without the `--yes` option: a
``Given the file `` step on an existing file whose content matches the
expected one does not prompt for any confirmation, and the file is
kept as is.

- Given the new file `config.ini` containing `Tmp_dir=/tmp`
- Given the new `keep_content.md` file
~~~md
# Scenario: nested

- Given the file `config.ini` containing `Tmp_dir=/tmp`
- Then the file `config.ini` is `Tmp_dir=/tmp`
~~~
- When I run `./bbt -q -c keep_content.md`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- And there is a `config.ini` file

### Scenario: An existing file with a different content is replaced with --yes

Same as the previous scenario, but the existing file does not have
the expected content, and the nested bbt is run with `--yes`: the
file is replaced without confirmation.

- Given the new file `config.ini` containing `Tmp_dir=/other`
- Given the new `replace_yes.md` file
~~~md
# Scenario: nested

- Given the file `config.ini` containing `Tmp_dir=/tmp`
- Then the file `config.ini` is `Tmp_dir=/tmp`
~~~
- When I run `./bbt -q --yes replace_yes.md`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- And there is a `config.ini` file

### Scenario: Replacing confirmed by a typed key

The nested bbt is run without the `--yes` option and without
`--cleanup`, so that the replaced file remains available to the final
checks: it prompts for the replacing confirmation, the scenario
answers `Y`, and the file is replaced.

The command runs on a pseudo terminal (cf. the interaction feature),
and the nested bbt would then detect a terminal and print its status
bar in the middle of the prompt: `--no_tty` keeps its output clean.

- Given the new file `config.ini` containing `Tmp_dir=/other`
- Given the new `replace_confirm.md` file
~~~md
# Scenario: nested

- Given the file `config.ini` containing `Tmp_dir=/tmp`
- Then the file `config.ini` is `Tmp_dir=/tmp`
~~~
- When I run `./bbt -q --no_tty replace_confirm.md`
- Then the output is
```
Overwrite file config.ini?   [Y]es/[N]o/[A]ll
```
- When I type `Y`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- And there is a `config.ini` file
- Then I get no error

### Scenario: Replacing refused by a typed key

Same run, but the scenario answers `n`: the file is kept as is, the
nested scenario fails. The nested bbt runs with `--no_tty`, as in the
previous scenario: the command is on a pseudo terminal.

- Given the new file `config.ini` containing `Tmp_dir=/other`
- Given the new `replace_refuse.md` file
~~~md
# Scenario: nested

- Given the file `config.ini` containing `Tmp_dir=/tmp`
- Then the file `config.ini` is `Tmp_dir=/tmp`
~~~
- When I run `./bbt -q --no_tty replace_refuse.md`
- Then the output is
```
Overwrite file config.ini?   [Y]es/[N]o/[A]ll
```
- When I type `n`
- Then I get an error
- And the output contains `file "config.ini" not overwritten`
- And there is a `config.ini` file

### Scenario: The current match mode drives the comparison

The nested bbt is run twice on an existing file differing from the
expected content only by casing and blanks. In human match mode (the
default), the difference is not significant: no replacing is proposed
and the run succeeds. In exact match mode, the difference is
significant: bbt prompts for the replacing confirmation, and the
scenario answers `n`.

- Given the new file `config.ini` containing `Tmp_dir=/tmp`
- Given the new `match_mode.md` file
~~~md
# Scenario: nested

- Given the file `config.ini` containing `tmp_dir = /tmp`
- Then the file `config.ini` is `tmp_dir = /tmp`
~~~
- When I run `./bbt -q match_mode.md`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- When I run `./bbt -q --no_tty --exact_match match_mode.md`
- Then the output is
```
Overwrite file config.ini?   [Y]es/[N]o/[A]ll
```
- When I type `n`
- Then I get an error
- And the output contains `file "config.ini" not overwritten`

