<!-- omit from toc -->
## Feature : testing the existence of a file

bbt aims at facilitating the developer's life!  
So when the precondition (the `Given` step) says that there should be no `config.ini` file, bbt will not only check that a file does not exist, but he will also propose to erase it if there's one.  

The same behavior applies to directories with the *there is no* form.
And the `new` keyword has the same intent on an existing homonym:
``Given the new directory `dir1` `` starts from a white page, erasing
the existing `dir1` tree before recreating the directory empty.

When in a test suite, this test should be run with the --yes option, otherwise it will stop and prompt the user to confirm the erasing of the file.

If run in interactive mode, the expected behavior depends on the user answer : 
- If it answer "yes", the test will be OK.
- And logically, if it answer "no", the test will fail.


_Table of Contents:_
- [Scenario : a required file does not exist](#scenario--a-required-file-does-not-exist)
- [Scenario : "Given there is no", when there actually is, should erase the file](#scenario--given-there-is-no-when-there-actually-is-should-erase-the-file)
- [Scenario : erasing confirmed by a typed key](#scenario--erasing-confirmed-by-a-typed-key)
- [Scenario : erasing refused by a typed key](#scenario--erasing-refused-by-a-typed-key)
- [Scenario : directory tree erasing confirmed by a typed key](#scenario--directory-tree-erasing-confirmed-by-a-typed-key)
- [Scenario : directory tree erasing refused by a typed key](#scenario--directory-tree-erasing-refused-by-a-typed-key)
- [Scenario : new directory erasing confirmed by a typed key](#scenario--new-directory-erasing-confirmed-by-a-typed-key)
- [Scenario : new directory erasing refused by a typed key](#scenario--new-directory-erasing-refused-by-a-typed-key)

### Scenario : a required file does not exist 

- Given there is no file `config.ini`
- When I run `./sut read config.ini`
- Then I get error

### Scenario : "Given there is no", when there actually is, should erase the file 

- Given my favorite and so useful `config.ini` file
```
Tmp_dir=/tmp
Alias l="ls -tla"
```
- Given there is no `config.ini` file  
- Then there is no more `config.ini` file
 
### Scenario : erasing confirmed by a typed key

The nested bbt is run without the `--yes` option: it prompts for the
erasing confirmation, the scenario answers `Y`, and the file is erased.

- Given the new file `config.ini` containing `Tmp_dir=/tmp`
- Given the new `erase_confirm.md` file
~~~md
# Scenario: nested

- Given there is no `config.ini` file
- Then there is no `config.ini` file
~~~
- When I run `./bbt -q -c erase_confirm.md`
- Then the output is
```
Delete file config.ini?   [Y]es/[N]o/[A]ll
```
- When I type `Y`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- And there is no `config.ini` file

### Scenario : erasing refused by a typed key

Same run, but the scenario answers `n`: the file is not erased, the
nested scenario fails.

- Given the new file `config.ini` containing `Tmp_dir=/tmp`
- Given the new `erase_confirm.md` file
~~~md
# Scenario: nested

- Given there is no `config.ini` file
- Then there is no `config.ini` file
~~~
- When I run `./bbt -q -c erase_confirm.md`
- Then the output is
```
Delete file config.ini?   [Y]es/[N]o/[A]ll
```
- When I type `n`
- Then I get an error
- And the output contains `file "config.ini" not deleted`
- And there is a `config.ini` file

### Scenario : directory tree erasing confirmed by a typed key

The nested bbt is run without the `--yes` option: it prompts for the
erasing confirmation of a directory, the scenario answers `Y`, and the
whole tree is erased.

- Given the new `d1` directory
- Given the new `d1/f1` file containing `some data`
- Given the new `erase_dir_confirm.md` file
~~~md
# Scenario: nested

- Given there is no `d1` directory
- Then there is no `d1` directory
~~~
- When I run `./bbt -q -c erase_dir_confirm.md`
- Then the output is
```
Delete tree d1?   [Y]es/[N]o/[A]ll
```
- When I type `Y`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- And there is no `d1` directory

### Scenario : directory tree erasing refused by a typed key

Same run, but the scenario answers `n`: the directory is not erased,
the nested scenario fails.

- Given the new `d1` directory
- Given the new `d1/f1` file containing `some data`
- Given the new `erase_dir_refuse.md` file
~~~md
# Scenario: nested

- Given there is no `d1` directory
- Then there is no `d1` directory
~~~
- When I run `./bbt -q -c erase_dir_refuse.md`
- Then the output is
```
Delete tree d1?   [Y]es/[N]o/[A]ll
```
- When I type `n`
- Then I get an error
- And the output contains `dir "d1" not deleted`
- And there is a `d1` directory

### Scenario : new directory erasing confirmed by a typed key

The nested bbt is run without the `--yes` option: a
``Given the new directory `` step on an existing tree prompts for
the erasing confirmation, the scenario answers `Y`, and the
directory is recreated empty. The nested bbt is also run without
`--cleanup`, so that the recreated directory remains available to
the final checks.

- Given the new `d2` directory
- Given the new `d2/f1` file containing `some data`
- Given the new `erase_new_dir_confirm.md` file
~~~md
# Scenario: nested

- Given the new `d2` directory
- Then there is a dir `d2`
- And there is no file `d2/f1`
~~~
- When I run `./bbt -q erase_new_dir_confirm.md`
- Then the output is
```
Delete tree d2?   [Y]es/[N]o/[A]ll
```
- When I type `Y`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`
- And there is a `d2` directory
- And there is no `d2/f1` file

### Scenario : new directory erasing refused by a typed key

Same run, but the scenario answers `n`: the existing tree is kept as
is, and the nested scenario fails.

- Given the new `d3` directory
- Given the new `d3/f1` file containing `some data`
- Given the new `erase_new_dir_refuse.md` file
~~~md
# Scenario: nested

- Given the new `d3` directory
~~~
- When I run `./bbt -q erase_new_dir_refuse.md`
- Then the output is
```
Delete tree d3?   [Y]es/[N]o/[A]ll
```
- When I type `n`
- Then I get an error
- And the output contains `dir "d3" not deleted`
- And there is a `d3` directory
- And there is a `d3/f1` file
