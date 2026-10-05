<!-- omit from toc -->
## Feature : testing the existence of a file

bbt aims at facilitating the developer's life!  
So when the precondition (the `Given` step) says that there should be no `config.ini` file, bbt will not only check that a file does not exist, but he will also propose to erase it if there's one.  

When in a test suite, this test should be run with the --yes option, otherwise it will stop and prompt the user to confirm the erasing of the file.

If run in interactive mode, the expected behavior depends on the user answer : 
- If it answer "yes", the test will be OK.
- And logically, if it answer "no", the test will fail.

> [!NOTE]
> The two scenarios below run a nested bbt without the `--yes` option:
> it prompts for the erasing confirmation, and the answer is given by
> a ``When I type `` step (cf. [A290](A290_When_I_Type_Or_Enter.md)).

_Table of Contents:_
- [Scenario : a required file does not exist](#scenario--a-required-file-does-not-exist)
- [Scenario : the required file is created](#scenario--the-required-file-is-created)
- [Scenario : "Given there is no", when there actually is, should erase the file](#scenario--given-there-is-no-when-there-actually-is-should-erase-the-file)
- [Scenario : erasing confirmed by a typed key](#scenario--erasing-confirmed-by-a-typed-key)
- [Scenario : erasing refused by a typed key](#scenario--erasing-refused-by-a-typed-key)

### Scenario : a required file does not exist 

- Given there is no file `config.ini`
- When I run `./sut read config.ini`
- Then I get error

### Scenario : the required file is created

  - Given my favorite and so useful `config.ini` file
```
Tmp_dir=/tmp
Alias l="ls -tla"
```
- Then `config.ini` contains `Tmp_dir=/tmp`

 ### Scenario : "Given there is no", when there actually is, should erase the file 


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
