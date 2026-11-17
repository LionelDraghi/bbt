<!-- omit from toc -->
## Feature: tmp files and directories deletion

Scenarios may create a lot of file and directory during run, and that's tedious to remove.  
The --cleanup option is here to ensure that those files are removed after run.

bbt never removes a pre-existing file or directory that no Given step touches. A pre-existing file that a Given step overwrites is different: with the expected content, it is just a leftover from a previous run, and overwriting it needs no consent; with a different content, the overwrite needs an explicit one. In both cases the overwritten file joins the deletion list, and is removed like any other run artifact. The only way to keep a pre-existing file is to refuse the overwrite — answering no on the prompt, or implicitly on a non interactive stdin.

> [!CAUTION] 
> With `--cleanup`, bbt will remove:
> 1. files that he did create or overwrote, including pre-existing ones
>    (see above);
> 2. directories that he did create, the whole tree, including files and subdirectories that he didn't create!
>
> With `--cleanup=full`, bbt will additionally remove every file and directory
> that appeared in the execution directory during the run, **whatever created
> them**. bbt does not track who created those new entries, only that they were
> not there when the run started: a file written in parallel by another process
> running at the same time — a build, an editor, a synchronization tool — will
> be removed like any other new file. Don't run `--cleanup=full` in a directory
> where something else may write concurrently, or run it with no `--yes` and
> check the confirmation.

The tested commands may also create files and directories of their own, that `--cleanup` ignores.  
With `--cleanup=full`, bbt takes a recursive snapshot of the execution directory at the start of each document, and removes at the end everything that appeared since, whatever created it. As always, nothing is removed when the run failed, to ease debugging.  
The removal of those entries, that bbt did not create itself, asks for one confirmation per document, skipped with `--yes`; on a non interactive stdin, the removal is refused and the run fails.  
Symbolic links are ignored by the snapshot and the removal: a link is never traversed, and a new link may be left in place.

_Table of Contents_:
- [Background:](#background)
- [Scenario: run without --cleanup](#scenario-run-without---cleanup)
- [Scenario: run with --cleanup](#scenario-run-with---cleanup)
- [Scenario: run with --cleanup leaves what the tested command created](#scenario-run-with---cleanup-leaves-what-the-tested-command-created)
- [Scenario: run with --cleanup=full removes what the tested command created](#scenario-run-with---cleanupfull-removes-what-the-tested-command-created)
- [Scenario: --cleanup=full keeps pre-existing files](#scenario---cleanupfull-keeps-pre-existing-files)
- [Scenario: --cleanup=full removal refused on a non interactive stdin](#scenario---cleanupfull-removal-refused-on-a-non-interactive-stdin)
- [Scenario: --cleanup=full removes a pre-existing file with the expected content](#scenario---cleanupfull-removes-a-pre-existing-file-with-the-expected-content)
- [Scenario: --cleanup=full removes a file overwritten with --yes](#scenario---cleanupfull-removes-a-file-overwritten-with---yes)
- [Scenario: a refused overwrite keeps the pre-existing file](#a-refused-overwrite-keeps-the-pre-existing-file)
- [Scenario: a non interactive run refuses the overwrite and keeps the file](#a-non-interactive-run-refuses-the-overwrite-and-keeps-the-file)

### Background:

- Given there is no dir `dir1`
- Given there is no dir `dir3`
- Given there is no file `f1`
- Given there is no file `sut_out.txt`
- Given there is no dir `sut_dir`
- Given the new `create_tree.md` file
  ```md
  # Scenario: create tree
    - Given the new dir `dir1`
    - And   the new dir `dir1/dir2`
    - And   the new dir `dir3/dir4/dir5`
    - And   the new file `f1` containing `text 1`
    - And   the new file `dir1/dir2/f2` containing `text 2`

    - Then there is a `dir1/dir2/f2` file
    - And  there is a `f1` file
    - And  there is a `dir3/dir4/dir5` dir
  ```
- Given the new `create_sut_files.md` file
  ```md
  # Scenario: create files with the tested command
    - When I run shell `mkdir sut_dir`
    - And I run `./sut create sut_out.txt`
    - And I run `./sut create sut_dir/f_in_dir.txt`

    - Then there is a `sut_out.txt` file
    - And there is a `sut_dir/f_in_dir.txt` file
  ```

### Scenario: run without --cleanup

- When I run `./bbt --yes create_tree.md`

- Then there is a `dir1/dir2/f2` file
- Then there is a `dir3/dir4/dir5` dir
- And  there is a `f1` file

### Scenario: run with --cleanup

- When I run `./bbt --cleanup create_tree.md`

- Then there is no `dir1` directory
- And  there is no `dir3` directory
- And  there is no `f1` file

### Scenario: run with --cleanup leaves what the tested command created

- When I run `./bbt --cleanup create_sut_files.md`

- Then there is a `sut_out.txt` file
- And  there is a `sut_dir` directory

### Scenario: run with --cleanup=full removes what the tested command created

- When I run `./bbt --yes --cleanup=full create_sut_files.md`

- Then the output contains `## Summary : **Success**`
- And  there is no `sut_out.txt` file
- And  there is no `sut_dir` directory

### Scenario: --cleanup=full keeps pre-existing files

- Given the new file `precious.txt` containing `keep me`
- When I run `./bbt --yes --cleanup=full create_sut_files.md`

- Then there is a `precious.txt` file

### Scenario: --cleanup=full removal refused on a non interactive stdin

The nested bbt is run without the `--yes` option, and is not
interactive: nobody can answer the cleanup confirmation. The removal
of the new entries is refused, and the run fails with an error telling
how to get them removed.

- When I run `./bbt --no_tty --cleanup=full create_sut_files.md`

- Then I get an error
- And  the output contains `use --yes to confirm all operations`
- And  there is a `sut_out.txt` file

### Scenario: --cleanup=full removes a pre-existing file with the expected content

A file left over by a previous run, holding the very content the
scenario is about to write, is not a user file: bbt silently overwrites
it, puts it on the deletion list, and `--cleanup=full` removes it at
the end of the run like any other run artifact. This is what clears
the residues left over by direct runs without `--cleanup`.

- Given the new file `leftover.txt` containing `some text`
- Given the new `overwrite_same.md` file
  ~~~md
  # Scenario: overwrite with the same content

  - Given the file `leftover.txt` containing `some text`
  - Then there is a `leftover.txt` file
  ~~~
- When I run `./bbt -q --yes --cleanup=full overwrite_same.md`

- Then the output contains `## Summary : **Success**`
- And there is no `leftover.txt` file

### Scenario: --cleanup=full removes a file overwritten with --yes

A pre-existing file with a different content is only overwritten with an
explicit consent. With `--yes`, the consent is given, the previous
content is lost anyway, so the file joins the deletion list and is
removed at the end of the run.

- Given the new file `replaced.txt` containing `keep me`
- Given the new `overwrite_yes.md` file
  ~~~md
  # Scenario: overwrite with --yes

  - Given the file `replaced.txt` containing `new content`
  - Then there is a `replaced.txt` file
  ~~~
- When I run `./bbt -q --yes --cleanup=full overwrite_yes.md`

- Then the output contains `## Summary : **Success**`
- And there is no `replaced.txt` file

### Scenario: a refused overwrite keeps the pre-existing file

The only case where a pre-existing file survives: the user answers no
to the overwrite prompt. The step fails, the file is not on the
deletion list, and it keeps its original content.

- Given the new file `refused.txt` containing `keep me`
- Given the new `overwrite_refused.md` file
  ~~~md
  # Scenario: overwrite refused

  - Given the file `refused.txt` containing `new content`
  - Then there is a `refused.txt` file
  ~~~
- When I run `./bbt -q --no_tty overwrite_refused.md`
- Then the output is
```
Overwrite file refused.txt?   [Y]es/[N]o/[A]ll
```
- When I type `n`
- Then I get an error
- And the output contains `file "refused.txt" not overwritten`
- And there is a `refused.txt` file
- And the file `refused.txt` contains `keep me`

### Scenario: a non interactive run refuses the overwrite and keeps the file

Same rule, implicit refusal: without `--yes` and without anybody able
to answer the prompt, the overwrite is refused, the step fails, and the
file keeps its original content.

- Given the new file `refused2.txt` containing `keep me`
- Given the new `overwrite_no_tty.md` file
  ~~~md
  # Scenario: overwrite refused on a pipe

  - Given the file `refused2.txt` containing `new content`
  - Then there is a `refused2.txt` file
  ~~~
- When I run `./bbt -q --no_tty overwrite_no_tty.md`

- Then I get an error
- And the output contains `use --yes to confirm all operations`
- And the output contains `not overwritten`
- And there is a `refused2.txt` file
- And the file `refused2.txt` contains `keep me`
