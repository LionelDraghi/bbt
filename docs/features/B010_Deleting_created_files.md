<!-- omit from toc -->
## Feature: tmp files and directories deletion

Scenarios may create a lot of file and directory during run, and that's tedious to remove.  
The --cleanup option is here to ensure that those files are removed after run.

Obviously, bbt never removes (unless a bug!) files and dir present when you started bbt.

> [!CAUTION] 
> bbt will remove:
> 1. files that he did create;
> 2. directories that he did create, the whole tree, including files and subdirectories that he didn't create!

_Table of Contents_:
- [Background:](#background)
- [Scenario: run without --cleanup](#scenario-run-without---cleanup)
- [Scenario: run with --cleanup](#scenario-run-with---cleanup)

### Background:

- Given there is no dir `dir1`
- Given there is no dir `dir3`
- Given there is no file `f1`
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
