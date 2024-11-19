## Feature: tmp files and directories deletion

Scenarios may create a lot of file and directory during run, and that's tedious to remove.  
The --cleanup option is here to ensure that those files are removed after run.

> [!NOTE] bbt will remove file that he create, but not files created by the run external commands. 
> This is mostly feasible, by taking a snapshot of the file system before and after the run, but is it desirable?

### Scenario: run without --cleanup

- Given the new `create_tree.md` file
```md
# Scenario: create tree
  - Given the new dir `dir1`
  - And   the new dir `dir1/dir2`
  - And   the new file `dir1/dir2/f2` containing `text 2`
  - And   the new file `f1` containing `text 1`
  - And   the new dir `dir3/dir4/dir5`

  - Then there is a `dir1/dir2/f2` file
  - And  there is a `f1` file
```

- When I run `./bbt --yes create_tree.md`

- Then there is a `dir1/dir2/f2` file
- And  there is a `f1` file

### Scenario: run with --cleanup

- When I run `./bbt --yes --cleanup create_tree.md`

- Then there is no `dir1` directory
- And  there is no `f1` file

[Issue #3](https://github.com/LionelDraghi/bbt/issues/3) this tests fails : - Then there is no `dir3` directory
