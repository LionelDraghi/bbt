<!-- omit from toc -->
## Feature: **Exec Dir** 

bbt is able to run a scenario in a specific directory, provided on the command line.  
> [!WARNING]
> Note that if the scenario depends on an external file, it must be placed in the exec dir.

_Table of Contents_:
- [Background: create some dir and file](#background-create-some-dir-and-file)
- [Scenario: Lets run `create_tree` in the current dir](#scenario-lets-run-create_tree-in-the-current-dir)
- [Scenario: Lets run `create_tree` in ./dir2](#scenario-lets-run-create_tree-in-dir2)

### Background: create some dir and file

- Given there is no `dir1` directory
- Given there is no `dir2` directory
- Given the new `create_tree.md` file
```
# Scenario:
- Given the dir `dir1`
- Given the file `dir1/file1` 
```

### Scenario: Lets run `create_tree` in the current dir

- When I run `./bbt create_tree.md`

- Then there is a `dir1` dir
- And there is a `dir1/file1` file

### Scenario: Lets run `create_tree` in ./dir2

- Given the new `dir2` directory

- When I run `./bbt create_tree.md --exec_dir dir2`

- Then there is a `dir2/dir1` dir
- And there is a `dir2/dir1/file1` file
- And there is no `dir1` dir
