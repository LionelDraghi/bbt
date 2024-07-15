
## Feature: **Exec Dir** 

bbt is able to run a scenario in a specific directory, provided on the command line.
The first scenario is run 

### Background: create some dir and file

- Given there is no `dir1` directory
- Given there is no `dir2` directory


### Scenario: Lets run `create_tree` in the current dir

- Given the new `create_tree.md` file
```
# Scenario:
- Given the dir `dir1`
- Given the file `dir1/file1` 
```

- When I run `bbt create_tree.md`

- Then there is a `dir1` dir
- And there is a `dir1/file1` file

### Scenario: Lets run `create_tree` in ./dir2

- Given the new `dir2` directory

- When I run `bbt create_tree.md --auto_delete --exec_dir dir2`

- Then there is a `dir2/dir1` dir
- And there is a `dir2/dir1/file1` file
- And there is no `dir1` dir
