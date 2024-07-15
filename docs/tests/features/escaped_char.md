## Feature: Space in names

Created file or dir may contain the space char.
Those spaces need to be escaped with a backslash only in the `run` command, for now. 

[Issue #2](https://github.com/LionelDraghi/bbt/issues/2#issue-2406271975)

### Scenario: Directory with space in the name

- Given the directory `dir 1`
- when I run `uut create dir\ 1/file\ 1` 
- then I get no error
- And there is a file `dir 1/file 1`
