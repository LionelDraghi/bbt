# Grand titre

## Feature : directory management

### Scenario: Given directory

- Given the directory `dir1`
- when I run `uut create dir1/file\ 1` 
- then I get no error

### Scenario: Given existing directory

- Given the existing directory `dir1`
- when I run `uut create dir1/file2` 
- then I get no error