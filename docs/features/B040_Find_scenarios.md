<!-- omit from toc -->
## Feature: multiples scenarios given in command line

bbt is able to run multiple scenario files, according to the command line.    
It is possible to specify one or more directories.  

It is also possible to use globing character and to run, for example, `bbt tests/*.scen`  
Note that this is not really tested here, as the globbing expansion is done by the shell. bbt will actually be called with `bbt tests/file1.scen tests/file2.scen` etc. (but this case is tested)

> [!NOTE]
> If search directories are specified, but no scenario file name, bbt will consider every .md file found as a scenario.  
> This behavior is subject to discussion, and could be modified in the future.  

- [Background:](#background)
- [Scenario: no file or dir on the command line](#scenario-no-file-or-dir-on-the-command-line)
- [Scenario: running all scenarios in dir1](#scenario-running-all-scenarios-in-dir1)
- [Scenario: running multiple scenarios given on command line](#scenario-running-multiple-scenarios-given-on-command-line)
- [Scenario: running scenarios in a tree thanks to `-r`](#scenario-running-scenarios-in-a-tree-thanks-to--r)
- [Scenario: error msg when trying to run scenarios, but none found in given directories](#scenario-error-msg-when-trying-to-run-scenarios-but-none-found-in-given-directories)
- [Scenario: empty list file on -lf if there is no scenario in given directories](#scenario-empty-list-file-on--lf-if-there-is-no-scenario-in-given-directories)

### Background:
- Given the new `dir1` directory
- Given the `dir1/scen1.md` file
```
# scenario:
- Given the `dir1/res1` file containing `res1`
``` 

- Given the `dir1/scen2.md` file
```
# scenario:
- Given the `dir1/res2` file containing `res2`
``` 

- Given the `dir1/scen3.scen` file
```
# scenario:
- Given the `dir1/res3` file containing `res3`
``` 

- Given the `dir1/scen4.scen` file
```
# scenario:
- Given the `dir1/res4` file containing `res4`
``` 

### Scenario: no file or dir on the command line

This is a change in the 0.0.4 respect to previous versions.
Default behavior was to search and run every .md file in the current dir.  
This behavior was retained dangerous, and moreover, unexpected : most command just print the help when called with an empty command line.

- When I run `./bbt`
- Then the output contains
```
Usage : bbt [Options]* [Command] [bbt_file]*
````
- And there is no error


### Scenario: running all scenarios in dir1
- When I run `./bbt -lf dir1`
- Then the output is (unordered)
```
dir1/scen2.md
dir1/scen1.md
```

### Scenario: running multiple scenarios given on command line

- When I run `./bbt -lf dir1/scen3.scen dir1/scen4.scen`  
Note that this command line is the result of shell expansion of 
`bbt -lf dir1/*.scen`

- Then the output is
```
dir1/scen3.scen
dir1/scen4.scen
```

### Scenario: running scenarios in a tree thanks to `-r`

- Given the `dir1/dir2/` dir
- Given the `dir1/dir3/dir4` dir
- Given the `dir1/dir2/scen5.md` file containing `foo`
- Given the `dir1/dir3/dir4/scen6.md` file containing `bar`
```
# scenario:
- Given the `dir1/res1` file containing `res1`
``` 
- When I run `./bbt -r -lf dir1`
- Then the output is (unordered)
```
dir1/scen2.md
dir1/scen1.md
dir1/dir3/dir4/scen6.md
dir1/dir2/scen5.md
```

### Scenario: error msg when trying to run scenarios, but none found in given directories

- Given the `dir5` dir
- Given the `dir6` dir
- Given the `dir6/dir7` dir

- When I run `./bbt dir5 dir6`
- Then the output contains
```
Error : No md file found
```
- And I get an error

### Scenario: empty list file on -lf if there is no scenario in given directories

- When I run `./bbt -lf dir5 dir6`
- Then I get no output
- And I get no error
