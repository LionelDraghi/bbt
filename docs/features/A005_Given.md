<!-- omit from toc -->
## Feature

Given steps are the first leg of the three-legged Steps stool.
It is meant to check and setup the execution environment, by:
- checking some dir or file exists or doesn't exists, or by
- running something that should be run first.

_Table of Contents:_
- [Scenario: Checking that there is no file or dir](#scenario-checking-that-there-is-no-file-or-dir)
- [Scenario: Checking that there is some dir](#scenario-checking-that-there-is-some-dir)
- [Scenario: Checking that there is a file with some content](#scenario-checking-that-there-is-a-file-with-some-content)
- [Scenario: Creating a file with some content](#scenario-creating-a-file-with-some-content)

### Scenario: Checking that there is no file or dir
- Given there is no dir `dir1`
- Given there is no file `file1`

- Then there is no dir `dir1`
- Then there is no file `file1`

### Scenario: Checking that there is some dir
- Given the directory `dir2`

- Then there is a dir `dir2`

### Scenario: Checking that there is a file with some content
- Given there is no file `file3`
- Given the file `file3`
~~~
Hello world!
~~~

- Then file `file3` is `Hello World!`

### Scenario: Creating a file with some content
- Given the file `file4` containing `alpha`
- Given the file `file5` containing 
~~~
beta
zeta
~~~

- Then file `file4` is `alpha`
- Then file `file5` is
~~~
beta
zeta
~~~

