## Feature

Given steps are the first leg of the three-legged Steps stool.
It is meant to check and setup the execution environment, by:
- checking some dir or file exists or doesn't exists, or by
- running something that should be run first.

### Scenario: Checking that there is no file or dir
- Given there is no dir `dir1`
- Given there is no file `file1`

- Then there is no dir `dir1`
- Then there is no file `file1`

### Scenario: Checking that there is some dir
- Given the directory `dir2`

- Then there is a dir `dir2`

### Scenario: Checking that there is a file with some content
- Given there is no `file3`
- Given the file `file3`
~~~
Hello world!
~~~

- Then file `file3` is `Hello World!`

