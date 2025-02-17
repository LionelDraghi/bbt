## Feature: Command line to spawn processing

### Scenario: Directory with space in the name

Created file or dir may contain space chars.
On Unix, those spaces need to be escaped with a backslash, but only in the `run` command.  
([Issue #2](https://github.com/LionelDraghi/bbt/issues/2#issue-2406271975))

Fixme: this scenario is Unix specific, and should not be run on Windows.  
This scenario fails on Windows.

- Given the directory `dir 1`
- when I run `./sut create dir\ 1/file\ 1` 
- then I get no error
- And there is a file `dir 1/file 1`


### Scenario : Command with quoted arguments

bbt is using quotes for joining arguments as the shell, but the quotes should not be
passed to the executed program.  
([Issue #11](https://github.com/LionelDraghi/bbt/issues/11))

- Given the new file `tmp.txt`
~~~
~~~
  
- When I run `./sut append "Hello world" tmp.txt`
- Then I get no error
- And file `tmp.txt` is `Hello world`

- When I run `./sut append Bye tmp.txt`
- Then I get no error
- And file `tmp.txt` is 
~~~
Hello world
Bye
~~~