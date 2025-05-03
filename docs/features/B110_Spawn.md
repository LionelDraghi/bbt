<!-- omit from toc -->
## Feature: Command line to spawn processing

_Table of Contents:_
- [Scenario: Directory with space in the name (Unix\_Only)](#scenario-directory-with-space-in-the-name-unix_only)
- [Scenario : Command with quoted arguments (Unix\_Only)](#scenario--command-with-quoted-arguments-unix_only)

### Scenario: Directory with space in the name (Unix_Only)

Created file or dir may contain space chars.
On Unix, those spaces need to be escaped with a backslash, but only in the `run` command.  
([Issue #2](https://github.com/LionelDraghi/bbt/issues/2#issue-2406271975))

- Given the directory `dir 1`
- when I run `./sut create dir\ 1/file\ 1` 
- then I get no error
- And there is a file `dir 1/file 1`
    
### Scenario : Command with quoted arguments (Unix_Only)

bbt is using quotes for joining arguments as the shell, but the quotes should not be passed to the executed program.  
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