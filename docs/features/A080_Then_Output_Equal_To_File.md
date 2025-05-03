<!-- omit from toc -->
## Feature : output is equal to a file

Expected results may be very long.  
It's handy to be able to keep the expected output in an external file.  

_Table of Contents:_
- [Scenario : test `output is equal` keyword](#scenario--test-output-is-equal-keyword)

### Scenario : test `output is equal` keyword

- Given the file `help_message.txt`
```
sut simulate an "under test unit" that can create,
read, or append a text to a file, with typical
file and command line input / output / error situations.

Usage:
   sut create|read        file_name
   sut append      "Text" file_name
   sut delete             file_name   : prompt user to confirm deletion
   sut read_env           var_name    : display environment variable
   sut -h | --help or no command line : display this message
   sut -v | --version                 : display a version string

Return code:
  Return code is set to 1 when :
  - there is a command line error (unknown option for example)
  - there is a file error (unable to open the given file, for example)
  Return code is set to 0 otherwise.

Errors:
  Error messages are output to the Standard_Error output
  To test it, call create or read without file_name
  When calling append with a file_name but without text to
  append, an unhandled exception is raised.
```
- When I run `./sut -h`

- Then output is equal to file `help_message.txt`
