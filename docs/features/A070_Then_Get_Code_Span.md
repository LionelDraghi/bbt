## Feature : checking an expected multiline output

Test an expected multi-lines output.

### Scenario : asking for sut help

  - When I run `sut -h`
  - Then I get 

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

### Scenario : causing an sut error with a long explanation

  - When I run `sut -e append `
  - Then I get on stderr

```
unknown option -e
Missing file name
```

 