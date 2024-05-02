## Feature : checking an expected multiline output

Test an app returning a long text

### Scenario : asking for uut help

  - When I run `uut -h`
  - Then I get 

```
uut -h -v [create|read file_name] [append "Text" file_name]
   uut simulate an "under test unit" that can create,
   read, or append a text to a file,
Options:
   -v | --version         : display a version string
   -h | --help
      or no command line  : display this message
Return code:
  Return code is set to 1 when :
  - there is a command line error (unknown option for example)
  - there is a file error (unable to open the given file, for example)
  Return code is set to 0 otherwise.
```

### Scenario : causing an uut error with a long explanation

  - When I run `uut -e append `
  - Then I get on stderr

```
Cannot append to file
…..
```

 