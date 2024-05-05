

- [Scenario : a required file does not exist (should fail)](#scenario--a-required-file-does-not-exist-should-fail)
- [Scenario : same as previous, but with the keyword file before the file name (should fail)](#scenario--same-as-previous-but-with-the-keyword-file-before-the-file-name-should-fail)
- [Scenario : the required file is created](#scenario--the-required-file-is-created)

<!-- omit from toc -->
## Feature : testing the existence of a file

Test both the ability to check that a file exists, or to directly create one

### Scenario : a required file does not exist (should fail)

  - Given the existing file `config.ini`
  - When I run `uut read config.ini`
  - Then I get error


### Scenario : same as previous, but with the keyword file before the file name (should fail) 

  - Given the existing file `config.ini`
  - When I run `uut read config.ini`
  - Then I get error


### Scenario : the required file is created
  - Given my favorite and so useful `config.ini` file
```
Tmp_dir=/tmp
Alias l="ls -tla"
```
  - When I run `uut read config.ini`
  - Then I get no error

 
