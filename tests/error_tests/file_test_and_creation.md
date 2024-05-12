

- [Scenario : a required file does not exist](#scenario--a-required-file-does-not-exist)
- [Scenario : same as previous, but with the keyword file before the file name](#scenario--same-as-previous-but-with-the-keyword-file-before-the-file-name)
- [Scenario : the required file is created](#scenario--the-required-file-is-created)
- [Scenario : "Given there is no", but there is! (should fail)](#scenario--given-there-is-no-but-there-is-should-fail)

<!-- omit from toc -->
## Feature : testing the existence of a file

Test both the ability to check that a file exists, or to directly create one

### Scenario : a required file does not exist 

  - Given there is no file `config.ini`
  - When I run `uut read config.ini`
  - Then I get error


### Scenario : same as previous, but with the keyword file before the file name 

  - Given there is no `config.ini` file 
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

 ### Scenario : "Given there is no", but there is! (should fail) 

  - Given there is no `config.ini` file 
 
