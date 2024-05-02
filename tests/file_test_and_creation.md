
## Feature : testing the existence of a file

Test both the ability to check that a file exists, or to directly create one

### Scenario outline : a required file does not exist  

  - Given the existing file `config.ini`
  - When I run `uut read config.ini`
  - Then I get error


### Scenario outline : same as previous, but with the keyword file before the file name  

  - Given the existing file `config.ini`
  - When I run `uut read config.ini`
  - Then I get error


### Scenario outline : the required file is created
  - Given the `config.ini` file
```
Tmp_dir=/tmp
Alias l="ls -tla"
```
  - When I run `uut read config.ini`
  - Then I get no error

 
