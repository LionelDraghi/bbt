
- [Scenario : a required file does not exist](#scenario--a-required-file-does-not-exist)
- [Scenario : the required file is created](#scenario--the-required-file-is-created)
- [Scenario : "Given there is no", when there actually is, should erase the file](#scenario--given-there-is-no-when-there-actually-is-should-erase-the-file)

<!-- omit from toc -->
## Feature : testing the existence of a file

Test both the ability to check that a file exists, or to directly create one

### Scenario : a required file does not exist 

- Given there is no file `config.ini`
- When I run `uut read config.ini`
- Then I get error

### Scenario : the required file is created

  - Given my favorite and so useful `config.ini` file
```
Tmp_dir=/tmp
Alias l="ls -tla"
```
- Then `config.ini` contains `Tmp_dir=/tmp`

 ### Scenario : "Given there is no", when there actually is, should erase the file 

Note that the expected behavior depends on the --auto_delete option on command line.  
This test will pass when bbt is called with this option, and fail otherwise.  
Refer to the doc regarding auto erase. 

- Given there is no `config.ini` file  
- Then there is no more `config.ini` file
 
