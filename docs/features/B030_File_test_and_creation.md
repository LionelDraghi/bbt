<!-- omit from toc -->
## Feature : testing the existence of a file

bbt aims at facilitating the developer's life!  
So when the precondition (the `Given` step) says that there should no `config.ini` file, bbt will not only check that a file does not exist, but he will also propose to erase it if there's one.  

When in a test suite, this test should be run with the --yes option, otherwise it will stop and prompt the user to confirm the erasing of the file.

If run in interactive mode, the expected behavior depends on the user answer : 
- If it answer "yes", the test will be OK.
- And logically, if it answer "no", the test will fail.

> [!NOTE]
> This last case is not yet tested because bbt doesn't support for now prompt interaction. (Fixme:)  
> Meaning that this test can not be run in a test suite.

- [Scenario : a required file does not exist](#scenario--a-required-file-does-not-exist)
- [Scenario : the required file is created](#scenario--the-required-file-is-created)
- [Scenario : "Given there is no", when there actually is, should erase the file](#scenario--given-there-is-no-when-there-actually-is-should-erase-the-file)

### Scenario : a required file does not exist 

- Given there is no file `config.ini`
- When I run `sut read config.ini`
- Then I get error

### Scenario : the required file is created

  - Given my favorite and so useful `config.ini` file
```
Tmp_dir=/tmp
Alias l="ls -tla"
```
- Then `config.ini` contains `Tmp_dir=/tmp`

 ### Scenario : "Given there is no", when there actually is, should erase the file 


- Given there is no `config.ini` file  
- Then there is no more `config.ini` file
 
