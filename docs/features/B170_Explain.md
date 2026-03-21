# Feature: Explain

<!-- omit from toc -->
## Description

This feature output what is understood by `bbt` in natural language.
Line that are not used by bbt are ignored.
There is one scenario per Action, cf `bbt list_grammar` for the list (that is defined in package bbt-model.steps.ads).

--- 

_Table of Contents_:
- [Feature: Explain](#feature-explain)
  - [Scenario explain elements other than steps](#scenario-explain-elements-other-than-steps)
  - [Scenario explain action Setup\_No\_File](#scenario-explain-action-setup_no_file)
  - [Scenario explain action Setup\_No\_Dir](#scenario-explain-action-setup_no_dir)
  - [Scenario explain action Check\_File\_Existence](#scenario-explain-action-check_file_existence)
  - [Scenario explain action Check\_Dir\_Existence](#scenario-explain-action-check_dir_existence)
  - [Scenario explain action Erase\_And\_Create](#scenario-explain-action-erase_and_create)
  - [Scenario explain action Create\_If\_None](#scenario-explain-action-create_if_none)
  - [Scenario explain action Run](#scenario-explain-action-run)
  - [Scenario explain action Run\_Without\_Error](#scenario-explain-action-run_without_error)
  - [Scenario explain action Check\_No\_File](#scenario-explain-action-check_no_file)
  - [Scenario explain action Check\_No\_Dir](#scenario-explain-action-check_no_dir)
  - [Scenario explain action Error\_Return\_Code and No\_Error\_Return\_Code](#scenario-explain-action-error_return_code-and-no_error_return_code)
  - [Scenario explain action Output\_Is](#scenario-explain-action-output_is)
  - [Scenario explain action Output\_Contains and Output\_Does\_Not\_Contain](#scenario-explain-action-output_contains-and-output_does_not_contain)
  - [Scenario explain action Output|File\_Matches and Output|File\_Does\_Not\_Match](#scenario-explain-action-outputfile_matches-and-outputfile_does_not_match)
  - [Scenario explain action File\_Is and File\_Is\_Not](#scenario-explain-action-file_is-and-file_is_not)
  - [Scenario explain action File\_Contains and File\_Does\_Not\_Contain](#scenario-explain-action-file_contains-and-file_does_not_contain)
  - [Scenario explain action No\_Output](#scenario-explain-action-no_output)

## Scenario explain elements other than steps

- Given the new file `scenario_to_explain.md` 
~~~
## Ignored header

- ignored bullet point

## Feature F1

Ignored text

## Background B1
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 

```
Document `scenario_to_explain.md`  
5: Feature `F1`  
9: Scenario `B1`  
```

## Scenario explain action Setup_No_File

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given there is no `config.ini` file
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
~~~
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that there is no file `config.ini` (erase it if needed)  
~~~

## Scenario explain action Setup_No_Dir

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given there is no `dir1` directory
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
~~~
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that there is no directory `dir1` (erase it if needed)  
~~~

## Scenario explain action Check_File_Existence

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given there is a `config.ini` file
- Then  there is a `config.ini` file
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
~~~
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that file `config.ini` exists  
3: - Check that file `config.ini` exists  
~~~

## Scenario explain action Check_Dir_Existence

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given there is a `dir1` directory
- Then there is a `dir1` directory
~~~
- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
~~~
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that directory `dir1` exists  
3: - Check that directory `dir1` exists  
~~~

## Scenario explain action Erase_And_Create

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given the new `dir1` directory
- Given the new `file1` file containing `string_A`
- Given the new `file2` file 
```
line_A
```
~~~
- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that there is a new directory named `dir1`(create if none, overwrite otherwise)  
3: - Ensure that there is a new file named `file1` containing `string_A`(create if none, overwrite otherwise)  
4: - Ensure that there is a new file named `file2` containing   
~~~  
line_A  
~~~  
(create if none, overwrite otherwise)  
```

## Scenario explain action Create_If_None

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given the dir `dir1`
- Given the `file1` file containing `string_A`
- Given the `file2` file 
```
line_A
```
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that there is a directory named `dir1`(create if none, fail otherwise)  
3: - Ensure that there is a file named `file1` containing `string_A`(create if none, fail otherwise)  
4: - Ensure that there is a file named `file2` containing   
~~~  
line_A  
~~~  
(create if none, fail otherwise)
```

## Scenario explain action Run

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given I run `sut --help` 
- When  I run `sut --help` 
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Run command `sut --help`
3: - Run command `sut --help`
```

## Scenario explain action Run_Without_Error

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given I successfully run `sut --help` 
- When  I successfully run `sut --help` 
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Run command `sut --help` and check that it does not return an error    
3: - Run command `sut --help` and check that it does not return an error 
```

## Scenario explain action Check_No_File

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given there is no `config.ini` file
- Then  there is no `config.ini` file
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
~~~
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that there is no file `config.ini` (erase it if needed)  
3: - Check that file `config.ini` does not exist
~~~

## Scenario explain action Check_No_Dir

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Given there is no `dir1` directory
- Then  there is no `dir1` directory
~~~
- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
~~~
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Ensure that there is no directory `dir1` (erase it if needed)  
3: - Check that directory `dir1` does not exist
~~~

## Scenario explain action Error_Return_Code and No_Error_Return_Code

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- Then there is no error
- Then I get    no error
- Then there is an error
- Then I get    an error
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
~~~
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Check that command does not return an error  
3: - Check that command does not return an error  
4: - Check that command returns an error  
5: - Check that command returns an error
~~~

## Scenario explain action Output_Is

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- then output is file `expected.txt`
- then output is 
```
line_A
```
- then I get `msg`
- Then I get file `flowers2.txt`
- then I get 
```
line_B
```
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Check  that output is as in file `expected.txt`  
3: - Check  that output is   
~~~  
line_A  
~~~  
7: - Check  that output is `msg`  
8: - Check  that output is as in file `flowers2.txt`  
9: - Check  that output is   
~~~  
line_B  
~~~  
```

## Scenario explain action Output_Contains and Output_Does_Not_Contain

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- then output contains `text`
- then output contains `expected.txt` file
- then output contains
```
line_A
```
- then output does not contain `text`
- then output does not contain `expected.txt` file
- then output does not contain
```
line_B
```
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Check that output contains `text`  
3: - Check that output contains as in file `expected.txt`  
4: - Check that output contains   
~~~  
line_A  
~~~  
8: - Check that output does not contain `text`  
9: - Check that output does not contain as in file `expected.txt`  
10: - Check that output does not contain   
~~~  
line_B  
~~~  
```

## Scenario explain action Output|File_Matches and Output|File_Does_Not_Match

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- then `config.ini` matches `regexp`
- then `config.ini` does not match `regexp`
- then output matches `regexp`
- then output does not match `regexp`
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
1: Scenario `1`  
2: - Check that file `config.ini` content matches regex `regexp`  
3: - Check that file `config.ini` content does not match regex `regexp`  
4: - Check that command output matches regex `regexp`  
5: - Check that command output does not match regex `regexp`
```

## Scenario explain action File_Is and File_Is_Not

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- then `config.ini` is `mode=silent`
- then `config.ini` is equal to file `expected/config.ini`
- then `config.ini` is
```
line_A
```
- then `config.ini` is no more equal to file `previous_config.ini`
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
  
1: Scenario `1`  
2: - Check that file `config.ini` content is exactly `mode=silent`  
3: - Check that file `config.ini` content is exactly `expected/config.ini` file content  
4: - Check that file `config.ini` content is exactly   
~~~  
line_A  
~~~  
  
8: - Check that file `config.ini` content is not exactly ``previous_config.ini` file content
```

## Scenario explain action File_Contains and File_Does_Not_Contain

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- then `config.ini` contains `mode=silent`
- then `config.ini` contains file `snippet.txt`
- then `config.ini` contains
```
line_A
```
- then `config.ini` does not contain `mode=silent`
- then `config.ini` does not contain file `snippet.txt`
- then `config.ini` does not contain
```
line_B
```
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
  
1: Scenario `1`  
2: - Check that file `config.ini contains `mode=silent`  
3: - Check that file `config.ini contains `snippet.txt` file content  
4: - Check that file `config.ini contains   
~~~  
line_A  
~~~  
  
8: - Check that file `config.ini does not contain `mode=silent`  
9: - Check that file `config.ini does not contain `snippet.txt` file content  
10: - Check that file `config.ini does not contain   
~~~  
line_B  
~~~  
```
## Scenario explain action No_Output

- Given the new file `scenario_to_explain.md` 
~~~
## Scenario 1  
- then I get no output
- then there is no output
~~~

- When I run `./bbt explain scenario_to_explain.md`

- Then I get 
```
Document `scenario_to_explain.md`  
  
1: Scenario `1`  
2: - Check that last command produces no output  
3: - Check that last command produces no output
```

