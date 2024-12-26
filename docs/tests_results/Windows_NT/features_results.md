
## [A010_Then_Contains_String.md](..\docs\features\A010_Then_Contains_String.md)  

  ### Feature: testing that a file contains a line  

  - [X] scenario [test on a string output](..\docs\features\A010_Then_Contains_String.md) pass  

  - [X] scenario [test on a multiline output](..\docs\features\A010_Then_Contains_String.md) pass  

  - [X] scenario [test on a file](..\docs\features\A010_Then_Contains_String.md) pass  


## [A020_Then_Contains.md](..\docs\features\A020_Then_Contains.md)  

  ### Feature: "contains" a string feature  

  - [X] scenario [test the standard output](..\docs\features\A020_Then_Contains.md) pass  

  - [X] scenario [compare with a different file](..\docs\features\A020_Then_Contains.md) pass  


## [A030_Then_Does_Not_Contain.md](..\docs\features\A030_Then_Does_Not_Contain.md)  

  ### Feature: checking that some string is not present in output or file  

  - [X] scenario [Successful checks](..\docs\features\A030_Then_Does_Not_Contain.md) pass  

  - [X] scenario [Failed "output doesnt contain"](..\docs\features\A030_Then_Does_Not_Contain.md) pass  

  - [X] scenario [Failed "file doesn't contain"](..\docs\features\A030_Then_Does_Not_Contain.md) pass  


## [A040_Example_Keyword.md](..\docs\features\A040_Example_Keyword.md)  

  ### Feature: The keyword `Example` is a synonym of the keyword `Scenario`.  

  - [X] scenario [NOK return code](..\docs\features\A040_Example_Keyword.md) pass  

  - [X] scenario [NOK return code](..\docs\features\A040_Example_Keyword.md) pass  


## [A050_Then_File_Is_Code_Fence.md](..\docs\features\A050_Then_File_Is_Code_Fence.md)  

  ### Feature: testing the "file is" feature  

  - [X] scenario [file is as expected](..\docs\features\A050_Then_File_Is_Code_Fence.md) pass  

  - [X] scenario [file is not as expected](..\docs\features\A050_Then_File_Is_Code_Fence.md) pass  


## [A060_Then_File_Equal_File_String.md](..\docs\features\A060_Then_File_Equal_File_String.md)  

  ### Feature: "file is" followed by a code span (a string)  

  - [X] scenario [test on a single line file](..\docs\features\A060_Then_File_Equal_File_String.md) pass  

  - [X] scenario [adding a second line to the file, so the same test should fail](..\docs\features\A060_Then_File_Equal_File_String.md) pass  


## [A065_Then_File_Equal_File_Code_Fence.md](..\docs\features\A065_Then_File_Equal_File_Code_Fence.md)  

  ### Feature: file is equal to a file  

  - [X] scenario [test `is equal to file` form](..\docs\features\A065_Then_File_Equal_File_Code_Fence.md) pass  

  - [X] scenario [test `is equal to file` when files are *not* equal](..\docs\features\A065_Then_File_Equal_File_Code_Fence.md) pass  

  - [X] scenario [test the negative form `is not equal to file`](..\docs\features\A065_Then_File_Equal_File_Code_Fence.md) pass  


## [A070_Then_Get_Code_Span.md](..\docs\features\A070_Then_Get_Code_Span.md)  

  ### Feature: checking an expected multiline output  

  - [X] scenario [asking for sut help](..\docs\features\A070_Then_Get_Code_Span.md) pass  

  - [X] scenario [causing an sut error with a long explanation](..\docs\features\A070_Then_Get_Code_Span.md) pass  


## [A080_Then_Output_Equal_To_File.md](..\docs\features\A080_Then_Output_Equal_To_File.md)  

  ### Feature: output is equal to a file  

  - [X] scenario [test `output is equal` keyword](..\docs\features\A080_Then_Output_Equal_To_File.md) pass  


## [A090_Then_Get_Error.md](..\docs\features\A090_Then_Get_Error.md)  

  ### Feature: return code test  

  - [X] scenario [NOK return code](..\docs\features\A090_Then_Get_Error.md) pass  

  - [X] scenario [OK return code](..\docs\features\A090_Then_Get_Error.md) pass  


## [A100_Then_Get_Stderr.md](..\docs\features\A100_Then_Get_Stderr.md)  

  ### Feature: stderr test  

  - [X] scenario [unknown option](..\docs\features\A100_Then_Get_Stderr.md) pass  


## [A110_Then_No_Output.md](..\docs\features\A110_Then_No_Output.md)  

  ### Feature: Check that there is no output  

  - [X] scenario [silent operation](..\docs\features\A110_Then_No_Output.md) pass  

  - [X] scenario [silent operation expected, but there is an output](..\docs\features\A110_Then_No_Output.md) pass  


## [A120_Then_Get_String.md](..\docs\features\A120_Then_Get_String.md)  

  ### Feature: checking a message line on stdout  

  - [X] scenario [asking for sut version](..\docs\features\A120_Then_Get_String.md) pass  


## [A130_Successfully_Keyword.md](..\docs\features\A130_Successfully_Keyword.md)  

  ### Feature: The “successfully” shortcut  

  - [X] scenario [*when I successfully run* a command with successful run](..\docs\features\A130_Successfully_Keyword.md) pass  

  - [X] scenario [*when I successfully run* a command with a wrong command line, returns an error status](..\docs\features\A130_Successfully_Keyword.md) pass  

  - [X] scenario [*when I run* a command with a wrong command line](..\docs\features\A130_Successfully_Keyword.md) pass  


## [A140_Unordered_Keyword.md](..\docs\features\A140_Unordered_Keyword.md)  

  ### Feature: when the modifier `unordered` is given after `get`, order of line is ignored  

  Feature background   
  - [X] scenario ["I get" without the modifier](..\docs\features\A140_Unordered_Keyword.md) pass  

  Feature background   
  - [X] scenario [same "I get" with the modifier](..\docs\features\A140_Unordered_Keyword.md) pass  

  Feature background   
  - [X] scenario ["contains" without the modifier](..\docs\features\A140_Unordered_Keyword.md) pass  

  Feature background   
  - [X] scenario [same "contains" but with the "unordered" modifier](..\docs\features\A140_Unordered_Keyword.md) pass  


## [A150_Background_Keyword.md](..\docs\features\A150_Background_Keyword.md)  

  ### Feature: Feature 1  

  Document Background Background1  
  Feature background Background2 in feature 1  
  - [X] scenario [lets erase what was created by previous background runs](..\docs\features\A150_Background_Keyword.md) pass  

  Document Background Background1  
  Feature background Background2 in feature 1  
  - [X] scenario [Two Background executed](..\docs\features\A150_Background_Keyword.md) pass  

  ### Feature: Feature 2  

  Document Background Background1  
  - [X] scenario [lets erase once more what was created by previous background runs](..\docs\features\A150_Background_Keyword.md) pass  

  Document Background Background1  
  - [X] scenario [only first background should apply](..\docs\features\A150_Background_Keyword.md) pass  


## [A160_Ignoring_Blank_Lines.md](..\docs\features\A160_Ignoring_Blank_Lines.md)  

  - [X] scenario [Comparing ignoring blank lines](..\docs\features\A160_Ignoring_Blank_Lines.md) pass  


## [A170_File_vs_File_Name.md](..\docs\features\A170_File_vs_File_Name.md)  

  ### Feature: expected content in a file  

  - [X] scenario ["Then I get file" form](..\docs\features\A170_File_vs_File_Name.md) pass  

  - [X] scenario ["Then output is | contains | does not contain file" form](..\docs\features\A170_File_vs_File_Name.md) pass  

  - [X] scenario ["Then file is | contains | does not contain file" form](..\docs\features\A170_File_vs_File_Name.md) pass  


## [B010_Deleting_created_files.md](..\docs\features\B010_Deleting_created_files.md)  

  ### Feature: tmp files and directories deletion  

  - [X] scenario [run without --cleanup](..\docs\features\B010_Deleting_created_files.md) pass  

  - [X] scenario [run with --cleanup](..\docs\features\B010_Deleting_created_files.md) pass  


## [B020_Exec_dir.md](..\docs\features\B020_Exec_dir.md)  

  ### Feature: **Exec Dir**  

  Feature background create some dir and file  
  - [X] scenario [Lets run `create_tree` in the current dir](..\docs\features\B020_Exec_dir.md) pass  

  Feature background create some dir and file  
  - [X] scenario [Lets run `create_tree` in ./dir2](..\docs\features\B020_Exec_dir.md) pass  


## [B030_File_test_and_creation.md](..\docs\features\B030_File_test_and_creation.md)  

  ### Feature: testing the existence of a file  

  - [X] scenario [a required file does not exist](..\docs\features\B030_File_test_and_creation.md) pass  

  - [X] scenario [the required file is created](..\docs\features\B030_File_test_and_creation.md) pass  

  - [X] scenario ["Given there is no", when there actually is, should erase the file](..\docs\features\B030_File_test_and_creation.md) pass  


## [B040_Find_scenarios.md](..\docs\features\B040_Find_scenarios.md)  

  ### Feature: multiples scenarios given in command line  

  Feature background   
  - [X] scenario [no file or dir on the command line](..\docs\features\B040_Find_scenarios.md) pass  

  Feature background   
*** NOK : Then the output is (unordered) (..\docs\features\b040_find_scenarios.md:60:)  
Output:  
| dir1\scen1.md  
| dir1\scen2.md  

not equal to expected:  
| dir1/scen2.md  
| dir1/scen1.md  

  
  - [ ] scenario [running all scenarios in dir1](..\docs\features\B040_Find_scenarios.md) fails  

  Feature background   
  - [X] scenario [running multiple scenarios given on command line](..\docs\features\B040_Find_scenarios.md) pass  

  Feature background   
*** NOK : Then the output is (unordered) (..\docs\features\b040_find_scenarios.md:89:)  
Output:  
| dir1\dir2\scen5.md  
| dir1\dir3\dir4\scen6.md  
| dir1\scen1.md  
| dir1\scen2.md  

not equal to expected:  
| dir1/scen2.md  
| dir1/scen1.md  
| dir1/dir3/dir4/scen6.md  
| dir1/dir2/scen5.md  

  
  - [ ] scenario [running scenarios in a tree thanks to `-r`](..\docs\features\B040_Find_scenarios.md) fails  

  Feature background   
  - [X] scenario [error msg when trying to run scenarios, but none found in given directories](..\docs\features\B040_Find_scenarios.md) pass  

  Feature background   
  - [X] scenario [empty list file on -lf if there is no scenario in given directories](..\docs\features\B040_Find_scenarios.md) pass  


## [B050_Strict_gherkin.md](..\docs\features\B050_Strict_gherkin.md)  

  ### Feature: Strict Gherkin rules  

  - [X] scenario [Multiple When in a scenario](..\docs\features\B050_Strict_gherkin.md) pass  


## [B060_Return_code_on_test_failure.md](..\docs\features\B060_Return_code_on_test_failure.md)  

  ### Feature: return code on test failure  

  - [X] scenario [return code on test success](..\docs\features\B060_Return_code_on_test_failure.md) pass  

  - [X] scenario [return code when the test fail](..\docs\features\B060_Return_code_on_test_failure.md) pass  

  - [X] scenario [return code when one fail and the other succeed](..\docs\features\B060_Return_code_on_test_failure.md) pass  


## [B070_Mandatory_new_bug.md](..\docs\features\B070_Mandatory_new_bug.md)  

  Document Background   
  - [X] scenario [](..\docs\features\B070_Mandatory_new_bug.md) pass  


## [B080_Keep_Going.md](..\docs\features\B080_Keep_Going.md)  

  ### Feature: on error, Keep going or stop  

  Feature background   
  - [X] scenario [with `-k`](..\docs\features\B080_Keep_Going.md) pass  

  Feature background   
  - [X] scenario [without `-k`](..\docs\features\B080_Keep_Going.md) pass  


## [C010_Empty_scenarios.md](..\docs\features\C010_Empty_scenarios.md)  

  - [X] scenario [No step test](..\docs\features\C010_Empty_scenarios.md) pass  

  - [X] scenario [No scenario test](..\docs\features\C010_Empty_scenarios.md) pass  


## [C020_Escaped_char.md](..\docs\features\C020_Escaped_char.md)  

  ### Feature: Space in names  

*** NOK : then I get no error (..\docs\features\c020_escaped_char.md:12:)  
No error expected, but got one ( 1)  
*** NOK : And there is a file `dir 1/file 1` (..\docs\features\c020_escaped_char.md:13:)  
Expected file "dir 1/file 1" doesn't exists  
  - [ ] scenario [Directory with space in the name](..\docs\features\C020_Escaped_char.md) fails  


## [C030_Markdown_syntax.md](..\docs\features\C030_Markdown_syntax.md)  

  ### Feature: Tolerance to formatting variations  

  - [X] scenario [Heading variations](..\docs\features\C030_Markdown_syntax.md) pass  

  - [X] scenario [Missing heading marker](..\docs\features\C030_Markdown_syntax.md) pass  


## [C040_Missing_title.md](..\docs\features\C040_Missing_title.md)  

  - [X] scenario [Missing tittle in scenario, background and feature](..\docs\features\C040_Missing_title.md) pass  


## [C050_Step_marker.md](..\docs\features\C050_Step_marker.md)  

  ### Feature:   

  - [X] scenario [](..\docs\features\C050_Step_marker.md) pass  


------------------
- Failed     =  3
- Successful =  65
- Empty      =  0
- Not run    =  0
