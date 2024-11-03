
## [multiline.md](../features/multiline.md)  

  ### Feature: checking an expected multiline output  

  - [X] scenario [asking for uut help](../features/multiline.md) pass  

  - [X] scenario [causing an uut error with a long explanation](../features/multiline.md) pass  


## [find_scenarios.md](../features/find_scenarios.md)  

  ### Feature: multiples scenarios given in command line  

  - [X] scenario [no file or dir on the command line](../features/find_scenarios.md) pass  

  - [X] scenario [running all scenarios in dir1](../features/find_scenarios.md) pass  

  - [X] scenario [running multiple scenarios given on command line](../features/find_scenarios.md) pass  

  - [X] scenario [running scenarios in a tree thanks to `-r`](../features/find_scenarios.md) pass  

  - [X] scenario [error msg when trying to run scenarios, but none found in given directories](../features/find_scenarios.md) pass  

  - [X] scenario [empty list file on -lf if there is no scenario in given directories](../features/find_scenarios.md) pass  


## [mandatory_new_bug.md](../features/mandatory_new_bug.md)  

  - [X] scenario [](../features/mandatory_new_bug.md) pass  


## [escaped_char.md](../features/escaped_char.md)  

  ### Feature: Space in names  

  - [X] scenario [Directory with space in the name](../features/escaped_char.md) pass  


## [file_is_code_fence.md](../features/file_is_code_fence.md)  

  ### Feature: testing the "file is" feature  

  - [X] scenario [file is as expected](../features/file_is_code_fence.md) pass  

  - [X] scenario [file is not as expected](../features/file_is_code_fence.md) pass  


## [output_equal_to_file.md](../features/output_equal_to_file.md)  

  ### Feature: output is equal to a file  

  - [X] scenario [test `output is equal` keyword](../features/output_equal_to_file.md) pass  


## [example_keyword.md](../features/example_keyword.md)  

  ### Feature: The keyword `Example` is a synonym of the keyword `Scenario`.  

  - [X] scenario [NOK return code](../features/example_keyword.md) pass  

  - [X] scenario [NOK return code](../features/example_keyword.md) pass  


## [ill_formated_file.md](../features/ill_formated_file.md)  

  ### Feature: Ill formatted file detection  

  - [X] scenario [Missing scenario](../features/ill_formated_file.md) pass  


## [missing_title.md](../features/missing_title.md)  

  - [X] scenario [Missing tittle in scenario, background and feature](../features/missing_title.md) pass  


## [background.md](../features/background.md)  

  ### Feature: Feature 1  

  - [X] scenario [lets erase what was created by previous background runs](../features/background.md) pass  

  - [X] scenario [Two Background executed](../features/background.md) pass  

  ### Feature: Feature 2  

  - [X] scenario [lets erase once more what was created by previous background runs](../features/background.md) pass  

  - [X] scenario [only first background should apply](../features/background.md) pass  


## [exec_dir.md](../features/exec_dir.md)  

  ### Feature: **Exec Dir**  

  - [X] scenario [Lets run `create_tree` in the current dir](../features/exec_dir.md) pass  

  - [X] scenario [Lets run `create_tree` in ./dir2](../features/exec_dir.md) pass  


## [stderr.md](../features/stderr.md)  

  ### Feature: stderr test  

  - [X] scenario [unknown option](../features/stderr.md) pass  


## [unordered.md](../features/unordered.md)  

  ### Feature: when the modifyer `unordered` is given after `get`, order of line is ignored  

  - [X] scenario ["I get" without the modifyer](../features/unordered.md) pass  

  - [X] scenario [same "I get" with the modifyer](../features/unordered.md) pass  


## [contains_multiline.md](../features/contains_multiline.md)  

  ### Feature: "contains" a string feature  

  - [X] scenario [test the standard output](../features/contains_multiline.md) pass  

  - [X] scenario [compare with a different file](../features/contains_multiline.md) pass  


## [return_code_on_test_failure.md](../features/return_code_on_test_failure.md)  

  ### Feature: return code on test failure  

  - [X] scenario [return code on test success](../features/return_code_on_test_failure.md) pass  

  - [X] scenario [return code when the test fail](../features/return_code_on_test_failure.md) pass  

  - [X] scenario [return code when one fail and the other succeed](../features/return_code_on_test_failure.md) pass  


## [file_is_string.md](../features/file_is_string.md)  

  ### Feature: "file is" followed by a code span (a string)  

  - [X] scenario [test on a single line file](../features/file_is_string.md) pass  

  - [X] scenario [adding a second line to the file, so the same test should fail](../features/file_is_string.md) pass  


## [contains_line.md](../features/contains_line.md)  

  ### Feature: testing that a file contains a line  

  - [X] scenario [test on a string output](../features/contains_line.md) pass  

  - [X] scenario [test on a multiline output](../features/contains_line.md) pass  

  - [X] scenario [test on a file](../features/contains_line.md) pass  


## [return_code.md](../features/return_code.md)  

  ### Feature: return code test  

  - [X] scenario [NOK return code](../features/return_code.md) pass  

  - [X] scenario [OK return code](../features/return_code.md) pass  


## [check_no_output.md](../features/check_no_output.md)  

  ### Feature: Check that there is no output  

  - [X] scenario [silent operation](../features/check_no_output.md) pass  

  - [X] scenario [silent operation expected, but there is an output](../features/check_no_output.md) pass  


## [deleting_created_files.md](../features/deleting_created_files.md)  

  ### Feature: tmp files and directories deletion  

  - [X] scenario [run without --cleanup](../features/deleting_created_files.md) pass  

  - [X] scenario [run with --cleanup](../features/deleting_created_files.md) pass  


## [file_test_and_creation.md](../features/file_test_and_creation.md)  

  ### Feature: testing the existence of a file  

  - [X] scenario [a required file does not exist](../features/file_test_and_creation.md) pass  

  - [X] scenario [the required file is created](../features/file_test_and_creation.md) pass  

  - [X] scenario ["Given there is no", when there actually is, should erase the file](../features/file_test_and_creation.md) pass  


## [stdout.md](../features/stdout.md)  

  ### Feature: checking a message line on stdout  

  - [X] scenario [asking for uut version](../features/stdout.md) pass  


## [empty_scenarios.md](../features/empty_scenarios.md)  

  - [X] scenario [No step test](../features/empty_scenarios.md) pass  

  - [X] scenario [No scenario test](../features/empty_scenarios.md) pass  


## [successfully.md](../features/successfully.md)  

  ### Feature: The “successfully” shortcut  

  - [X] scenario [*when I successfully run* a command with successful run](../features/successfully.md) pass  

  - [X] scenario [*when I successfully run* a command with a wrong command line, returns an error status](../features/successfully.md) pass  

  - [X] scenario [*when I run* a command with a wrong command line](../features/successfully.md) pass  


## [step_marker.md](../features/step_marker.md)  

  ### Feature:   

  - [X] scenario [](../features/step_marker.md) pass  


-----------------------
- Failed     tests =  0
- Successful tests =  52
- Empty      tests =  0
