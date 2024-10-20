
## [multiline.md](../docs/features/multiline.md)  

  ### Feature: checking an expected multiline output  

  - [X] scenario [asking for uut help](../docs/features/multiline.md) pass  

  - [X] scenario [causing an uut error with a long explanation](../docs/features/multiline.md) pass  


## [find_scenarios.md](../docs/features/find_scenarios.md)  

  ### Feature: multiples scenarios given in command line  

  - [X] scenario [no file or dir on the command line](../docs/features/find_scenarios.md) pass  

  - [X] scenario [running all scenarios in dir1](../docs/features/find_scenarios.md) pass  

  - [X] scenario [running multiple scenarios given on command line](../docs/features/find_scenarios.md) pass  

  - [X] scenario [running scenarios in a tree thanks to `-r`](../docs/features/find_scenarios.md) pass  

  - [X] scenario [error msg when trying to run scenarios, but none found in given directories](../docs/features/find_scenarios.md) pass  

  - [X] scenario [empty list file on -lf if there is no scenario in given directories](../docs/features/find_scenarios.md) pass  


## [escaped_char.md](../docs/features/escaped_char.md)  

  ### Feature: Space in names  

  - [X] scenario [Directory with space in the name](../docs/features/escaped_char.md) pass  


## [file_is_code_fence.md](../docs/features/file_is_code_fence.md)  

  ### Feature: testing the "file is" feature  

  - [X] scenario [file is as expected](../docs/features/file_is_code_fence.md) pass  

  - [X] scenario [file is not as expected](../docs/features/file_is_code_fence.md) pass  


## [output_equal_to_file.md](../docs/features/output_equal_to_file.md)  

  ### Feature: output is equal to a file  

  - [X] scenario [test `output is equal` keyword](../docs/features/output_equal_to_file.md) pass  


## [example_keyword.md](../docs/features/example_keyword.md)  

  ### Feature: The keyword `Example` is a synonym of the keyword `Scenario`.  

  - [X] scenario [NOK return code](../docs/features/example_keyword.md) pass  

  - [X] scenario [NOK return code](../docs/features/example_keyword.md) pass  


## [missing_title.md](../docs/features/missing_title.md)  

  - [X] scenario [Missing tittle in scenario, background and feature](../docs/features/missing_title.md) pass  


## [background.md](../docs/features/background.md)  

  ### Feature: Feature 1  

  - [X] scenario [lets erase what was created by previous background runs](../docs/features/background.md) pass  

  - [X] scenario [Two Background executed](../docs/features/background.md) pass  

  ### Feature: Feature 2  

  - [X] scenario [lets erase once more what was created by previous background runs](../docs/features/background.md) pass  

  - [X] scenario [only first background should apply](../docs/features/background.md) pass  


## [exec_dir.md](../docs/features/exec_dir.md)  

  ### Feature: **Exec Dir**  

  - [X] scenario [Lets run `create_tree` in the current dir](../docs/features/exec_dir.md) pass  

  - [X] scenario [Lets run `create_tree` in ./dir2](../docs/features/exec_dir.md) pass  


## [stderr.md](../docs/features/stderr.md)  

  ### Feature: stderr test  

  - [X] scenario [unknown option](../docs/features/stderr.md) pass  


## [unordered.md](../docs/features/unordered.md)  

  ### Feature: when the modifyer `unordered` is given after `get`, order of line is ignored  

  - [X] scenario ["I get" without the modifyer](../docs/features/unordered.md) pass  

  - [X] scenario [same "I get" with the modifyer](../docs/features/unordered.md) pass  


## [contains_multiline.md](../docs/features/contains_multiline.md)  

  ### Feature: "contains" a string feature  

  - [X] scenario [test the standard output](../docs/features/contains_multiline.md) pass  

  - [X] scenario [should fail](../docs/features/contains_multiline.md) pass  


## [return_code_on_test_failure.md](../docs/features/return_code_on_test_failure.md)  

  ### Feature: return code on test failure  

  - [X] scenario [return code on test success](../docs/features/return_code_on_test_failure.md) pass  

  - [X] scenario [return code when the test fail](../docs/features/return_code_on_test_failure.md) pass  

  - [X] scenario [return code when one fail and the other succeed](../docs/features/return_code_on_test_failure.md) pass  


## [file_is_string.md](../docs/features/file_is_string.md)  

  ### Feature: "file is" followed by a code span (a string)  

  - [X] scenario [test on a single line file](../docs/features/file_is_string.md) pass  

  - [X] scenario [adding a second line to the file, so the same test should fail](../docs/features/file_is_string.md) pass  


## [contains_line.md](../docs/features/contains_line.md)  

  ### Feature: testing that a file contains a line  

  - [X] scenario [test on a string output](../docs/features/contains_line.md) pass  

../docs/features/contains_line.md:15: Error : Unrecognized step "Then the error output contains `unknown option -qsd`"
  - [ ] scenario [test on the error output](../docs/features/contains_line.md) fails  

  - [X] scenario [test on a multiline output](../docs/features/contains_line.md) pass  

  - [X] scenario [test on a file](../docs/features/contains_line.md) pass  


## [return_code.md](../docs/features/return_code.md)  

  ### Feature: return code test  

  - [X] scenario [NOK return code](../docs/features/return_code.md) pass  

  - [X] scenario [OK return code](../docs/features/return_code.md) pass  


## [check_no_output.md](../docs/features/check_no_output.md)  

  ### Feature: Check that there is no output  

  - [X] scenario [silent operation](../docs/features/check_no_output.md) pass  

  - [X] scenario [silent operation expected, but there is an output](../docs/features/check_no_output.md) pass  


## [deleting_created_files.md](../docs/features/deleting_created_files.md)  

  ### Feature: tmp files and directories deletion  

  - [X] scenario [run without --cleanup](../docs/features/deleting_created_files.md) pass  

  - [X] scenario [run with --cleanup](../docs/features/deleting_created_files.md) pass  


## [file_test_and_creation.md](../docs/features/file_test_and_creation.md)  

  ### Feature: testing the existence of a file  

  - [X] scenario [a required file does not exist](../docs/features/file_test_and_creation.md) pass  

  - [X] scenario [the required file is created](../docs/features/file_test_and_creation.md) pass  

  - [X] scenario ["Given there is no", when there actually is, should erase the file](../docs/features/file_test_and_creation.md) pass  


## [stdout.md](../docs/features/stdout.md)  

  ### Feature: checking a message line on stdout  

  - [X] scenario [asking for uut version](../docs/features/stdout.md) pass  


## [empty_scenarios.md](../docs/features/empty_scenarios.md)  

  - [X] scenario [No step test](../docs/features/empty_scenarios.md) pass  

  - [X] scenario [No scenario test](../docs/features/empty_scenarios.md) pass  


## [successfully.md](../docs/features/successfully.md)  

  ### Feature: The “successfully” shortcut  

  - [X] scenario [*when I successfully run* a command with successful run](../docs/features/successfully.md) pass  

  - [X] scenario [*when I successfully run* a command with a wrong command line, returns an error status](../docs/features/successfully.md) pass  

  - [X] scenario [*when I run* a command with a wrong command line](../docs/features/successfully.md) pass  


-----------------------
- Failed     tests =  1
- Successful tests =  49
- Empty      tests =  0
