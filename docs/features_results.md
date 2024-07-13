
## [features/multiline.md](features/multiline.md)  

  - [X] scenario [asking for uut help](features/multiline.md) pass  

  - [X] scenario [causing an uut error with a long explanation](features/multiline.md) pass  

## [features/escaped_char.md](features/escaped_char.md)  

  - [X] scenario [Directory with space in the name](features/escaped_char.md) pass  

## [features/file_is_code_fence.md](features/file_is_code_fence.md)  

  - [X] scenario [file is as expected](features/file_is_code_fence.md) pass  

  - [X] scenario [file is not as expected](features/file_is_code_fence.md) pass  

## [features/output_equal_to_file.md](features/output_equal_to_file.md)  

  - [X] scenario [test `output is equal` keyword](features/output_equal_to_file.md) pass  

## [features/missing_title.md](features/missing_title.md)  

  - [X] scenario [Missing tittle in scenario, background and feature](features/missing_title.md) pass  

## [features/background.md](features/background.md)  

  - [X] scenario [lets erase what was created by previous background runs](features/background.md) pass  

  - [X] scenario [Two Background executed](features/background.md) pass  

  - [X] scenario [lets erase once more what was created by previous background runs](features/background.md) pass  

  - [X] scenario [only first background should apply](features/background.md) pass  

## [features/exec_dir.md](features/exec_dir.md)  

  - [X] scenario [Lets run `create_tree` in the current dir](features/exec_dir.md) pass  

  - [X] scenario [Lets run `create_tree` in ./dir2](features/exec_dir.md) pass  

## [features/stderr.md](features/stderr.md)  

  - [X] scenario [unknown option](features/stderr.md) pass  

## [features/contains_multiline.md](features/contains_multiline.md)  

  - [X] scenario [test the standard output](features/contains_multiline.md) pass  

*** NOK : And output contains `file config.ini does not contain` (features/contains_multiline.md:40:)  
Output:  
["Error : Unknown option or file ""contains_multiline.keep"""]    does not contain expected:  
["file config.ini does not contain"]  
  - [ ] scenario [should fail](features/contains_multiline.md) fails  

## [features/return_code_on_test_failure.md](features/return_code_on_test_failure.md)  

  - [X] scenario [return code on test success](features/return_code_on_test_failure.md) pass  

  - [X] scenario [return code when the test fail](features/return_code_on_test_failure.md) pass  

  - [X] scenario [return code when one fail and the other succeed](features/return_code_on_test_failure.md) pass  

## [features/contains_line.md](features/contains_line.md)  

  - [X] scenario [test on a string output](features/contains_line.md) pass  

  - [X] scenario [test on a multiline output](features/contains_line.md) pass  

  - [X] scenario [test on a file](features/contains_line.md) pass  

## [features/return_code.md](features/return_code.md)  

  - [X] scenario [NOK return code](features/return_code.md) pass  

  - [X] scenario [OK return code](features/return_code.md) pass  

## [features/deleting_created_files.md](features/deleting_created_files.md)  

  - [X] scenario [run without --cleanup](features/deleting_created_files.md) pass  

  - [X] scenario [run with --cleanup](features/deleting_created_files.md) pass  

## [features/file_test_and_creation.md](features/file_test_and_creation.md)  

  - [X] scenario [a required file does not exist](features/file_test_and_creation.md) pass  

  - [X] scenario [the required file is created](features/file_test_and_creation.md) pass  

  - [X] scenario ["Given there is no", when there actually is, should erase the file](features/file_test_and_creation.md) pass  

## [features/stdout.md](features/stdout.md)  

  - [X] scenario [asking for uut version](features/stdout.md) pass  

## [features/empty_scenarios.md](features/empty_scenarios.md)  

  - [X] scenario [No step test](features/empty_scenarios.md) pass  

  - [X] scenario [No scenario test](features/empty_scenarios.md) pass  

## [features/successfully.md](features/successfully.md)  

  - [X] scenario [*when I successfully run* a command with successful run](features/successfully.md) pass  

  - [X] scenario [*when I successfully run* a command with a wrong command line, returns an error status](features/successfully.md) pass  

  - [X] scenario [*when I run* a command with a wrong command line](features/successfully.md) pass  

------------------------------------------------
- Failed     tests =  1
- Successful tests =  34
- Empty      tests =  0
