
Running file "features/return_code_on_test_failure.md"  

*** NOK : Then I get no error   
features/return_code_on_test_failure.md:16:No error expected, but got one ( 1)  
  - [ ] scenario [return code on test success](features/return_code_on_test_failure.md) fails  

  - [X] scenario [return code when the test fail](features/return_code_on_test_failure.md) pass  

  - [X] scenario [return code when one fail and the other succeed](features/return_code_on_test_failure.md) pass  

Running file "features/contains_line.md"  

  - [X] scenario [test on a string output](features/contains_line.md) pass  

  - [X] scenario [test on a multiline output](features/contains_line.md) pass  

  - [ ] scenario [test on a file](features/contains_line.md) fails  

Running file "features/file_test_and_creation.md"  

  - [X] scenario [a required file does not exist](features/file_test_and_creation.md) pass  

  - [ ] scenario [the required file is created](features/file_test_and_creation.md) fails  

  - [X] scenario ["Given there is no", when there actually is, should erase the file](features/file_test_and_creation.md) pass  

------------------------------------------------
- Failed     tests =  3
- Successful tests =  6
- Empty      tests =  0
