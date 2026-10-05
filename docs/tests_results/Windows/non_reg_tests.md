
# Document: [New_non_Gerkhin_header_do_not_ends_the_current_Scenario.md](..\..\..\tests\non_reg_tests\New_non_Gerkhin_header_do_not_ends_the_current_Scenario.md)  
   ### Scenario: [Step analysis is interrupted when exiting the section ([Issue #37](https://github.com/LionelDraghi/bbt/issues/37))](..\..\..\tests\non_reg_tests\New_non_Gerkhin_header_do_not_ends_the_current_Scenario.md): 
   - OK : Given there is no `config.ini` file  
   - OK : Given the file `step_markers2.md`  
   - OK : When I successfully run `./bbt -c step_markers2.md`  
   - OK : Then the output contains `- [ ] scenario [1](step_markers2.md) is empty, nothing tested`  
   - [X] scenario   [Step analysis is interrupted when exiting the section ([Issue #37](https://github.com/LionelDraghi/bbt/issues/37))](..\..\..\tests\non_reg_tests\New_non_Gerkhin_header_do_not_ends_the_current_Scenario.md) pass  


# Document: [empty_output_vs_non_empty_expected.md](..\..\..\tests\non_reg_tests\empty_output_vs_non_empty_expected.md)  
   ### Scenario: [comparing an empty actual output with a non empty expected content shall fail normally, not raise an exception, and the scenario shall be counted as failed (bug present in bbt 0.4.2-dev, found on 2026-10-03 while migrating the ArchiCheck test suite)](..\..\..\tests\non_reg_tests\empty_output_vs_non_empty_expected.md): 
   - OK : Given the new file `empty_output_test.md`  
   - OK : When I run `./bbt --keep_going empty_output_test.md`  
   - OK : Then I get an error  
   - OK : And output do not contain `Exception`  
   - OK : And output contains `Output not equal to expected`  
   - OK : And output contains `**fails**`  
   - [X] scenario   [comparing an empty actual output with a non empty expected content shall fail normally, not raise an exception, and the scenario shall be counted as failed (bug present in bbt 0.4.2-dev, found on 2026-10-03 while migrating the ArchiCheck test suite)](..\..\..\tests\non_reg_tests\empty_output_vs_non_empty_expected.md) pass  


# Document: [exception_on_is_equal.md](..\..\..\tests\non_reg_tests\exception_on_is_equal.md)  
   ### Scenario: [test that `is equal to file` no more raise an exception when files are of different sizes, check Issue: #7](..\..\..\tests\non_reg_tests\exception_on_is_equal.md): 
   - OK : Given the file `tmp.1`  
   - OK : Given the file `tmp.2`  
   - OK : And the file `test_that_should_fail.md`  
   - OK : When I run `./bbt test_that_should_fail.md`  
   - OK : Then output contains `| Failed     |  1`  
   - OK : Then output do not contain `Exception`  
   - OK : And I get an error  
   - [X] scenario   [test that `is equal to file` no more raise an exception when files are of different sizes, check Issue: #7](..\..\..\tests\non_reg_tests\exception_on_is_equal.md) pass  


# Document: [extra_line.md](..\..\..\tests\non_reg_tests\extra_line.md)  
   ### Scenario: [](..\..\..\tests\non_reg_tests\extra_line.md): 
   - OK : Given the new file `simple.ads`  
   - OK : And the file `scen.md`  
   - OK : When I run `./bbt -em scen.md`  
   - OK : Then I get no error    
   - [X] scenario   [](..\..\..\tests\non_reg_tests\extra_line.md) pass  


# Document: [missing_code_block_then_new_scenario.md](..\..\..\tests\non_reg_tests\missing_code_block_then_new_scenario.md)  
   ### Scenario: [a step with a missing code block followed by a new Scenario header shall not crash the analysis (bug present in bbt 0.4.2-dev, found on 2026-10-03 while migrating the ArchiCheck test suite)](..\..\..\tests\non_reg_tests\missing_code_block_then_new_scenario.md): 
   - OK : Given the new file `missing_code_block_test.md`  
   - OK : When I run `./bbt explain missing_code_block_test.md`  
   - OK : Then output do not contain `Exception`  
   - OK : And output contains `Missing Code Block expected line`  
   - OK : And output contains `following`  
   - OK : When I run `./bbt --keep_going missing_code_block_test.md`  
   - OK : Then I get an error  
   - OK : And output do not contain `Exception`  
   - [X] scenario   [a step with a missing code block followed by a new Scenario header shall not crash the analysis (bug present in bbt 0.4.2-dev, found on 2026-10-03 while migrating the ArchiCheck test suite)](..\..\..\tests\non_reg_tests\missing_code_block_then_new_scenario.md) pass  


## Summary : **Success**, 5 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 5     |
| Empty      | 0     |
| Not Run    | 0     |

