
# Document: [exception_on_is_equal.md](../../../tests/non_reg_tests/exception_on_is_equal.md)  
   ### Scenario: [test that `is equal to file` no more raise an exception when files are of different sizes, check Issue: #7](../../../tests/non_reg_tests/exception_on_is_equal.md): 
   - OK : Given the file `tmp.1`  
   - OK : Given the file `tmp.2`  
   - OK : And the file `test_that_should_fail.md`  
   - OK : When I run `./bbt test_that_should_fail.md`  
   - OK : Then output contains `| Failed     |  1`  
   - OK : Then output do not contain `Exception`  
   - OK : And I get an error  
   - [X] scenario   [test that `is equal to file` no more raise an exception when files are of different sizes, check Issue: #7](../../../tests/non_reg_tests/exception_on_is_equal.md) pass  


# Document: [extra_line.md](../../../tests/non_reg_tests/extra_line.md)  
   ### Scenario: [](../../../tests/non_reg_tests/extra_line.md): 
   - OK : Given the new file `simple.ads`  
   - OK : And the file `scen.md`  
   - OK : When I run `./bbt -em scen.md`  
   - OK : Then I get no error    
   - [X] scenario   [](../../../tests/non_reg_tests/extra_line.md) pass  


## Summary : **Success**, 2 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 2     |
| Empty      | 0     |
| Not Run    | 0     |

