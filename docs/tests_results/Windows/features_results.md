
# Document: [A005_Given.md](..\..\features\A005_Given.md)  
  ## Feature:   
   ### Scenario: [Checking that there is no file or dir](..\..\features\A005_Given.md): 
   - OK : Given there is no dir `dir1`  
   - OK : Given there is no file `file1`  
   - OK : Then there is no dir `dir1`  
   - OK : Then there is no file `file1`  
   - [X] scenario   [Checking that there is no file or dir](..\..\features\A005_Given.md) pass  

   ### Scenario: [Checking that there is some dir](..\..\features\A005_Given.md): 
   - OK : Given the directory `dir2`  
   - OK : Then there is a dir `dir2`  
   - [X] scenario   [Checking that there is some dir](..\..\features\A005_Given.md) pass  

   ### Scenario: [Checking that there is a file with some content](..\..\features\A005_Given.md): 
   - OK : Given there is no file `file3`  
   - OK : Given the file `file3`  
   - OK : Then file `file3` is `Hello World!`  
   - [X] scenario   [Checking that there is a file with some content](..\..\features\A005_Given.md) pass  

   ### Scenario: [Creating a file with some content](..\..\features\A005_Given.md): 
   - OK : Given the file `file4` containing `alpha`  
   - OK : Given the file `file5` containing   
   - OK : Then file `file4` is `alpha`  
   - OK : Then file `file5` is  
   - [X] scenario   [Creating a file with some content](..\..\features\A005_Given.md) pass  


# Document: [A006_Given_Executable_File.md](..\..\features\A006_Given_Executable_File.md)  
  ## Feature: running scripts (Unix_Only)  

# Document: [A010_Then_Contains_Code_Span.md](..\..\features\A010_Then_Contains_Code_Span.md)  
  ## Feature: testing that a file contains a string  
   ### Scenario: [test on a string output](..\..\features\A010_Then_Contains_Code_Span.md): 
   - OK : When I run `./sut -v`  
   - OK : Then the output should contain `version`  
   - [X] scenario   [test on a string output](..\..\features\A010_Then_Contains_Code_Span.md) pass  

   ### Scenario: [test on a multiline output](..\..\features\A010_Then_Contains_Code_Span.md): 
   - OK : When I run `./sut -h`  
   - OK : Then the output should contains `--version`  
   - [X] scenario   [test on a multiline output](..\..\features\A010_Then_Contains_Code_Span.md) pass  

   ### Scenario: [test on a file](..\..\features\A010_Then_Contains_Code_Span.md): 
   - OK : Given the new file `config.ini`   
   - OK : Then `config.ini` contains `mode=silent`  
   - [X] scenario   [test on a file](..\..\features\A010_Then_Contains_Code_Span.md) pass  


# Document: [A020_Then_Contains_Code_Block.md](..\..\features\A020_Then_Contains_Code_Block.md)  
  ## Feature: "contains" a string feature  
   ### Scenario: [test the standard output](..\..\features\A020_Then_Contains_Code_Block.md): 
   - OK : Given the new `config.ini` file  
   - OK : And the `contains_multiline.md` file  
   - OK : Then `config.ini` contains  
   - OK : And `config.ini` contains   
   - OK : And `config.ini` contains   
   - OK : And `config.ini` contains   
   - [X] scenario   [test the standard output](..\..\features\A020_Then_Contains_Code_Block.md) pass  

   ### Scenario: [compare with an existing file](..\..\features\A020_Then_Contains_Code_Block.md): 
   - OK : When I run `./bbt contains_multiline.md`  
   - OK : Then I get an error  
   - OK : And output should contain    
   - [X] scenario   [compare with an existing file](..\..\features\A020_Then_Contains_Code_Block.md) pass  


# Document: [A030_Then_Does_Not_Contain.md](..\..\features\A030_Then_Does_Not_Contain.md)  
  ## Feature: checking that some string is not present in output or file  
   ### Background: [](..\..\features\A030_Then_Does_Not_Contain.md): 
   - OK : Given file `flowers.txt`  
   - [X] background [](..\..\features\A030_Then_Does_Not_Contain.md) pass  

   ### Scenario: [Successful checks](..\..\features\A030_Then_Does_Not_Contain.md): 
   - OK : When I run `./sut read flowers.txt`  
   - OK : Then output contains `Rose`  
   - OK : But output doesn't contain `Cactus`  
   - OK : Then file `flowers.txt` contains `Tulip`  
   - OK : But file `flowers.txt` do not contain `Eucalyptus`  
   - [X] scenario   [Successful checks](..\..\features\A030_Then_Does_Not_Contain.md) pass  

   ### Background: [](..\..\features\A030_Then_Does_Not_Contain.md): 
   - OK : Given file `flowers.txt`  
   - [X] background [](..\..\features\A030_Then_Does_Not_Contain.md) pass  

   ### Scenario: [Failed "output doesn't contain"](..\..\features\A030_Then_Does_Not_Contain.md): 
   - OK : Given the file `failed_doesnt_1.md`  
   - OK : When I run `./bbt failed_doesnt_1.md`  
   - OK : Then I get an error  
   - OK : And output contains   
   - OK : And output contains   
   - [X] scenario   [Failed "output doesn't contain"](..\..\features\A030_Then_Does_Not_Contain.md) pass  

   ### Background: [](..\..\features\A030_Then_Does_Not_Contain.md): 
   - OK : Given file `flowers.txt`  
   - [X] background [](..\..\features\A030_Then_Does_Not_Contain.md) pass  

   ### Scenario: [Failed "file doesn't contain"](..\..\features\A030_Then_Does_Not_Contain.md): 
   - OK : Given the file `failed_doesnt_2.md`  
   - OK : When I run `./bbt failed_doesnt_2.md`  
   - OK : Then I get an error  
   - OK : And output contains   
   - [X] scenario   [Failed "file doesn't contain"](..\..\features\A030_Then_Does_Not_Contain.md) pass  


# Document: [A040_Example_Keyword.md](..\..\features\A040_Example_Keyword.md)  
  ## Feature: The keyword `Example` is a synonym of the keyword `Scenario`.  
   ### Scenario: [NOK return code](..\..\features\A040_Example_Keyword.md): 
   - OK : When I run `./sut -qsdqsd`  
   - OK : Then I get an error  
   - [X] scenario   [NOK return code](..\..\features\A040_Example_Keyword.md) pass  

   ### Scenario: [NOK return code](..\..\features\A040_Example_Keyword.md): 
   - OK : When I run `./sut -qsdqsd`  
   - OK : Then I get an error  
   - [X] scenario   [NOK return code](..\..\features\A040_Example_Keyword.md) pass  


# Document: [A050_Then_File_Is_Code_Block.md](..\..\features\A050_Then_File_Is_Code_Block.md)  
  ## Feature: testing the "file is" feature  
   ### Scenario: [file is as expected](..\..\features\A050_Then_File_Is_Code_Block.md): 
   - OK : Given the `blaise_cendrars.txt` file  
   - OK : Then `blaise_cendrars.txt` is  
   - [X] scenario   [file is as expected](..\..\features\A050_Then_File_Is_Code_Block.md) pass  

   ### Scenario: [file is not as expected](..\..\features\A050_Then_File_Is_Code_Block.md): 
   - OK : Given the `file_is_code_fence.input` file  
   - OK : When running `./bbt file_is_code_fence.input`  
   - OK : Then I get an error  
   - [X] scenario   [file is not as expected](..\..\features\A050_Then_File_Is_Code_Block.md) pass  


# Document: [A060_Then_File_Equal_Code_Span.md](..\..\features\A060_Then_File_Equal_Code_Span.md)  
  ## Feature: "file is" followed by a code span (a string)  
   ### Scenario: [test on a single line file](..\..\features\A060_Then_File_Equal_Code_Span.md): 
   - OK : When I run `./sut create config.ini`  
   - OK : When I run `./sut append mode=silent config.ini`  
   - OK : Then file `config.ini` is equal to `mode=silent`  
   - OK : Then file `config.ini` is          `mode=silent`  
   - [X] scenario   [test on a single line file](..\..\features\A060_Then_File_Equal_Code_Span.md) pass  

   ### Scenario: [adding a second line to the file, so the same test should fail](..\..\features\A060_Then_File_Equal_Code_Span.md): 
   - OK : Given the new `tmp.md` file  
   - OK : When I run `./sut create config.ini`  
   - OK : When I run `./sut append mode=silent config.ini`  
   - OK : When I run `./sut append recurse=false config.ini`  
   - OK : When I run `./bbt tmp.md`  
   - OK : then I get an error  
   - [X] scenario   [adding a second line to the file, so the same test should fail](..\..\features\A060_Then_File_Equal_Code_Span.md) pass  


# Document: [A065_Then_File_Equal_File.md](..\..\features\A065_Then_File_Equal_File.md)  
  ## Feature: file is equal to a file  
   ### Scenario: [test `is equal to file` form](..\..\features\A065_Then_File_Equal_File.md): 
   - OK : Given the file `tmp.1`  
   - OK : And the file `tmp.2`  
   - OK : Then `tmp.1` is equal to file `tmp.2`  
   - [X] scenario   [test `is equal to file` form](..\..\features\A065_Then_File_Equal_File.md) pass  

   ### Scenario: [test `is equal to file` when files are *not* equal](..\..\features\A065_Then_File_Equal_File.md): 
   - OK : Given the file `tmp.3`  
   - OK : And the file `test_that_should_fail.md`  
   - OK : When I run `./bbt test_that_should_fail.md`  
   - OK : Then output contains `| Failed     |  1 `  
   - OK : And I get an error  
   - [X] scenario   [test `is equal to file` when files are *not* equal](..\..\features\A065_Then_File_Equal_File.md) pass  

   ### Scenario: [test the negative form `is not equal to file`](..\..\features\A065_Then_File_Equal_File.md): 
   - OK : Given the file `tmp.4`  
   - OK : Then `tmp.1` is not equal to file `tmp.4`  
   - OK : Then `tmp.4` is not equal to file `tmp.1`  
   - [X] scenario   [test the negative form `is not equal to file`](..\..\features\A065_Then_File_Equal_File.md) pass  


# Document: [A070_Then_Get_Code_Span.md](..\..\features\A070_Then_Get_Code_Span.md)  
  ## Feature: checking an expected multiline output  
   ### Scenario: [asking for sut help](..\..\features\A070_Then_Get_Code_Span.md): 
   - OK : When I run `./sut -h`  
   - OK : Then I get   
   - [X] scenario   [asking for sut help](..\..\features\A070_Then_Get_Code_Span.md) pass  

   ### Scenario: [causing an sut error with a long explanation](..\..\features\A070_Then_Get_Code_Span.md): 
   - OK : When I run `./sut -e append `  
   - OK : Then I get on stderr  
   - [X] scenario   [causing an sut error with a long explanation](..\..\features\A070_Then_Get_Code_Span.md) pass  


# Document: [A080_Then_Output_Equal_To_File.md](..\..\features\A080_Then_Output_Equal_To_File.md)  
  ## Feature: output is equal to a file  
   ### Scenario: [test `output is equal` keyword](..\..\features\A080_Then_Output_Equal_To_File.md): 
   - OK : Given the file `help_message.txt`  
   - OK : When I run `./sut -h`  
   - OK : Then output is equal to file `help_message.txt`  
   - [X] scenario   [test `output is equal` keyword](..\..\features\A080_Then_Output_Equal_To_File.md) pass  


# Document: [A090_Then_Get_Error.md](..\..\features\A090_Then_Get_Error.md)  
  ## Feature: return code test  
   ### Scenario: [NOK return code](..\..\features\A090_Then_Get_Error.md): 
   - OK : When I run `./sut -qsdqsd`  
   - OK : Then I get an error  
   - [X] scenario   [NOK return code](..\..\features\A090_Then_Get_Error.md) pass  

   ### Scenario: [OK return code](..\..\features\A090_Then_Get_Error.md): 
   - OK : When I run `./sut -v`  
   - OK : Then I get no error  
   - [X] scenario   [OK return code](..\..\features\A090_Then_Get_Error.md) pass  


# Document: [A100_Then_Get_Stderr.md](..\..\features\A100_Then_Get_Stderr.md)  
  ## Feature: stderr test  
   ### Scenario: [unknown option](..\..\features\A100_Then_Get_Stderr.md): 
   - OK : When I run `./sut -qsd`  
   - OK : Then I get `unknown option -qsd` on stderr  
   - [X] scenario   [unknown option](..\..\features\A100_Then_Get_Stderr.md) pass  


# Document: [A110_Then_No_Output.md](..\..\features\A110_Then_No_Output.md)  
  ## Feature: Check that there is no output  
   ### Scenario: [silent operation](..\..\features\A110_Then_No_Output.md): 
   - OK : Given the new file `file.txt` containing `text1`  
   - OK : When I run `./sut append text file.txt`  
   - OK : Then there is no output  
   - OK : And I get no error  
   - [X] scenario   [silent operation](..\..\features\A110_Then_No_Output.md) pass  

   ### Scenario: [silent operation expected, but there is an output](..\..\features\A110_Then_No_Output.md): 
   - OK : Given the new `file.txt` file containing `text2`  
   - OK : Given the new file `no_output.input`  
   - OK : When I run `./bbt -c no_output.input`  
   - OK : Then I get an error  
   - OK : And output contains `**NOK** : Then there is no output`  
   - OK : And output contains `output not null`  
   - [X] scenario   [silent operation expected, but there is an output](..\..\features\A110_Then_No_Output.md) pass  


# Document: [A120_Then_Get_String.md](..\..\features\A120_Then_Get_String.md)  
  ## Feature: checking a message line on stdout  
   ### Scenario: [asking for sut version](..\..\features\A120_Then_Get_String.md): 
   - OK : When I run `./sut -v`  
   - OK : Then I get `sut version 1.0`  
   - [X] scenario   [asking for sut version](..\..\features\A120_Then_Get_String.md) pass  


# Document: [A130_Successfully_Keyword.md](..\..\features\A130_Successfully_Keyword.md)  
  ## Feature: The “successfully” shortcut  
   ### Scenario: [*when I successfully run* a command with successful run](..\..\features\A130_Successfully_Keyword.md): 
   - OK : When I successfully run `./sut --version`  
   - OK : Then I get `sut version 1.0`  
   - [X] scenario   [*when I successfully run* a command with successful run](..\..\features\A130_Successfully_Keyword.md) pass  

   ### Scenario: [*when I successfully run* a command with a wrong command line, returns an error status](..\..\features\A130_Successfully_Keyword.md): 
   - OK : Given the `vza.input` file  
   - OK : When I run `./bbt vza.input`  
   - OK : Then I get an error  
   - [X] scenario   [*when I successfully run* a command with a wrong command line, returns an error status](..\..\features\A130_Successfully_Keyword.md) pass  

   ### Scenario: [*when I run* a command with a wrong command line](..\..\features\A130_Successfully_Keyword.md): 
   - OK : When I run `./sut -vza`  
   - OK : Then I get `unknown option -vza`  
   - [X] scenario   [*when I run* a command with a wrong command line](..\..\features\A130_Successfully_Keyword.md) pass  


# Document: [A140_Unordered_Keyword.md](..\..\features\A140_Unordered_Keyword.md)  
  ## Feature: when the modifier `unordered` is given after `get`, order of line is ignored  
   ### Background: [](..\..\features\A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](..\..\features\A140_Unordered_Keyword.md) pass  

   ### Scenario: ["I get" without the modifier](..\..\features\A140_Unordered_Keyword.md): 
   - OK : Given the file `scenario1.md`  
   - OK : When I run `./bbt scenario1.md`  
   - OK : Then I get an error  
   - [X] scenario   ["I get" without the modifier](..\..\features\A140_Unordered_Keyword.md) pass  

   ### Background: [](..\..\features\A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](..\..\features\A140_Unordered_Keyword.md) pass  

   ### Scenario: [same "I get" with the modifier](..\..\features\A140_Unordered_Keyword.md): 
   - OK : When I run `./sut read flowers1.txt`  
   - OK : Then I get file (unordered) `flowers2.txt`  
   - [X] scenario   [same "I get" with the modifier](..\..\features\A140_Unordered_Keyword.md) pass  

   ### Background: [](..\..\features\A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](..\..\features\A140_Unordered_Keyword.md) pass  

   ### Scenario: ["contains" without the modifier](..\..\features\A140_Unordered_Keyword.md): 
   - OK : Given the file `scenario1.md`  
   - OK : When I run `./bbt scenario1.md`  
   - OK : Then I get an error  
   - [X] scenario   ["contains" without the modifier](..\..\features\A140_Unordered_Keyword.md) pass  

   ### Background: [](..\..\features\A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](..\..\features\A140_Unordered_Keyword.md) pass  

   ### Scenario: [same "contains" but with the "unordered" modifier](..\..\features\A140_Unordered_Keyword.md): 
   - OK : When I run `./sut read flowers1.txt`  
   - OK : Then output contains file `flowers3.txt` (unordered)  
   - [X] scenario   [same "contains" but with the "unordered" modifier](..\..\features\A140_Unordered_Keyword.md) pass  


# Document: [A150_Background_Keyword.md](..\..\features\A150_Background_Keyword.md)  
  ## Feature: Feature 1  
   ### Background: [Background1](..\..\features\A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](..\..\features\A150_Background_Keyword.md) pass  

   ### Background: [Background2 in feature 1](..\..\features\A150_Background_Keyword.md): 
   - OK : Given the file `dir1/file2`  
   - [X] background [Background2 in feature 1](..\..\features\A150_Background_Keyword.md) pass  

   ### Scenario: [lets erase what was created by previous background runs](..\..\features\A150_Background_Keyword.md): 
   - OK : Given there is no `dir1` dir  
   - [X] scenario   [lets erase what was created by previous background runs](..\..\features\A150_Background_Keyword.md) pass  

   ### Background: [Background1](..\..\features\A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](..\..\features\A150_Background_Keyword.md) pass  

   ### Background: [Background2 in feature 1](..\..\features\A150_Background_Keyword.md): 
   - OK : Given the file `dir1/file2`  
   - [X] background [Background2 in feature 1](..\..\features\A150_Background_Keyword.md) pass  

   ### Scenario: [Two Background executed](..\..\features\A150_Background_Keyword.md): 
   - OK : then there is a `dir1/file1` file  
   - OK : then there is a `dir1/file2` file  
   - [X] scenario   [Two Background executed](..\..\features\A150_Background_Keyword.md) pass  

  ## Feature: Feature 2  
   ### Background: [Background1](..\..\features\A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](..\..\features\A150_Background_Keyword.md) pass  

   ### Scenario: [lets erase once more what was created by previous background runs](..\..\features\A150_Background_Keyword.md): 
   - OK : Given there is no `dir1` dir  
   - [X] scenario   [lets erase once more what was created by previous background runs](..\..\features\A150_Background_Keyword.md) pass  

   ### Background: [Background1](..\..\features\A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](..\..\features\A150_Background_Keyword.md) pass  

   ### Scenario: [only first background should apply](..\..\features\A150_Background_Keyword.md): 
   - OK : then there is a `dir1/file1` file  
   - OK : then there is no `dir1/file2` file  
   - [X] scenario   [only first background should apply](..\..\features\A150_Background_Keyword.md) pass  


# Document: [A160_Ignoring_Blank_Lines.md](..\..\features\A160_Ignoring_Blank_Lines.md)  
  ## Feature: Ignoring blank lines  
   ### Background: [](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](..\..\features\A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [some obvious tests](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : Then file `le_Cid_1.txt` is equal to file `le_Cid_1.txt`  
   - OK : Then file `le_Cid_1.txt` contains file `le_Cid_1.txt`  
   - [X] scenario   [some obvious tests](..\..\features\A160_Ignoring_Blank_Lines.md) pass  

   ### Background: [](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](..\..\features\A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [default behavior, non sensible to blank lines](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : when I run `./bbt is.md`  
   - OK : then there is no error   
   - OK : when I run `./bbt contains.md`  
   - OK : then there is no error   
   - [X] scenario   [default behavior, non sensible to blank lines](..\..\features\A160_Ignoring_Blank_Lines.md) pass  

   ### Background: [](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](..\..\features\A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [with --exact_match, sensible to blank lines](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : when I run `./bbt -em is.md`  
   - OK : then there is an error   
   - OK : and output contains `Error : le_Cid_1.txt not equal to expected:`  
   - OK : when I run `./bbt -em contains.md`  
   - OK : then there is an error   
   - OK : and output contains `Error : le_Cid_1.txt does not contain expected:`  
   - [X] scenario   [with --exact_match, sensible to blank lines](..\..\features\A160_Ignoring_Blank_Lines.md) pass  

   ### Background: [](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](..\..\features\A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [with --exact_match and --ignore_blank_lines, non sensible to blank lines](..\..\features\A160_Ignoring_Blank_Lines.md): 
   - OK : when I run `./bbt -em -ibl is.md`  
   - OK : then there is no error   
   - OK : when I run `./bbt --exact_match --ignore_blank_lines contains.md`  
   - OK : then there is no error   
   - [X] scenario   [with --exact_match and --ignore_blank_lines, non sensible to blank lines](..\..\features\A160_Ignoring_Blank_Lines.md) pass  


# Document: [A170_File_vs_File_Name.md](..\..\features\A170_File_vs_File_Name.md)  
  ## Feature: expected content in a file  
   ### Scenario: ["Then I get file" form](..\..\features\A170_File_vs_File_Name.md): 
   - OK : Given the file `list_1.txt`  
   - OK : Given the file `list_2.txt`  
   - OK : When I run `./sut read list_1.txt`  
   - OK : Then I get file `list_1.txt`  
   - [X] scenario   ["Then I get file" form](..\..\features\A170_File_vs_File_Name.md) pass  

   ### Scenario: ["Then output is | contains | does not contain file" form](..\..\features\A170_File_vs_File_Name.md): 
   - OK : When I run `./sut read list_1.txt`  
   - OK : Then output is the `list_1.txt` file  
   - OK : Then output contains file `list_1.txt`  
   - OK : Then output does not contain file `list_2.txt`  
   - [X] scenario   ["Then output is | contains | does not contain file" form](..\..\features\A170_File_vs_File_Name.md) pass  

   ### Scenario: ["Then file is | contains | does not contain file" form](..\..\features\A170_File_vs_File_Name.md): 
   - OK : Then `list_2.txt` contains file `list_1.txt`  
   - OK : Then `list_1.txt` does not contain file `list_2.txt`  
   - [X] scenario   ["Then file is | contains | does not contain file" form](..\..\features\A170_File_vs_File_Name.md) pass  


# Document: [A190_Run.md](..\..\features\A190_Run.md)  
  ## Feature:   
   ### Scenario: [the command relative path is given](..\..\features\A190_Run.md): 
   - OK : when i run `./bbt lf`  
   - OK : then I get no error  
   - OK : and  I get no output  
   - [X] scenario   [the command relative path is given](..\..\features\A190_Run.md) pass  

   ### Scenario: [the command is in the PATH](..\..\features\A190_Run.md): 
   - OK : when i run `git --version`  
   - OK : then I get no error  
   - OK : and  output contains `git version`  
   - [X] scenario   [the command is in the PATH](..\..\features\A190_Run.md) pass  

   ### Scenario: [command not found](..\..\features\A190_Run.md): 
   - OK : Given the `cmd_not_found.md` file   
   - OK : When I run `./bbt -c cmd_not_found.md`  
   - OK : Then I get an error  
   - OK : And  the output contains `cmd_not_found.md:2: Error : xyzabc not found`  
   - [X] scenario   [command not found](..\..\features\A190_Run.md) pass  


# Document: [A195_Run_Or.md](..\..\features\A195_Run_Or.md)  
  ## Feature: “when I run X or Y” test  
   ### Scenario: [normal use case](..\..\features\A195_Run_Or.md): 
   - OK : Given the new file `simple_or.md`   
   - OK : When I run `./bbt simple_or.md`  
   - OK : Then I get no error  
   - **NOK** : And the output contains (..\docs\features\a195_run_or.md:15:)  
..\docs\features\a195_run_or.md:15: Error: Output:  
~~~
  
# Document: [simple_or.md](simple_or.md)    
   ### Scenario: [A 1/2](simple_or.md):   
 Exception : EXCEPTION_ACCESS_VIOLATIONLoad address: 0x7ff600970000  
[C:\Users\Lionel\Prj\bbt\tests\bbt.exe]  
0x7ff600b70ec2 __gnat_personality_seh0 at raise-gcc.c:1826  
[C:\WINDOWS\SYSTEM32\ntdll.dll]  
0x7ffe7182623d  
0x7ffe716d2395  
0x7ffe71825b7c  
[C:\Users\Lionel\Prj\bbt\tests\bbt.exe]  
0x7ff600b3c48e ada__tags__object_specific_dataIP at ???  
0x7ff600a074c7 Bbt.Model.Documents.Parent_Doc at bbt-model-documents.adb:43  
0x7ff600b06b50 Bbt.Tests.Runner.Run_Step at bbt-tests-runner.adb:76  
0x7ff600b0a429 Bbt.Tests.Runner.Run_Scenario at bbt-tests-runner.adb:244  
0x7ff600b0c7f2 Bbt.Tests.Runner.Run_Scenario_List at bbt-tests-runner.adb:341  
0x7ff600b0d07f Bbt.Tests.Runner.Run_Doc at bbt-tests-runner.adb:380  
0x7ff600b0e2fe Bbt.Tests.Runner.Run_All at bbt-tests-runner.adb:438  
0x7ff6009724cd Bbt.Main at bbt-main.adb:152  
0x7ff60097386e Main at b__bbt-main.adb:588  
0x7ff600971339 __tmainCRTStartup at ???  
0x7ff600971145 mainCRTStartup at ???  
[C:\WINDOWS\System32\KERNEL32.DLL]  
0x7ffe70ade8d5  
[C:\WINDOWS\SYSTEM32\ntdll.dll]  
0x7ffe7174c3fa  
  
  
## Summary : **Success**, 0 scenarios OK, 1 empty scenarios
~~~
does not contain expected:  
~~~
   ### Scenario: [A 1/2](simple_or.md):   
   - OK : When I run `./sut -v` whatever comment here    
   - [X] scenario   [A 1/2](simple_or.md) pass    
   ### Scenario: [A 2/2](simple_or.md):   
   - OK : When I run `./sut --version` whatever comment here    
   - [X] scenario   [A 2/2](simple_or.md) pass  
~~~
  
   - [ ] scenario   [normal use case](..\..\features\A195_Run_Or.md) **fails**  

   ### Scenario: [The last command does not meet expectation (test should fail)](..\..\features\A195_Run_Or.md): 
   - OK : Given the new file `erroneous_or.md`   
   - OK : When I run `./bbt -c erroneous_or.md`  
   - **NOK** : Then I get an error (..\docs\features\a195_run_or.md:34:)  
..\docs\features\a195_run_or.md:34: Error: Expected error code, got no error  
   - **NOK** : And the output contains (..\docs\features\a195_run_or.md:35:)  
..\docs\features\a195_run_or.md:35: Error: Output:  
~~~
  
# Document: [erroneous_or.md](erroneous_or.md)    
   ### Scenario: [B 1/3](erroneous_or.md):   
 Exception : EXCEPTION_ACCESS_VIOLATIONLoad address: 0x7ff600970000  
[C:\Users\Lionel\Prj\bbt\tests\bbt.exe]  
0x7ff600b70ec2 __gnat_personality_seh0 at raise-gcc.c:1826  
[C:\WINDOWS\SYSTEM32\ntdll.dll]  
0x7ffe7182623d  
0x7ffe716d2395  
0x7ffe71825b7c  
[C:\Users\Lionel\Prj\bbt\tests\bbt.exe]  
0x7ff600b3c48e ada__tags__object_specific_dataIP at ???  
0x7ff600a074c7 Bbt.Model.Documents.Parent_Doc at bbt-model-documents.adb:43  
0x7ff600b06b50 Bbt.Tests.Runner.Run_Step at bbt-tests-runner.adb:76  
0x7ff600b0a429 Bbt.Tests.Runner.Run_Scenario at bbt-tests-runner.adb:244  
0x7ff600b0c7f2 Bbt.Tests.Runner.Run_Scenario_List at bbt-tests-runner.adb:341  
0x7ff600b0d07f Bbt.Tests.Runner.Run_Doc at bbt-tests-runner.adb:380  
0x7ff600b0e2fe Bbt.Tests.Runner.Run_All at bbt-tests-runner.adb:438  
0x7ff6009724cd Bbt.Main at bbt-main.adb:152  
0x7ff60097386e Main at b__bbt-main.adb:588  
0x7ff600971339 __tmainCRTStartup at ???  
0x7ff600971145 mainCRTStartup at ???  
[C:\WINDOWS\System32\KERNEL32.DLL]  
0x7ffe70ade8d5  
[C:\WINDOWS\SYSTEM32\ntdll.dll]  
0x7ffe7174c3fa  
  
  
## Summary : **Success**, 0 scenarios OK, 1 empty scenarios
~~~
does not contain expected:  
~~~
   - **NOK** : Then I should get no error (erroneous_or.md:3:)    
erroneous_or.md:3: Error: No error expected, but got one ( 1)    
   - [ ] scenario   [B 3/3](erroneous_or.md) **fails**  
~~~
  
   - [ ] scenario   [The last command does not meet expectation (test should fail)](..\..\features\A195_Run_Or.md) **fails**  

  ## Feature: Syntactic and Semantic error related to "or" should be detected  
   ### Scenario: [Missing command before "or"](..\..\features\A195_Run_Or.md): 
   - OK : Given the new file `erroneous1.md`   
   - OK : When I run `./bbt -c erroneous1.md`  
   - OK : Then I get no error  
   - **NOK** : And the output contains (..\docs\features\a195_run_or.md:56:)  
..\docs\features\a195_run_or.md:56: Error: Output:  
~~~
  
# Document: [erroneous1.md](erroneous1.md)    
   ### Scenario: [C 1/2](erroneous1.md):   
 Exception : EXCEPTION_ACCESS_VIOLATIONLoad address: 0x7ff600970000  
[C:\Users\Lionel\Prj\bbt\tests\bbt.exe]  
0x7ff600b70ec2 __gnat_personality_seh0 at raise-gcc.c:1826  
[C:\WINDOWS\SYSTEM32\ntdll.dll]  
0x7ffe7182623d  
0x7ffe716d2395  
0x7ffe71825b7c  
[C:\Users\Lionel\Prj\bbt\tests\bbt.exe]  
0x7ff600b3c48e ada__tags__object_specific_dataIP at ???  
0x7ff600a074c7 Bbt.Model.Documents.Parent_Doc at bbt-model-documents.adb:43  
0x7ff600b06b50 Bbt.Tests.Runner.Run_Step at bbt-tests-runner.adb:76  
0x7ff600b0a429 Bbt.Tests.Runner.Run_Scenario at bbt-tests-runner.adb:244  
0x7ff600b0c7f2 Bbt.Tests.Runner.Run_Scenario_List at bbt-tests-runner.adb:341  
0x7ff600b0d07f Bbt.Tests.Runner.Run_Doc at bbt-tests-runner.adb:380  
0x7ff600b0e2fe Bbt.Tests.Runner.Run_All at bbt-tests-runner.adb:438  
0x7ff6009724cd Bbt.Main at bbt-main.adb:152  
0x7ff60097386e Main at b__bbt-main.adb:588  
0x7ff600971339 __tmainCRTStartup at ???  
0x7ff600971145 mainCRTStartup at ???  
[C:\WINDOWS\System32\KERNEL32.DLL]  
0x7ffe70ade8d5  
[C:\WINDOWS\SYSTEM32\ntdll.dll]  
0x7ffe7174c3fa  
  
  
## Summary : **Success**, 0 scenarios OK, 1 empty scenarios
~~~
does not contain expected:  
~~~
   ### Scenario: [C 1/2](erroneous1.md):   
   - OK : When I run or `./sut --version`     
   - [X] scenario   [C 1/2](erroneous1.md) pass    
  
   ### Scenario: [C 2/2](erroneous1.md):   
   - OK : When I run or `./sut -v`     
   - [X] scenario   [C 2/2](erroneous1.md) pass    
  
## Summary : **Success**, 2 scenarios OK
~~~
  
   - [ ] scenario   [Missing command before "or"](..\..\features\A195_Run_Or.md) **fails**  

   ### Scenario: [Missing command after "or"](..\..\features\A195_Run_Or.md): 
   - OK : Given the new file `erroneous2.md`   
   - OK : When I run `./bbt -c erroneous2.md`  
   - OK : Then I get an error  
   - OK : And the output contains  
   - [X] scenario   [Missing command after "or"](..\..\features\A195_Run_Or.md) pass  

   ### Scenario: [Two consecutive "or"](..\..\features\A195_Run_Or.md): 
   - OK : Given the new file `erroneous3.md`   
   - OK : When I run `./bbt -c erroneous3.md`  
   - OK : Then I get no error  
   - OK : And the output contains  
   - [X] scenario   [Two consecutive "or"](..\..\features\A195_Run_Or.md) pass  


# Document: [A200_Regexp.md](..\..\features\A200_Regexp.md)  
  ## Feature: identifying expected output with regexp  
   ### Scenario: [version number match](..\..\features\A200_Regexp.md): 
   - OK : When I run `./sut -v`  
   - OK : Then output matches `sut version [0-9]+\.[0-9]+`  
   - [X] scenario   [version number match](..\..\features\A200_Regexp.md) pass  

   ### Scenario: [version number mismatch](..\..\features\A200_Regexp.md): 
   - OK : Given the new file `wrong_regexp.md`  
   - OK : When I run `./bbt wrong_regexp.md`  
   - OK : Then I get an error  
   - OK : and output contains   
   - [X] scenario   [version number mismatch](..\..\features\A200_Regexp.md) pass  

   ### Scenario: [Test of "output does not match"](..\..\features\A200_Regexp.md): 
   - OK : When I run `./sut -v`  
   - OK : Then output does not match `sut version [0-9]+\.[0-9]+\.[0-9]+`  
   - [X] scenario   [Test of "output does not match"](..\..\features\A200_Regexp.md) pass  

   ### Scenario: [Test of "output does not match" that indeed matches](..\..\features\A200_Regexp.md): 
   - OK : Given the new file `wrong_regexp.md`  
   - OK : When I run `./bbt wrong_regexp.md`  
   - OK : Then I get an error  
   - OK : and output contains   
   - [X] scenario   [Test of "output does not match" that indeed matches](..\..\features\A200_Regexp.md) pass  

   ### Scenario: [test of "file match"](..\..\features\A200_Regexp.md): 
   - OK : Given the new file `Cities.txt`  
   - OK : Then `Cities.txt` match `.*ehera.*`  
   - OK : And  `Cities.txt` does not match `.*York.*`  
   - [X] scenario   [test of "file match"](..\..\features\A200_Regexp.md) pass  


# Document: [A210_Exact_Match.md](..\..\features\A210_Exact_Match.md)  
   ### Background: [](..\..\features\A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - [X] background [](..\..\features\A210_Exact_Match.md) pass  

   ### Scenario: [Human match](..\..\features\A210_Exact_Match.md): 
   - OK : When I run `./bbt compare.md`  
   - OK : Then I get no error  
   - OK : When I run `./bbt --human_match compare.md`  
   - OK : Then I get no error  
   - [X] scenario   [Human match](..\..\features\A210_Exact_Match.md) pass  

   ### Background: [](..\..\features\A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - [X] background [](..\..\features\A210_Exact_Match.md) pass  

   ### Scenario: [exact match](..\..\features\A210_Exact_Match.md): 
   - OK : When I run `./bbt -k --exact_match compare.md`  
   - OK : Then I get an error  
   - OK : And output contains  
   - OK : And output contains  
   - OK : And output contains  
   - [X] scenario   [exact match](..\..\features\A210_Exact_Match.md) pass  

   ### Background: [](..\..\features\A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - [X] background [](..\..\features\A210_Exact_Match.md) pass  

   ### Scenario: [exact match except for casing](..\..\features\A210_Exact_Match.md): 
   - OK : When I run `./bbt -k -em -ic compare.md`  
   - OK : Then I get an error  
   - OK : And output contains  
   - OK : And output contains  
   - OK : And output contains  
   - [X] scenario   [exact match except for casing](..\..\features\A210_Exact_Match.md) pass  

   ### Background: [](..\..\features\A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - [X] background [](..\..\features\A210_Exact_Match.md) pass  

   ### Scenario: [exact match except for casing and blank lines](..\..\features\A210_Exact_Match.md): 
   - OK : When I run `./bbt -k -em --ignore_casing -ibl compare.md`  
   - OK : Then I get an error  
   - OK : And output contains  
   - OK : And output contains  
   - OK : And output contains  
   - [X] scenario   [exact match except for casing and blank lines](..\..\features\A210_Exact_Match.md) pass  


# Document: [A220_AsciiDoc_gcc_hello_world.adoc](..\..\features\A220_AsciiDoc_gcc_hello_world.adoc)  
   ### Scenario: [gcc version?](..\..\features\A220_AsciiDoc_gcc_hello_world.adoc): 
   - OK : When I run `gcc -v`  
   - OK : Then the output contains `version `  
   - OK : Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
   - [X] scenario   [gcc version?](..\..\features\A220_AsciiDoc_gcc_hello_world.adoc) pass  

   ### Scenario: [compiling and executing an hello word](..\..\features\A220_AsciiDoc_gcc_hello_world.adoc): 
   - OK : Given the new file `main.c`  
   - OK : And given there is no `main` file  
   - OK : When I successfully run `gcc main.c -o main`  
   - OK : And  I run `./main`  
   - OK : Then the output is `Hello, World!`  
   - [X] scenario   [compiling and executing an hello word](..\..\features\A220_AsciiDoc_gcc_hello_world.adoc) pass  


# Document: [A230_select_exclude_include.md](..\..\features\A230_select_exclude_include.md)  
  ## Feature: Filter  
   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [no filtering](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt suite_1.md`    
   - OK : Then file `output_1.txt` is  
   - [X] scenario   [no filtering](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [step filtering](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt --exclude Windows suite_1.md`    
   - OK : Then file `output_1.txt` is  
   - [X] scenario   [step filtering](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [step selection](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select create suite_1.md`    
   - OK : Then file `output_1.txt` contains  
   - [X] scenario   [step selection](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [Selecting a scenario](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select Robustness suite_1.md suite_2.md`    
   - OK : Then there is no file `output_1.txt`  
   - OK : And `output_2.txt` is  
   - [X] scenario   [Selecting a scenario](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [selection is empty](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select xzscskfjhs suite_1.md suite_2.md`    
   - OK : Then there is no file `output_1.txt`  
   - OK : And  there is no file `output_2.txt`  
   - [X] scenario   [selection is empty](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [Selecting a Background only](..\..\features\A230_select_exclude_include.md): 
   - OK : Given the file `background.md`  
   - OK : When I run `./bbt --select new_file background.md`    
   - OK : Then there is no file `input.txt`  
   - OK : And  there is no file `output_2.txt`  
   - [X] scenario   [Selecting a Background only](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [Select followed by an exclude](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select Robustness --exclude Linux suite_2.md`  
   - OK : Then `output_2.txt` is  
   - [X] scenario   [Select followed by an exclude](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [scenario Excluded followed by an include of a step inside](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt --exclude Robustness --include create --include Linux suite_2.md`    
   - OK : Then `output_2.txt` is  
   - [X] scenario   [scenario Excluded followed by an include of a step inside](..\..\features\A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](..\..\features\A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](..\..\features\A230_select_exclude_include.md) pass  

   ### Scenario: [Exclude by file name](..\..\features\A230_select_exclude_include.md): 
   - OK : When I run `./bbt --exclude suite_2.md suite_2.md`    
   - OK : Then there is no `output_2.txt` file  
   - [X] scenario   [Exclude by file name](..\..\features\A230_select_exclude_include.md) pass  


# Document: [A240_Then_I_Succesfully_Run.md](..\..\features\A240_Then_I_Succesfully_Run.md)  
   ### Scenario: [Running something in postcondition](..\..\features\A240_Then_I_Succesfully_Run.md): 
   - OK : Given the new file `simple.xml`  
   - OK : Then I successfully run `xmllint simple.xml`   
   - [X] scenario   [Running something in postcondition](..\..\features\A240_Then_I_Succesfully_Run.md) pass  


# Document: [A250_Fails.md](..\..\features\A250_Fails.md)  
   ### Scenario: [Checking that a command fails](..\..\features\A250_Fails.md): 
   - OK : Given the new file `mismatched_tag.xml`  
   - OK : Then `xmllint mismatched_tag.xml` fails   
   - OK : And  the output matches `.*Opening and ending tag mismatch.*`  
   - OK : And  `xmllint mismatched_tag.xml` should fail   
   - OK : And  the output matches `.*Opening and ending tag mismatch.*`  
   - [X] scenario   [Checking that a command fails](..\..\features\A250_Fails.md) pass  


# Document: [B010_Deleting_created_files.md](..\..\features\B010_Deleting_created_files.md)  
  ## Feature: tmp files and directories deletion  
   ### Background: [](..\..\features\B010_Deleting_created_files.md): 
   - OK : Given there is no dir `dir1`  
   - OK : Given there is no dir `dir3`  
   - OK : Given there is no file `f1`  
   - OK : Given the new `create_tree.md` file  
   - [X] background [](..\..\features\B010_Deleting_created_files.md) pass  

   ### Scenario: [run without --cleanup](..\..\features\B010_Deleting_created_files.md): 
   - OK : When I run `./bbt --yes create_tree.md`  
   - OK : Then there is a `dir1/dir2/f2` file  
   - OK : Then there is a `dir3/dir4/dir5` dir  
   - OK : And  there is a `f1` file  
   - [X] scenario   [run without --cleanup](..\..\features\B010_Deleting_created_files.md) pass  

   ### Background: [](..\..\features\B010_Deleting_created_files.md): 
   - OK : Given there is no dir `dir1`  
   - OK : Given there is no dir `dir3`  
   - OK : Given there is no file `f1`  
   - OK : Given the new `create_tree.md` file  
   - [X] background [](..\..\features\B010_Deleting_created_files.md) pass  

   ### Scenario: [run with --cleanup](..\..\features\B010_Deleting_created_files.md): 
   - OK : When I run `./bbt --cleanup create_tree.md`  
   - OK : Then there is no `dir1` directory  
   - OK : And  there is no `dir3` directory  
   - OK : And  there is no `f1` file  
   - [X] scenario   [run with --cleanup](..\..\features\B010_Deleting_created_files.md) pass  


# Document: [B020_Exec_dir.md](..\..\features\B020_Exec_dir.md)  
  ## Feature: **Exec Dir**  
   ### Background: [create some dir and file](..\..\features\B020_Exec_dir.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given there is no `dir2` directory  
   - OK : Given the new `create_tree.md` file  
   - [X] background [create some dir and file](..\..\features\B020_Exec_dir.md) pass  

   ### Scenario: [Lets run "create_tree.md" in the current dir](..\..\features\B020_Exec_dir.md): 
   - OK : When I run `./bbt create_tree.md`  
   - OK : Then there is a `dir1` dir  
   - OK : And there is a `dir1/file1` file  
   - [X] scenario   [Lets run "create_tree.md" in the current dir](..\..\features\B020_Exec_dir.md) pass  

   ### Background: [create some dir and file](..\..\features\B020_Exec_dir.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given there is no `dir2` directory  
   - OK : Given the new `create_tree.md` file  
   - [X] background [create some dir and file](..\..\features\B020_Exec_dir.md) pass  

   ### Scenario: [Lets run "create_tree.md" in ./dir2](..\..\features\B020_Exec_dir.md): 
   - OK : Given the new `dir2` directory  
   - OK : When I run `./bbt create_tree.md --exec_dir dir2`  
   - OK : Then there is a `dir2/dir1` dir  
   - OK : And there is a `dir2/dir1/file1` file  
   - OK : And there is no `dir1` dir  
   - [X] scenario   [Lets run "create_tree.md" in ./dir2](..\..\features\B020_Exec_dir.md) pass  


# Document: [B030_File_creation_in_Given_steps.md](..\..\features\B030_File_creation_in_Given_steps.md)  
  ## Feature: testing the existence of a file  
   ### Scenario: [a required file does not exist](..\..\features\B030_File_creation_in_Given_steps.md): 
   - OK : Given there is no file `config.ini`  
   - OK : When I run `./sut read config.ini`  
   - OK : Then I get error  
   - [X] scenario   [a required file does not exist](..\..\features\B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: [the required file is created](..\..\features\B030_File_creation_in_Given_steps.md): 
   - OK : Given my favorite and so useful `config.ini` file  
   - OK : Then `config.ini` contains `Tmp_dir=/tmp`  
   - [X] scenario   [the required file is created](..\..\features\B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: ["Given there is no", when there actually is, should erase the file](..\..\features\B030_File_creation_in_Given_steps.md): 
   - OK : Given there is no `config.ini` file    
   - OK : Then there is no more `config.ini` file  
   - [X] scenario   ["Given there is no", when there actually is, should erase the file](..\..\features\B030_File_creation_in_Given_steps.md) pass  


# Document: [B040_Find_scenarios.md](..\..\features\B040_Find_scenarios.md)  
  ## Feature: multiples scenarios given in command line  
   ### Background: [](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](..\..\features\B040_Find_scenarios.md) pass  

   ### Scenario: [no file or dir on the command line](..\..\features\B040_Find_scenarios.md): 
   - OK : When I run `./bbt`  
   - OK : Then the output contains  
   - OK : And there is no error  
   - [X] scenario   [no file or dir on the command line](..\..\features\B040_Find_scenarios.md) pass  

   ### Background: [](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](..\..\features\B040_Find_scenarios.md) pass  

   ### Scenario: [running all scenarios in dir1](..\..\features\B040_Find_scenarios.md): 
   - OK : When I run `./bbt lf dir1`  
   - OK : And the output is on Windows_Only (unordered)  
   - [X] scenario   [running all scenarios in dir1](..\..\features\B040_Find_scenarios.md) pass  

   ### Background: [](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](..\..\features\B040_Find_scenarios.md) pass  

   ### Scenario: [running multiple scenarios given on command line](..\..\features\B040_Find_scenarios.md): 
   - OK : When I run `./bbt list_files dir1/scen3.scen dir1/scen4.scen`    
   - OK : Then the output is  
   - [X] scenario   [running multiple scenarios given on command line](..\..\features\B040_Find_scenarios.md) pass  

   ### Background: [](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](..\..\features\B040_Find_scenarios.md) pass  

   ### Scenario: [running scenarios in a tree thanks to `-r`](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the `dir1/dir2/` dir  
   - OK : Given the `dir1/dir3/dir4` dir  
   - OK : Given the `dir1/dir2/scen5.md` file containing `foo`  
   - OK : Given the `dir1/dir3/dir4/scen6.md` file containing `bar`  
   - OK : When I run `./bbt lf -r dir1`  
   - OK : And the output is on Windows_Only (unordered)  
   - [X] scenario   [running scenarios in a tree thanks to `-r`](..\..\features\B040_Find_scenarios.md) pass  

   ### Background: [](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](..\..\features\B040_Find_scenarios.md) pass  

   ### Scenario: [error msg when trying to run scenarios, but none found in given directories](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the `dir5` dir  
   - OK : Given the `dir6` dir  
   - OK : Given the `dir6/dir7` dir  
   - OK : When I run `./bbt dir5 dir6`  
   - OK : Then the output contains  
   - OK : And I get an error  
   - [X] scenario   [error msg when trying to run scenarios, but none found in given directories](..\..\features\B040_Find_scenarios.md) pass  

   ### Background: [](..\..\features\B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](..\..\features\B040_Find_scenarios.md) pass  

   ### Scenario: [empty list file on list_files if there is no scenario in given directories](..\..\features\B040_Find_scenarios.md): 
   - OK : When I run `./bbt lf dir5 dir6`  
   - OK : Then I get no output  
   - OK : And I get no error  
   - [X] scenario   [empty list file on list_files if there is no scenario in given directories](..\..\features\B040_Find_scenarios.md) pass  


# Document: [B050_Strict_gherkin.md](..\..\features\B050_Strict_gherkin.md)  
  ## Feature: Strict Gherkin rules  
   ### Scenario: [Multiple When in a scenario](..\..\features\B050_Strict_gherkin.md): 
   - OK : Given there is no file `tmp.txt`  
   - OK : Given the file `t1.md` :  
   - OK : When I successfully run `./bbt --strict t1.md`  
   - OK : Then the output contains  
   - OK : And `tmp.txt` contains   
   - OK : When I successfully run `./bbt t1.md`  
   - OK : Then the output do not contain `Warning`  
   - OK : And `tmp.txt` contains   
   - [X] scenario   [Multiple When in a scenario](..\..\features\B050_Strict_gherkin.md) pass  


# Document: [B060_Return_code_on_test_failure.md](..\..\features\B060_Return_code_on_test_failure.md)  
  ## Feature: return code on test failure  
   ### Scenario: [return code on test success](..\..\features\B060_Return_code_on_test_failure.md): 
   - OK : Given the new `good_option.md` file  
   - OK : When I run `./bbt good_option.md`  
   - OK : Then I get no error   
   - OK : and `good_option.md.out` is   
   - [X] scenario   [return code on test success](..\..\features\B060_Return_code_on_test_failure.md) pass  

   ### Scenario: [return code when the test fail](..\..\features\B060_Return_code_on_test_failure.md): 
   - OK : Given the new `wrong_option.md` file  
   - OK : When I run `./bbt --cleanup wrong_option.md`  
   - OK : Then I get an error  
   - [X] scenario   [return code when the test fail](..\..\features\B060_Return_code_on_test_failure.md) pass  

   ### Scenario: [return code when one fail and the other succeed](..\..\features\B060_Return_code_on_test_failure.md): 
   - OK : Given the new `wrong_and_good_option.md` file  
   - OK : When I run `./bbt --cleanup wrong_and_good_option.md`  
   - OK : Then I get an error  
   - [X] scenario   [return code when one fail and the other succeed](..\..\features\B060_Return_code_on_test_failure.md) pass  


# Document: [B080_Keep_Going.md](..\..\features\B080_Keep_Going.md)  
  ## Feature: on error, Keep going or stop  
   ### Background: [setup](..\..\features\B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](..\..\features\B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_syntax_error run without `-k`](..\..\features\B080_Keep_Going.md): 
   - OK : When I run `./bbt scenario_with_syntax_error.md`  
   - OK : then output is  
   - [X] scenario   [scenario_with_syntax_error run without `-k`](..\..\features\B080_Keep_Going.md) pass  

   ### Background: [setup](..\..\features\B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](..\..\features\B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_syntax_error run with `-k`](..\..\features\B080_Keep_Going.md): 
   - OK : When I run `./bbt -k scenario_with_syntax_error.md`  
   - OK : then output contains  
   - OK : and output contains  
   - OK : and output contains  
   - OK : and output contains  
   - OK : and output contains  
   - OK : and output contains  
   - [X] scenario   [scenario_with_syntax_error run with `-k`](..\..\features\B080_Keep_Going.md) pass  

   ### Background: [setup](..\..\features\B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](..\..\features\B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_assertion_error run without `-k`](..\..\features\B080_Keep_Going.md): 
   - OK : When I run `./bbt scenario_with_assertion_error.md`  
   - OK : then output is  
   - [X] scenario   [scenario_with_assertion_error run without `-k`](..\..\features\B080_Keep_Going.md) pass  

   ### Background: [setup](..\..\features\B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](..\..\features\B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_assertion_error run with `-k`](..\..\features\B080_Keep_Going.md): 
   - OK : When I run `./bbt -k scenario_with_assertion_error.md`  
   - OK : then output is  
   - [X] scenario   [scenario_with_assertion_error run with `-k`](..\..\features\B080_Keep_Going.md) pass  

   ### Background: [setup](..\..\features\B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](..\..\features\B080_Keep_Going.md) pass  

   ### Scenario: [explain scenario_with_syntax_error without `-k`](..\..\features\B080_Keep_Going.md): 
   - OK : When I run `./bbt ex scenario_with_syntax_error.md`  
   - OK : then output is  
   - [X] scenario   [explain scenario_with_syntax_error without `-k`](..\..\features\B080_Keep_Going.md) pass  

   ### Background: [setup](..\..\features\B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](..\..\features\B080_Keep_Going.md) pass  

   ### Scenario: [explain scenario_with_syntax_error with `-k`](..\..\features\B080_Keep_Going.md): 
   - OK : When I run `./bbt -k ex scenario_with_syntax_error.md`  
   - OK : then output is  
   - [X] scenario   [explain scenario_with_syntax_error with `-k`](..\..\features\B080_Keep_Going.md) pass  


# Document: [B090_tmp_dir.md](..\..\features\B090_tmp_dir.md)  
  ## Feature: Tmp dir  
   ### Background: [](..\..\features\B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](..\..\features\B090_tmp_dir.md) pass  

   ### Scenario: [Current dir, no cleanup](..\..\features\B090_tmp_dir.md): 
   - OK : When I run `./bbt scen1.md`   
   - OK : Then there is a `scen1.md.out` file  
   - [X] scenario   [Current dir, no cleanup](..\..\features\B090_tmp_dir.md) pass  

   ### Background: [](..\..\features\B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](..\..\features\B090_tmp_dir.md) pass  

   ### Scenario: [Current dir, with cleanup](..\..\features\B090_tmp_dir.md): 
   - OK : When I run `./bbt --cleanup scen1.md`   
   - OK : Then there is no `scen1.md.out` file  
   - [X] scenario   [Current dir, with cleanup](..\..\features\B090_tmp_dir.md) pass  

   ### Background: [](..\..\features\B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](..\..\features\B090_tmp_dir.md) pass  

   ### Scenario: [Within dir1, no cleanup](..\..\features\B090_tmp_dir.md): 
   - OK : When I run `./bbt --tmp_dir dir1 scen1.md`   
   - OK : Then there is a `dir1` directory  
   - OK : And  there is a `dir1/scen1.md.out` file  
   - [X] scenario   [Within dir1, no cleanup](..\..\features\B090_tmp_dir.md) pass  

   ### Background: [](..\..\features\B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](..\..\features\B090_tmp_dir.md) pass  

   ### Scenario: [Within dir1, with cleanup](..\..\features\B090_tmp_dir.md): 
   - OK : When I run `./bbt --cleanup -v --tmp_dir dir2 scen1.md`   
   - OK : Then there is no `dir2/scen1.md.out` file  
   - OK : And  there is no `dir2` directory  
   - [X] scenario   [Within dir1, with cleanup](..\..\features\B090_tmp_dir.md) pass  


# Document: [B100_Results_Output_In_MD_Format.md](..\..\features\B100_Results_Output_In_MD_Format.md)  
   ### Background: [](..\..\features\B100_Results_Output_In_MD_Format.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B100_Results_Output_In_MD_Format.md) pass  

   ### Scenario: [default mode run](..\..\features\B100_Results_Output_In_MD_Format.md): 
   - OK : When I run `./bbt -c --yes OK_scen.md`  
   - OK : Then the output is  
   - [X] scenario   [default mode run](..\..\features\B100_Results_Output_In_MD_Format.md) pass  

   ### Background: [](..\..\features\B100_Results_Output_In_MD_Format.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B100_Results_Output_In_MD_Format.md) pass  

   ### Scenario: [verbose mode run](..\..\features\B100_Results_Output_In_MD_Format.md): 
   - OK : When I run `./bbt -v -c --yes OK_scen.md`  
   - OK : Then the output contains  
   - [X] scenario   [verbose mode run](..\..\features\B100_Results_Output_In_MD_Format.md) pass  

   ### Background: [](..\..\features\B100_Results_Output_In_MD_Format.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B100_Results_Output_In_MD_Format.md) pass  

   ### Scenario: [run with an error](..\..\features\B100_Results_Output_In_MD_Format.md): 
   - OK : When I run `./bbt -c --yes NOK_scen.md`  
   - OK : Then the output contains  
   - OK : And the output contains  
   - [X] scenario   [run with an error](..\..\features\B100_Results_Output_In_MD_Format.md) pass  


# Document: [B110_Spawn.md](..\..\features\B110_Spawn.md)  
  ## Feature: Command line to spawn processing  

# Document: [B120_Output_Verbosity.md](..\..\features\B120_Output_Verbosity.md)  
   ### Background: [](..\..\features\B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B120_Output_Verbosity.md) pass  

   ### Scenario: [Quiet mode run](..\..\features\B120_Output_Verbosity.md): 
   - OK : When I successfully run `./bbt -c --yes -q OK_scen.md`  
   - OK : Then the output is `## Summary : **Success**, 2 scenarios OK`  
   - [X] scenario   [Quiet mode run](..\..\features\B120_Output_Verbosity.md) pass  

   ### Background: [](..\..\features\B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B120_Output_Verbosity.md) pass  

   ### Scenario: [Default mode run](..\..\features\B120_Output_Verbosity.md): 
   - OK : When I successfully run `./bbt -c --yes OK_scen.md`  
   - OK : Then the output is  
   - [X] scenario   [Default mode run](..\..\features\B120_Output_Verbosity.md) pass  

   ### Background: [](..\..\features\B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B120_Output_Verbosity.md) pass  

   ### Scenario: [Verbose mode run](..\..\features\B120_Output_Verbosity.md): 
   - OK : When I successfully run `./bbt -c --yes -v OK_scen.md`  
   - OK : Then the output is  
   - [X] scenario   [Verbose mode run](..\..\features\B120_Output_Verbosity.md) pass  

   ### Background: [](..\..\features\B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B120_Output_Verbosity.md) pass  

   ### Scenario: [Normal mode with an error](..\..\features\B120_Output_Verbosity.md): 
   - OK : When I run `./bbt -c --yes NOK_scen.md`  
   - OK : Then there is an error  
   - OK : And  the output is  
   - [X] scenario   [Normal mode with an error](..\..\features\B120_Output_Verbosity.md) pass  

   ### Background: [](..\..\features\B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](..\..\features\B120_Output_Verbosity.md) pass  

   ### Scenario: [Quiet mode with an error](..\..\features\B120_Output_Verbosity.md): 
   - OK : When I run `./bbt -c --yes --quiet NOK_scen.md`  
   - OK : Then there is an error  
   - OK : And  the output is  
   - [X] scenario   [Quiet mode with an error](..\..\features\B120_Output_Verbosity.md) pass  


# Document: [B130_Cmd_Line_Help.md](..\..\features\B130_Cmd_Line_Help.md)  
  ## Feature: Clear command line help  
   ### Scenario: [calling bbt without parameter or with -h put the normal help](..\..\features\B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt`   
   - **NOK** : then the output is equal to file `../docs/help/base.txt` (..\docs\features\b130_cmd_line_help.md:18:)  
..\docs\features\b130_cmd_line_help.md:18: Error: Output:  
~~~
Usage : bbt [Options]* [Command] file*
  

  
  The default command is 'run'
  
  If no file is provided, reads *.md files
  

  
Basic options:
  
       --yes        : do not prompt if deletion is needed in
  
                      "Given" steps, silently answer yes
  
  -c | --cleanup    : after run, remove every file and dir
  
                      created by bbt in "Given" steps
  
  -r | --recursive  : search scenarios in subdirs
  
  -k | --keep_going : do as much work as possible
  
       --Werror     : treat warnings as errors
  
  -v | --verbose
  
  -q | --quiet      : no message unless error,
  
                      Warnings are also ignored
  

  
Basic commands:
  
       run               : the default command
  
  ls | list              : list selected items
  
  he | help [topic]      : base help, or more on one of the topic listed below
  
  he | help on_all       : full online help
  
  he | help tutorial     : print a tutorial 
  
  he | help example      : print an example scenario file
  

  
Help topics:
  
  filtering : --select --exclude --include
  
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines
  
  other     : list_files list_keywords list_grammar explain --strict
  
              --index file.md --junit file.xml --exec_dir --tmp_dir --generate_badge
  
  debug     : -d tt -ls -t
  

  
bbt version 0.3.0-dev
  
https://github.com/LionelDraghi/bbt/
  

~~~
not equal to expected:  
~~~
Usage : bbt [Options]* [Command] file*  
  
  The default command is 'run'  
  If no file is provided, reads *.md files  
  
Basic options:  
       --yes        : do not prompt if deletion is needed in  
                      "Given" steps, silently answer yes  
  -c | --cleanup    : after run, remove every file and dir  
                      created by bbt in "Given" steps  
  -r | --recursive  : search scenarios in subdirs  
  -k | --keep_going : do as much work as possible  
       --Werror     : treat warnings as errors  
  -v | --verbose  
  -q | --quiet      : no message unless error,  
                      Warnings are also ignored  
  
Basic commands:  
       run               : the default command  
  ls | list              : list selected items  
  he | help [topic]      : base help, or more on one of the topic listed below  
  he | help on_all       : full online help  
  he | help tutorial     : print a tutorial   
  he | help example      : print an example scenario file  
  
Help topics:  
  filtering : --select --exclude --include  
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines  
  other     : list_files list_keywords list_grammar explain --strict  
              --index file.md --junit file.xml --exec_dir --tmp_dir --generate_badge  
  debug     : -d tt -ls -t  
  
bbt version 0.3.0-dev  
https://github.com/LionelDraghi/bbt/
~~~
  
   - [ ] scenario   [calling bbt without parameter or with -h put the normal help](..\..\features\B130_Cmd_Line_Help.md) **fails**  

   ### Scenario: [filtering help](..\..\features\B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt he filtering`   
   - **NOK** : then the output is equal to file `../docs/help/filtering.txt` (..\docs\features\b130_cmd_line_help.md:22:)  
..\docs\features\b130_cmd_line_help.md:22: Error: Output:  
~~~
Filtering:
  
  Features, Scenarios and Steps may be selected or filtered.
  
  By default, every item is selected.
  
  -s | --select 'string'  : only items containing 'string' are selected
  
  -e | --exclude 'string' : remove from selection items containing 'string'
  
  -i | --include 'string' : include in selection items containing 'string'
  
  Multiple occurrences are processed in order, meaning that you can exclude
  
  a whole Feature and then re-include a Scenario belonging to this feature.
  

~~~
not equal to expected:  
~~~
Filtering:  
  Features, Scenarios and Steps may be selected or filtered.  
  By default, every item is selected.  
  -s | --select 'string'  : only items containing 'string' are selected  
  -e | --exclude 'string' : remove from selection items containing 'string'  
  -i | --include 'string' : include in selection items containing 'string'  
  Multiple occurrences are processed in order, meaning that you can exclude  
  a whole Feature and then re-include a Scenario belonging to this feature.
~~~
  
   - [ ] scenario   [filtering help](..\..\features\B130_Cmd_Line_Help.md) **fails**  

   ### Scenario: [matching help](..\..\features\B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help matching`   
   - **NOK** : then the output is equal to file `../docs/help/matching.txt` (..\docs\features\b130_cmd_line_help.md:26:)  
..\docs\features\b130_cmd_line_help.md:26: Error: Output:  
~~~
Human vs exact matching:
  
  bbt default behavior is "human match", that is ignoring differences
  
  in casing, ignoring consecutive spaces, and ignoring blank lines.
  
  The opposite behavior, to make strict compare, is set with:
  
  -em  | --exact_match
  
  exact_match may be altered if **followed** by one or more of:
  
  -iw  | --ignore_whitespaces (default)
  
  -ic  | --ignore_casing      (default)
  
  -ibl | --ignore_blank_lines (default)
  
  For example, "-em -iw" will take into account blank lines and
  
  casing but ignore whitespaces
  
  Note that -iw, -ic, and -ibl are useless if not preceded by -em, 
  
  because they are the default setting.
  
  There is also a
  
  -hm  | --human_match
  
  option, equivalent to defaults "-iw -ic -ibl", if you want to
  
  assert on the command line that this is the required behavior.
  

~~~
not equal to expected:  
~~~
Human vs exact matching:  
  bbt default behavior is "human match", that is ignoring differences  
  in casing, ignoring consecutive spaces, and ignoring blank lines.  
  The opposite behavior, to make strict compare, is set with:  
  -em  | --exact_match  
  exact_match may be altered if **followed** by one or more of:  
  -iw  | --ignore_whitespaces (default)  
  -ic  | --ignore_casing      (default)  
  -ibl | --ignore_blank_lines (default)  
  For example, "-em -iw" will take into account blank lines and  
  casing but ignore whitespaces  
  Note that -iw, -ic, and -ibl are useless if not preceded by -em,   
  because they are the default setting.  
  There is also a  
  -hm  | --human_match  
  option, equivalent to defaults "-iw -ic -ibl", if you want to  
  assert on the command line that this is the required behavior.
~~~
  
   - [ ] scenario   [matching help](..\..\features\B130_Cmd_Line_Help.md) **fails**  

   ### Scenario: [others help](..\..\features\B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help other`   
   - **NOK** : then the output is equal to file `../docs/help/other.txt` (..\docs\features\b130_cmd_line_help.md:30:)  
..\docs\features\b130_cmd_line_help.md:30: Error: Output:  
~~~
Other commands:
  
  lf | list_files      : list Scenario files found
  
  lk | list_keywords   : list Step keywords
  
  lg | list_grammar    : list possible Steps syntax and associated action
  
  ex | explain         : explain what bbt understands from Scenarios files
  
                         (do not run the scenarios)
  

  
Other options:
  
        --strict         : warn when not strictly following Gherkin common guidelines
  
        --index file.md  : create an md file with test results
  
                           that indexes all scenarios run.
  
                           This file will contain the normal bbt output,
  
                           whatever are the verbosity settings (-q, -v, etc.)
  
                           for standard output.
  
  -ed | --exec_dir 'dir' : run command in dir instead of current dir
  
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir
  
  -sb | --status_bar     : enable a progress bar in the terminal (WIP!!)
  
  -j  | --junit file.xml : generate a JUnit XML report file
  
  -gb | --generate_badge badge.url : create a text file containing
  
                           a shields.io URL to get a svg badge
  
                           with tests results summary.
  

~~~
not equal to expected:  
~~~
Other commands:  
  lf | list_files      : list Scenario files found  
  lk | list_keywords   : list Step keywords  
  lg | list_grammar    : list possible Steps syntax and associated action  
  ex | explain         : explain what bbt understands from Scenarios files  
                         (do not run the scenarios)  
  
Other options:  
        --strict         : warn when not strictly following Gherkin common guidelines  
        --index file.md  : create an md file with test results  
                           that indexes all scenarios run.  
                           This file will contain the normal bbt output,  
                           whatever are the verbosity settings (-q, -v, etc.)  
                           for standard output.  
  -ed | --exec_dir 'dir' : run command in dir instead of current dir  
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir  
  -sb | --status_bar     : enable a progress bar in the terminal (WIP!!)  
  -j  | --junit file.xml : generate a JUnit XML report file  
  -gb | --generate_badge badge.url : create a text file containing  
                           a shields.io URL to get a svg badge  
                           with tests results summary.
~~~
  
   - [ ] scenario   [others help](..\..\features\B130_Cmd_Line_Help.md) **fails**  

   ### Scenario: [On_All help](..\..\features\B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help on_all`   
   - **NOK** : then the output contains file `../docs/help/base.txt` (..\docs\features\b130_cmd_line_help.md:34:)  
..\docs\features\b130_cmd_line_help.md:34: Error: Output:  
~~~
Usage : bbt [Options]* [Command] file*
  

  
  The default command is 'run'
  
  If no file is provided, reads *.md files
  

  
Basic options:
  
       --yes        : do not prompt if deletion is needed in
  
                      "Given" steps, silently answer yes
  
  -c | --cleanup    : after run, remove every file and dir
  
                      created by bbt in "Given" steps
  
  -r | --recursive  : search scenarios in subdirs
  
  -k | --keep_going : do as much work as possible
  
       --Werror     : treat warnings as errors
  
  -v | --verbose
  
  -q | --quiet      : no message unless error,
  
                      Warnings are also ignored
  

  
Basic commands:
  
       run               : the default command
  
  ls | list              : list selected items
  
  he | help [topic]      : base help, or more on one of the topic listed below
  
  he | help on_all       : full online help
  
  he | help tutorial     : print a tutorial 
  
  he | help example      : print an example scenario file
  

  
Help topics:
  
  filtering : --select --exclude --include
  
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines
  
  other     : list_files list_keywords list_grammar explain --strict
  
              --index file.md --junit file.xml --exec_dir --tmp_dir --generate_badge
  
  debug     : -d tt -ls -t
  

  
bbt version 0.3.0-dev
  
https://github.com/LionelDraghi/bbt/
  
  
Filtering:
  
  Features, Scenarios and Steps may be selected or filtered.
  
  By default, every item is selected.
  
  -s | --select 'string'  : only items containing 'string' are selected
  
  -e | --exclude 'string' : remove from selection items containing 'string'
  
  -i | --include 'string' : include in selection items containing 'string'
  
  Multiple occurrences are processed in order, meaning that you can exclude
  
  a whole Feature and then re-include a Scenario belonging to this feature.
  
  
Human vs exact matching:
  
  bbt default behavior is "human match", that is ignoring differences
  
  in casing, ignoring consecutive spaces, and ignoring blank lines.
  
  The opposite behavior, to make strict compare, is set with:
  
  -em  | --exact_match
  
  exact_match may be altered if **followed** by one or more of:
  
  -iw  | --ignore_whitespaces (default)
  
  -ic  | --ignore_casing      (default)
  
  -ibl | --ignore_blank_lines (default)
  
  For example, "-em -iw" will take into account blank lines and
  
  casing but ignore whitespaces
  
  Note that -iw, -ic, and -ibl are useless if not preceded by -em, 
  
  because they are the default setting.
  
  There is also a
  
  -hm  | --human_match
  
  option, equivalent to defaults "-iw -ic -ibl", if you want to
  
  assert on the command line that this is the required behavior.
  
  
Other commands:
  
  lf | list_files      : list Scenario files found
  
  lk | list_keywords   : list Step keywords
  
  lg | list_grammar    : list possible Steps syntax and associated action
  
  ex | explain         : explain what bbt understands from Scenarios files
  
                         (do not run the scenarios)
  

  
Other options:
  
        --strict         : warn when not strictly following Gherkin common guidelines
  
        --index file.md  : create an md file with test results
  
                           that indexes all scenarios run.
  
                           This file will contain the normal bbt output,
  
                           whatever are the verbosity settings (-q, -v, etc.)
  
                           for standard output.
  
  -ed | --exec_dir 'dir' : run command in dir instead of current dir
  
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir
  
  -sb | --status_bar     : enable a progress bar in the terminal (WIP!!)
  
  -j  | --junit file.xml : generate a JUnit XML report file
  
  -gb | --generate_badge badge.url : create a text file containing
  
                           a shields.io URL to get a svg badge
  
                           with tests results summary.
  
  
Debug command:
  
   tt             : list trace topics
  
Debug options:
  
   -ls            : list settings
  
   -d             : very very verbose output
  
   -d trace_topic : activate debug traces for the topic
  

  
Friends are here : https://github.com/LionelDraghi/bbt/Issues
  
Good luck :-)
  

~~~
does not contain expected:  
~~~
Usage : bbt [Options]* [Command] file*  
  
  The default command is 'run'  
  If no file is provided, reads *.md files  
  
Basic options:  
       --yes        : do not prompt if deletion is needed in  
                      "Given" steps, silently answer yes  
  -c | --cleanup    : after run, remove every file and dir  
                      created by bbt in "Given" steps  
  -r | --recursive  : search scenarios in subdirs  
  -k | --keep_going : do as much work as possible  
       --Werror     : treat warnings as errors  
  -v | --verbose  
  -q | --quiet      : no message unless error,  
                      Warnings are also ignored  
  
Basic commands:  
       run               : the default command  
  ls | list              : list selected items  
  he | help [topic]      : base help, or more on one of the topic listed below  
  he | help on_all       : full online help  
  he | help tutorial     : print a tutorial   
  he | help example      : print an example scenario file  
  
Help topics:  
  filtering : --select --exclude --include  
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines  
  other     : list_files list_keywords list_grammar explain --strict  
              --index file.md --junit file.xml --exec_dir --tmp_dir --generate_badge  
  debug     : -d tt -ls -t  
  
bbt version 0.3.0-dev  
https://github.com/LionelDraghi/bbt/
~~~
  
   - OK : and  the output contains file `../docs/help/filtering.txt`  
   - OK : and  the output contains file `../docs/help/matching.txt`  
   - **NOK** : and  the output contains file `../docs/help/other.txt` (..\docs\features\b130_cmd_line_help.md:37:)  
..\docs\features\b130_cmd_line_help.md:37: Error: Output:  
~~~
Usage : bbt [Options]* [Command] file*
  

  
  The default command is 'run'
  
  If no file is provided, reads *.md files
  

  
Basic options:
  
       --yes        : do not prompt if deletion is needed in
  
                      "Given" steps, silently answer yes
  
  -c | --cleanup    : after run, remove every file and dir
  
                      created by bbt in "Given" steps
  
  -r | --recursive  : search scenarios in subdirs
  
  -k | --keep_going : do as much work as possible
  
       --Werror     : treat warnings as errors
  
  -v | --verbose
  
  -q | --quiet      : no message unless error,
  
                      Warnings are also ignored
  

  
Basic commands:
  
       run               : the default command
  
  ls | list              : list selected items
  
  he | help [topic]      : base help, or more on one of the topic listed below
  
  he | help on_all       : full online help
  
  he | help tutorial     : print a tutorial 
  
  he | help example      : print an example scenario file
  

  
Help topics:
  
  filtering : --select --exclude --include
  
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines
  
  other     : list_files list_keywords list_grammar explain --strict
  
              --index file.md --junit file.xml --exec_dir --tmp_dir --generate_badge
  
  debug     : -d tt -ls -t
  

  
bbt version 0.3.0-dev
  
https://github.com/LionelDraghi/bbt/
  
  
Filtering:
  
  Features, Scenarios and Steps may be selected or filtered.
  
  By default, every item is selected.
  
  -s | --select 'string'  : only items containing 'string' are selected
  
  -e | --exclude 'string' : remove from selection items containing 'string'
  
  -i | --include 'string' : include in selection items containing 'string'
  
  Multiple occurrences are processed in order, meaning that you can exclude
  
  a whole Feature and then re-include a Scenario belonging to this feature.
  
  
Human vs exact matching:
  
  bbt default behavior is "human match", that is ignoring differences
  
  in casing, ignoring consecutive spaces, and ignoring blank lines.
  
  The opposite behavior, to make strict compare, is set with:
  
  -em  | --exact_match
  
  exact_match may be altered if **followed** by one or more of:
  
  -iw  | --ignore_whitespaces (default)
  
  -ic  | --ignore_casing      (default)
  
  -ibl | --ignore_blank_lines (default)
  
  For example, "-em -iw" will take into account blank lines and
  
  casing but ignore whitespaces
  
  Note that -iw, -ic, and -ibl are useless if not preceded by -em, 
  
  because they are the default setting.
  
  There is also a
  
  -hm  | --human_match
  
  option, equivalent to defaults "-iw -ic -ibl", if you want to
  
  assert on the command line that this is the required behavior.
  
  
Other commands:
  
  lf | list_files      : list Scenario files found
  
  lk | list_keywords   : list Step keywords
  
  lg | list_grammar    : list possible Steps syntax and associated action
  
  ex | explain         : explain what bbt understands from Scenarios files
  
                         (do not run the scenarios)
  

  
Other options:
  
        --strict         : warn when not strictly following Gherkin common guidelines
  
        --index file.md  : create an md file with test results
  
                           that indexes all scenarios run.
  
                           This file will contain the normal bbt output,
  
                           whatever are the verbosity settings (-q, -v, etc.)
  
                           for standard output.
  
  -ed | --exec_dir 'dir' : run command in dir instead of current dir
  
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir
  
  -sb | --status_bar     : enable a progress bar in the terminal (WIP!!)
  
  -j  | --junit file.xml : generate a JUnit XML report file
  
  -gb | --generate_badge badge.url : create a text file containing
  
                           a shields.io URL to get a svg badge
  
                           with tests results summary.
  
  
Debug command:
  
   tt             : list trace topics
  
Debug options:
  
   -ls            : list settings
  
   -d             : very very verbose output
  
   -d trace_topic : activate debug traces for the topic
  

  
Friends are here : https://github.com/LionelDraghi/bbt/Issues
  
Good luck :-)
  

~~~
does not contain expected:  
~~~
Other commands:  
  lf | list_files      : list Scenario files found  
  lk | list_keywords   : list Step keywords  
  lg | list_grammar    : list possible Steps syntax and associated action  
  ex | explain         : explain what bbt understands from Scenarios files  
                         (do not run the scenarios)  
  
Other options:  
        --strict         : warn when not strictly following Gherkin common guidelines  
        --index file.md  : create an md file with test results  
                           that indexes all scenarios run.  
                           This file will contain the normal bbt output,  
                           whatever are the verbosity settings (-q, -v, etc.)  
                           for standard output.  
  -ed | --exec_dir 'dir' : run command in dir instead of current dir  
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir  
  -sb | --status_bar     : enable a progress bar in the terminal (WIP!!)  
  -j  | --junit file.xml : generate a JUnit XML report file  
  -gb | --generate_badge badge.url : create a text file containing  
                           a shields.io URL to get a svg badge  
                           with tests results summary.
~~~
  
   - [ ] scenario   [On_All help](..\..\features\B130_Cmd_Line_Help.md) **fails**  

   ### Scenario: [tutorial generation](..\..\features\B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help tutorial`   
   - **NOK** : then the output is equal to file `../docs/help/tutorial.md` (..\docs\features\b130_cmd_line_help.md:42:)  
..\docs\features\b130_cmd_line_help.md:42: Error: Output:  
~~~

  
## Introduction  
  

  
This is a bbt tutorial, generated with BBT 0.3.0-dev  
  

  
A bbt file contains:  
  
1. text that is ignored
  
2. scenarios that are interpreted
  
 
  
A Scenario minimal structure is:  
  
1. a Scenario header, that is a line starting with "# Scenario : "  
  
2. One or more Steps, that is lines starting with "- Given", "- When" or "- Then"  
  

  
**Example :**  
  
    ## Scenario : get gcc version  
  
    - When I run `gcc --version`  
  
    - Then I get `14.2.0`  
  

  
## Scenarios structure   
  

  
The complete scenarios structure is heavily inspired by Gherkin files, with a few nuances. 
  

  
    [# Background] (at most one per file))
  

  
    [# Feature] (any number of features per file)
  

  
    [# Background] (at most one per Feature)
  

  
    # Scenario 1 (any number of scenarios per feature)
  
    - Given/When/Then step
  
    [- Given/When/Then/And/But step] (any number of steps per scenario)
  

  
**Example :**  
  

  
    # Feature : Case sensitivity control 
  

  
    ## Scenario : default behavior, no option  
  
    - When I run `grep xyz input.txt`  
  
    - Then ...  
  

  
    ## Scenario : case insensitive search  
  
    - When I run `grep -i xyz input.txt`  
  
    - Then ...  
  

  
The only headers reserved for bbt uses are "Feature", "Scenario" or "Example", and "Background"  
  
(Example is a synonym for Scenario).  
  
Header level is not taken into account : `# Scenario` is equivalent to `#### Scenario`.  
  

  
### Non interpreted content
  

  
Outside previously mentioned headers and steps, lines are ignored by bbt, that is considered as comments.
  
Meaning that you can interleave Scenarios with comments as you want: comments may appear between Header and Steps or even between Steps and code blocks.  
  
In case of doubt, just run `bbt explain` on your scenario to ensure that the file is understud the way you want.  
  

  
### Background  
  

  
Preconditions common to several scenarios may be put in a Background section, before scenarios :  
  

  
    ### Background:  
  
    - Given there is no `input.txt` file  
  
    - Given there is a `tmp` dir  
  

  
Background scope is logical : if it appears at the beginning of the file, it applies to all  
  
scenario in the file, if it appears at the beginning of a feature, it apply only  
  
to the scenarios of this feature.  
  
If there is both, Backgrounds are run in appearance order.  
  

  
**Example :**  
  

  
    ## Background 1 
  
    - Given there is no `config.ini` file  
  
    - Given ...  
  

  
    # Feature A  
  

  
    ## Scenario A.1  
  
    Background 1 will run here  
  
    - When I run `grep -i xyz input.txt`  
  
    - Then ...  
  

  
    # Feature B  
  

  
    ## Background 2 
  
    - Given ...  
  

  
    ## Scenario B.1  
  
    Background 1 run here  
  
    Background 2 run here  
  
    - When ...  
  

  
### Steps  
  

  
Steps are the most important part of bbt files, they perform the actions and checks.  
  
- Given [setup condition]  
  
- When  [action to perform]  
  
- Then  [expected result]  
  

  
**Examples of steps:**  
  

  
    - Given there is no `.config` dir
  
    - Given the `config.ini` file
  
      ```
  
      verbose=false
  
      lang=am
  
      ```
  
    - Given the executable file `command.sh`
  
      ```
  
      #!/bin/bash
  
      echo "bbt rules!"
  
      ```
  
    - When I successfully run `xxx`
  
      (Equivalent to both lines "- When I run `xxx`" and "- Then I Get No Error")
  
    - Then there is no output
  
    - Then I get no error
  
    - Then output is `sut v0.1.0` (Equivalent "Then I get...")
  

  
You can continue a list of Given / When / Then with "And" or "But":  
  

  
    - Then output contains `234 processed data`
  
    - And  output contains `result = 29580`
  
    - But  output doesn't contain `Warning:`
  
    - And  output does not contain `Error:`  
  

  
*And* and *But* are synonymous of the *Given* / *When* / *Then* that preceedes.  
  

  
### Parameters  
  

  
Parameters are given in three possible ways :  
  
  1. as a string:
  

  
    - Then I get `string`
  

  
  2. as a code fenced block:
  

  
    - Then I get
  
    ```
  
    This is my multi-line
  
    file content
  
    ```
  

  
  3. in an external file:
  

  
    - Then I get the content of file `expected.txt`  
  

  
     Note in that case the mandatory "file" keyword  
  

  
### Matching level  
  

  
Above forms test that the output is exactly what is given.  
  
If what you want is just test that the output contains something, then use the "contains" keyword:  
  

  
    - Then output contains `sut version v0.1.0`  
  

  
If what you want is search for some pattern, then use the "matches" keyword, followed by a regexp :  
  

  
    - Then output matches `sut version v[0-9]+\.[0-9]+\.[0-9]+`  
  

  
Note that the regexp must match the entire line,
  
don't forget to put ".*" at the beginning or at the end if necessary.  
  

  
## Help  
  

  
To get a complete (although less friendly) view on the grammar:  
  

  
    bbt list_grammar  
  

  
To check your scenario with a dry run:  
  

  
    bbt explain scenario.md  
  

  
More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-
  

~~~
not equal to expected:  
~~~
  
## Introduction    
  
This is a bbt tutorial, generated with BBT 0.3.0-dev    
  
A bbt file contains:    
1. text that is ignored  
2. scenarios that are interpreted  
   
A Scenario minimal structure is:    
1. a Scenario header, that is a line starting with "# Scenario : "    
2. One or more Steps, that is lines starting with "- Given", "- When" or "- Then"    
  
**Example :**    
    ## Scenario : get gcc version    
    - When I run `gcc --version`    
    - Then I get `14.2.0`    
  
## Scenarios structure     
  
The complete scenarios structure is heavily inspired by Gherkin files, with a few nuances.   
  
    [# Background] (at most one per file))  
  
    [# Feature] (any number of features per file)  
  
    [# Background] (at most one per Feature)  
  
    # Scenario 1 (any number of scenarios per feature)  
    - Given/When/Then step  
    [- Given/When/Then/And/But step] (any number of steps per scenario)  
  
**Example :**    
  
    # Feature : Case sensitivity control   
  
    ## Scenario : default behavior, no option    
    - When I run `grep xyz input.txt`    
    - Then ...    
  
    ## Scenario : case insensitive search    
    - When I run `grep -i xyz input.txt`    
    - Then ...    
  
The only headers reserved for bbt uses are "Feature", "Scenario" or "Example", and "Background"    
(Example is a synonym for Scenario).    
Header level is not taken into account : `# Scenario` is equivalent to `#### Scenario`.    
  
### Non interpreted content  
  
Outside previously mentioned headers and steps, lines are ignored by bbt, that is considered as comments.  
Meaning that you can interleave Scenarios with comments as you want: comments may appear between Header and Steps or even between Steps and code blocks.    
In case of doubt, just run `bbt explain` on your scenario to ensure that the file is understud the way you want.    
  
### Background    
  
Preconditions common to several scenarios may be put in a Background section, before scenarios :    
  
    ### Background:    
    - Given there is no `input.txt` file    
    - Given there is a `tmp` dir    
  
Background scope is logical : if it appears at the beginning of the file, it applies to all    
scenario in the file, if it appears at the beginning of a feature, it apply only    
to the scenarios of this feature.    
If there is both, Backgrounds are run in appearance order.    
  
**Example :**    
  
    ## Background 1   
    - Given there is no `config.ini` file    
    - Given ...    
  
    # Feature A    
  
    ## Scenario A.1    
    Background 1 will run here    
    - When I run `grep -i xyz input.txt`    
    - Then ...    
  
    # Feature B    
  
    ## Background 2   
    - Given ...    
  
    ## Scenario B.1    
    Background 1 run here    
    Background 2 run here    
    - When ...    
  
### Steps    
  
Steps are the most important part of bbt files, they perform the actions and checks.    
- Given [setup condition]    
- When  [action to perform]    
- Then  [expected result]    
  
**Examples of steps:**    
  
    - Given there is no `.config` dir  
    - Given the `config.ini` file  
      ```  
      verbose=false  
      lang=am  
      ```  
    - Given the executable file `command.sh`  
      ```  
      #!/bin/bash  
      echo "bbt rules!"  
      ```  
    - When I successfully run `xxx`  
      (Equivalent to both lines "- When I run `xxx`" and "- Then I Get No Error")  
    - Then there is no output  
    - Then I get no error  
    - Then output is `sut v0.1.0` (Equivalent "Then I get...")  
  
You can continue a list of Given / When / Then with "And" or "But":    
  
    - Then output contains `234 processed data`  
    - And  output contains `result = 29580`  
    - But  output doesn't contain `Warning:`  
    - And  output does not contain `Error:`    
  
*And* and *But* are synonymous of the *Given* / *When* / *Then* that preceedes.    
  
### Parameters    
  
Parameters are given in three possible ways :    
  1. as a string:  
  
    - Then I get `string`  
  
  2. as a code fenced block:  
  
    - Then I get  
    ```  
    This is my multi-line  
    file content  
    ```  
  
  3. in an external file:  
  
    - Then I get the content of file `expected.txt`    
  
     Note in that case the mandatory "file" keyword    
  
### Matching level    
  
Above forms test that the output is exactly what is given.    
If what you want is just test that the output contains something, then use the "contains" keyword:    
  
    - Then output contains `sut version v0.1.0`    
  
If what you want is search for some pattern, then use the "matches" keyword, followed by a regexp :    
  
    - Then output matches `sut version v[0-9]+\.[0-9]+\.[0-9]+`    
  
Note that the regexp must match the entire line,  
don't forget to put ".*" at the beginning or at the end if necessary.    
  
## Help    
  
To get a complete (although less friendly) view on the grammar:    
  
    bbt list_grammar    
  
To check your scenario with a dry run:    
  
    bbt explain scenario.md    
  
More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-
~~~
  
   - [ ] scenario   [tutorial generation](..\..\features\B130_Cmd_Line_Help.md) **fails**  

   ### Scenario: [generated example is OK](..\..\features\B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help example`   
   - **NOK** : then the output is equal to file `../docs/examples/gcc_hello_world.md` (..\docs\features\b130_cmd_line_help.md:50:)  
..\docs\features\b130_cmd_line_help.md:50: Error: Output:  
~~~
# gcc simple sanity tests
  

  
## Scenario 1 : get gcc version
  
  
  
  On Linux or Windows, `gcc -v` output contains something like: 
  
  > gcc version 14.2.0 (Debian 14.2.0-16)  
  

  
  On Darwin:  
  
  > Apple clang version 12.0.0 (clang-1200.0.32.29)  
  

  
Let's use a regexp to test both:  
  

  
- When I run `gcc -v`
  
- Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`
  

  
## Scenario 2 : compiling and executing an hello word
  

  
Sanity check of a complete compile / link / run sequence :
  

  
- Given the new file `main.c`
  
  ```c
  
  #include <stdio.h>
  
  int main() {
  
  printf("Hello, World!");
  
  return 0;
  
  }
  
  ```
  
- And given there is no `./main` file
  

  
- When I successfully run `gcc main.c -o main`
  
- And  I run `./main`
  

  
- Then the output is `Hello, World!`
  

~~~
not equal to expected:  
~~~
# gcc simple sanity tests  
  
## Scenario 1 : get gcc version  
    
  On Linux or Windows, `gcc -v` output contains something like:   
  > gcc version 14.2.0 (Debian 14.2.0-16)    
  
  On Darwin:    
  > Apple clang version 12.0.0 (clang-1200.0.32.29)    
  
Let's use a regexp to test both:    
  
- When I run `gcc -v`  
- Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
  
## Scenario 2 : compiling and executing an hello word  
  
Sanity check of a complete compile / link / run sequence :  
  
- Given the new file `main.c`  
  ```c  
  #include <stdio.h>  
  int main() {  
  printf("Hello, World!");  
  return 0;  
  }  
  ```  
- And given there is no `./main` file  
  
- When I successfully run `gcc main.c -o main`  
- And  I run `./main`  
  
- Then the output is `Hello, World!`
~~~
  
   - [ ] scenario   [generated example is OK](..\..\features\B130_Cmd_Line_Help.md) **fails**  


# Document: [B140_Index_File.md](..\..\features\B140_Index_File.md)  
   ### Scenario: [Successful run index file](..\..\features\B140_Index_File.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : And there is no `index1.md` file  
   - OK : And there is no `index2.md` file  
   - OK : And there is no `verbose_output_OK.md` file  
   - OK : When I successfully run `./bbt -c --yes -v OK_scen.md --index verbose_output_OK.md`  
   - OK : When I successfully run `./bbt -c --yes -q OK_scen.md --index index_1.md`  
   - OK : Then `index_1.md` is equal to file `verbose_output_OK.md`  
   - OK : When I successfully run `./bbt -c --yes    OK_scen.md --index index_2.md`  
   - OK : Then `index_2.md` is equal to file `verbose_output_OK.md`  
   - [X] scenario   [Successful run index file](..\..\features\B140_Index_File.md) pass  

   ### Scenario: [Unsuccessful run index file](..\..\features\B140_Index_File.md): 
   - OK : Given the file `NOK_scen.md`  
   - OK : And there is no `index3.md` file  
   - OK : And there is no `index4.md` file  
   - OK : And there is no `verbose_output_NOK.md` file  
   - OK : When I run `./bbt -c --yes -v NOK_scen.md --index verbose_output_NOK.md`  
   - OK : Then I get an error  
   - OK : When I run `./bbt -c --yes -q NOK_scen.md --index index_3.md`  
   - OK : Then I get an error  
   - OK : And `index_3.md` is equal to file `verbose_output_NOK.md`  
   - OK : When I  run `./bbt -c --yes    NOK_scen.md --index index_4.md`  
   - OK : Then  I get an error  
   - OK : And `index_4.md` is equal to file `verbose_output_NOK.md`  
   - [X] scenario   [Unsuccessful run index file](..\..\features\B140_Index_File.md) pass  


# Document: [B150_Deprecated_Options.md](..\..\features\B150_Deprecated_Options.md)  
  ## Feature: Deprecated and no more supported options  
   ### Scenario: [--output option](..\..\features\B150_Deprecated_Options.md): 
   - OK : Given the new `ver.md` file  
   - OK : When I run `./bbt -c --yes ver.md --output tmp.md`   
   - OK : Then the output contains   
   - [X] scenario   [--output option](..\..\features\B150_Deprecated_Options.md) pass  


# Document: [B160_JUnit_XML_Export.md](..\..\features\B160_JUnit_XML_Export.md)  
  ## Feature: JUnit XML Export  
   ### Scenario: [1 : Basic file with only a single scenario](..\..\features\B160_JUnit_XML_Export.md): 
   - OK : Given there is no file `result.xml`  
   - OK : Given the file `simple_test.md` containing  
   - OK : When I run `./bbt --junit result.xml simple_test.md`  
   - OK : Then I get no error  
   - OK : And file `result.xml` contains `<?xml version="1.0" encoding="UTF-8"?>`  
   - OK : And file `result.xml` matches `<testsuites name="result" tests="1" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`  
   - OK : And file `result.xml` matches `  <testsuite name="" tests="1" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`  
   - OK : And file `result.xml` matches `    <testcase name="Passing test" classname="" time="[0-9]*\.[0-9]*"/>`  
   - OK : And file `result.xml` contains  
   - [X] scenario   [1 : Basic file with only a single scenario](..\..\features\B160_JUnit_XML_Export.md) pass  

  ## Feature: Skipped tests count  
   ### Background: [](..\..\features\B160_JUnit_XML_Export.md): 
   - OK : Given there is no `file1` file  
   - OK : Given there is no `file2` file  
   - OK : Given the scenarios file `skipped_count_test.md`   
   - [X] background [](..\..\features\B160_JUnit_XML_Export.md) pass  

   ### Scenario: [1: both scenario are run](..\..\features\B160_JUnit_XML_Export.md): 
   - OK : Given there is no `all_results.xml` file  
   - OK : When I successfully run `./bbt --junit all_results.xml skipped_count_test.md`  
   - OK : Then I get no error  
   - OK : And file `all_results.xml` matches `<testsuites name="all_results" tests="2" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`  
   - [X] scenario   [1: both scenario are run](..\..\features\B160_JUnit_XML_Export.md) pass  

   ### Background: [](..\..\features\B160_JUnit_XML_Export.md): 
   - OK : Given there is no `file1` file  
   - OK : Given there is no `file2` file  
   - OK : Given the scenarios file `skipped_count_test.md`   
   - [X] background [](..\..\features\B160_JUnit_XML_Export.md) pass  

   ### Scenario: [2 : one scenario is skipped](..\..\features\B160_JUnit_XML_Export.md): 
   - OK : Given there is no `windows_results.xml` file  
   - OK : When I successfully run `./bbt --junit windows_results.xml --select @Windows_Specific skipped_count_test.md`  
   - OK : Then file `windows_results.xml` matches `<testsuites name="windows_results" tests="2" failures="0" errors="0" skipped="1" time="[0-9]*\.[0-9]*">`  
   - [X] scenario   [2 : one scenario is skipped](..\..\features\B160_JUnit_XML_Export.md) pass  

  ## Feature: xml robustness  
   ### Scenario: [Test XML escaping with special characters](..\..\features\B160_JUnit_XML_Export.md): 
   - OK : Given there is no file `result2.xml`  
   - OK : Given the file `escape_test.md` containing  
   - OK : When I run `./bbt --junit result2.xml escape_test.md`  
   - OK : Then I get no error  
   - OK : And file `result2.xml` matches `    <testcase name="Title with Quotation mark &quot; Ampersand &amp; Greater and less than &lt; &gt; or Apostrophe &apos;&quot;" classname="" time="[0-9]*\.[0-9]*"/>`  
   - [X] scenario   [Test XML escaping with special characters](..\..\features\B160_JUnit_XML_Export.md) pass  

  ## Feature: Time Attribute (@Flaky results depends on execution time on the plafform)  

# Document: [B170_Explain.md](..\..\features\B170_Explain.md)  
  ## Feature: Explain  
   ### Scenario: [explain elements other than steps](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain elements other than steps](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Setup_No_File](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Setup_No_File](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Setup_No_Dir](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Setup_No_Dir](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Check_File_Existence](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_File_Existence](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Check_Dir_Existence](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_Dir_Existence](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Erase_And_Create](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Erase_And_Create](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Create_If_None](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Create_If_None](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Run](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Run](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Run_Without_Error](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Run_Without_Error](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Check_No_File](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_No_File](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Check_No_Dir](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_No_Dir](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Error_Return_Code and No_Error_Return_Code](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Error_Return_Code and No_Error_Return_Code](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Output_Is](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Output_Is](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Output_Contains and Output_Does_Not_Contain](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Output_Contains and Output_Does_Not_Contain](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action Output|File_Matches and Output|File_Does_Not_Match](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Output|File_Matches and Output|File_Does_Not_Match](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action File_Is and File_Is_Not](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action File_Is and File_Is_Not](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action File_Contains and File_Does_Not_Contain](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action File_Contains and File_Does_Not_Contain](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain action No_Output](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action No_Output](..\..\features\B170_Explain.md) pass  

   ### Scenario: [explain Invalid Step](..\..\features\B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then the output contains  
   - OK : And the output contains  
   - OK : When I run `./bbt -q explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain Invalid Step](..\..\features\B170_Explain.md) pass  


# Document: [B180_Ignored_Files.md](..\..\features\B180_Ignored_Files.md)  
  ## Feature: some files are ignored by *bbt*  
   ### Scenario: [Checking that the index file is ignored](..\..\features\B180_Ignored_Files.md): 
   - OK : Given the new dir `tmp77`  
   - OK : When running  `./bbt list_files --tmp_dir tmp77 --index tmp77/index.md ../docs/examples/gcc_hello_world.md tmp77`  
   - OK : Then I should get   
   - [X] scenario   [Checking that the index file is ignored](..\..\features\B180_Ignored_Files.md) pass  


# Document: [C010_Empty_scenarios.md](..\..\features\C010_Empty_scenarios.md)  
   ### Scenario: [No step test A](..\..\features\C010_Empty_scenarios.md): 
   - OK : Given the `no_step_in_scenario.input` file  
   - OK : When I successfully run `./bbt no_step_in_scenario.input`   
   - OK : Then the output contains `scenario [My_Scenario](no_step_in_scenario.input) is empty, nothing tested`  
   - [X] scenario   [No step test A](..\..\features\C010_Empty_scenarios.md) pass  

   ### Scenario: [No step test B](..\..\features\C010_Empty_scenarios.md): 
   - OK : Given the `no_step_in_scenario_b.input` file  
   - OK : When I successfully run `./bbt no_step_in_scenario_b.input`   
   - OK : Then the output contains `scenario [My_Scenario](no_step_in_scenario_b.input) is empty, nothing tested`  
   - [X] scenario   [No step test B](..\..\features\C010_Empty_scenarios.md) pass  

   ### Scenario: [No scenario in Feature](..\..\features\C010_Empty_scenarios.md): 
   - OK : Given the `no_step_or_scenario_in_feature.input` file  
   - OK : When I successfully run `./bbt no_step_or_scenario_in_feature.input`   
   - OK : Then the output contains `Warning : No scenario in feature "My_Feature"`  
   - [X] scenario   [No scenario in Feature](..\..\features\C010_Empty_scenarios.md) pass  


# Document: [C030_Markdown_syntax.md](..\..\features\C030_Markdown_syntax.md)  
  ## Feature: Tolerance to formatting variations  
   ### Scenario: [Heading variations](..\..\features\C030_Markdown_syntax.md): 
   - OK : Given the new file `1.md` containing `# Scenario: S1`  
   - OK : Given the new file `2.md` containing `#Scenario: S2`  
   - OK : Given the new file `3.md` containing `# Scenario : S3`  
   - OK : Given the new file `4.md` containing `# Scenario S4`  
   - OK : Given the new file `5.md` containing `# Scenario :S5 etc etc! #########################`  
   - OK : Given the new file `6.md` containing `# Scenario`  
   - OK : Given the new file `7.md` containing `# Scenario:`  
   - OK : Given the new file `8.md` containing `# Scenario #################`  
   - OK : Given the new file `9.md` containing `  # Scenario`  
   - OK : When I successfully run `./bbt 1.md`  
   - OK : Then output contains `scenario [S1](1.md) is empty, nothing tested`    
   - OK : When I successfully run `./bbt 2.md`  
   - OK : Then output contains `scenario [S2](2.md) is empty, nothing tested`  
   - OK : And  output contains `2.md:1: Warning : Markdown expect space in Headings after last '#'`  
   - OK : When I successfully run `./bbt 3.md`  
   - OK : Then output contains `scenario [S3](3.md) is empty, nothing tested`  
   - OK : When I successfully run `./bbt 4.md`  
   - OK : Then output contains `scenario [S4](4.md) is empty, nothing tested`  
   - OK : When I successfully run `./bbt 5.md`  
   - OK : Then output contains `scenario [S5 etc etc!](5.md) is empty, nothing tested`  
   - OK : When I successfully run `./bbt 6.md`  
   - OK : Then output contains `scenario [](6.md) is empty, nothing tested`  
   - OK : When I successfully run `./bbt 7.md`  
   - OK : Then output contains `scenario [](7.md) is empty, nothing tested`  
   - OK : When I successfully run `./bbt 8.md`  
   - OK : Then output contains `scenario [](8.md) is empty, nothing tested`  
   - OK : When I successfully run `./bbt 9.md`  
   - OK : Then output contains `scenario [](9.md) is empty, nothing tested`  
   - [X] scenario   [Heading variations](..\..\features\C030_Markdown_syntax.md) pass  

   ### Scenario: [Missing heading marker](..\..\features\C030_Markdown_syntax.md): 
   - OK : Given the new file `no_heading_marker.input`  
   - OK : When I run `./bbt -d no_heading_marker.input`  
   - [X] scenario   [Missing heading marker](..\..\features\C030_Markdown_syntax.md) pass  

   ### Scenario: [Steps using [Emphasis and strong emphasis](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis)](..\..\features\C030_Markdown_syntax.md): 
   - OK : Given the new file `italic_and_bold_in_steps.md`  
   - OK : When I successfully run `./bbt explain italic_and_bold_in_steps.md`  
   - OK : Then the output contains  
   - [X] scenario   [Steps using [Emphasis and strong emphasis](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis)](..\..\features\C030_Markdown_syntax.md) pass  


# Document: [C040_Missing_title.md](..\..\features\C040_Missing_title.md)  
   ### Scenario: [Missing tittle in scenario, background and feature](..\..\features\C040_Missing_title.md): 
   - OK : Given the new file `no_title.md`  
   - OK : When I run `./bbt -c --yes no_title.md`  
   - OK : Then output is  
   - [X] scenario   [Missing tittle in scenario, background and feature](..\..\features\C040_Missing_title.md) pass  


# Document: [C050_Step_marker.md](..\..\features\C050_Step_marker.md)  
  ## Feature:   
   ### Scenario: [Different markers within the scenario](..\..\features\C050_Step_marker.md): 
   - OK : Given the file `step_markers.md`  
   - OK : When I run `./bbt -c -q step_markers.md`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [Different markers within the scenario](..\..\features\C050_Step_marker.md) pass  


# Document: [C060_code_block.md](..\..\features\C060_code_block.md)  
   ### Scenario: [Code fenced blocks](..\..\features\C060_code_block.md): 
   - OK : Given the new file `lot_of_code_blocks.md`  
   - OK : When I run `./bbt -c lot_of_code_blocks.md`  
   - OK : Then I get no error  
   - [X] scenario   [Code fenced blocks](..\..\features\C060_code_block.md) pass  


# Document: [C070_missing_code_block.md](..\..\features\C070_missing_code_block.md)  
  ## Feature: missing or erroneous code blocks  
   ### Scenario: [Code block missing at the end of the file](..\..\features\C070_missing_code_block.md): 
   - OK : Given the new file `code_block_missing_at_EOF.md`  
   - OK : When I run `./bbt -c code_block_missing_at_EOF.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Code block missing at the end of the file](..\..\features\C070_missing_code_block.md) pass  

   ### Scenario: [Code block missing while reaching next step](..\..\features\C070_missing_code_block.md): 
   - OK : Given the new file `code_block_missing_in_step.md`  
   - OK : When I run `./bbt -c code_block_missing_in_step.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Code block missing while reaching next step](..\..\features\C070_missing_code_block.md) pass  

   ### Scenario: [Code block missing while reaching next scenario](..\..\features\C070_missing_code_block.md): 
   - OK : Given the new file `code_block_missing_in_scenario.md`  
   - OK : When I run `./bbt -c code_block_missing_in_scenario.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Code block missing while reaching next scenario](..\..\features\C070_missing_code_block.md) pass  

   ### Scenario: [Closing code block mark missing](..\..\features\C070_missing_code_block.md): 
   - OK : Given the new file `code_block_not_closed.md`  
   - OK : When I run `./bbt -c code_block_not_closed.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - OK : Given the new file `code_block_not_closed2.md`  
   - OK : When I run `./bbt -c code_block_not_closed2.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Closing code block mark missing](..\..\features\C070_missing_code_block.md) pass  


# Document: [C080_missing_scenario.md](..\..\features\C080_missing_scenario.md)  
  ## Feature:   
   ### Scenario: [Steps without scenario header](..\..\features\C080_missing_scenario.md): 
   - OK : Given the file `No_Scenario.md`  
   - OK : When I run `./bbt No_Scenario.md`  
   - OK : Then output contains   
   - [X] scenario   [Steps without scenario header](..\..\features\C080_missing_scenario.md) pass  

   ### Scenario: [Empty file](..\..\features\C080_missing_scenario.md): 
   - OK : Given the file `empty.md`  
   - OK : When I run `./bbt empty.md`  
   - OK : Then output contains   
   - [X] scenario   [Empty file](..\..\features\C080_missing_scenario.md) pass  


# Document: [C090_GNU_Error_Msg_Format.md](..\..\features\C090_GNU_Error_Msg_Format.md)  
  ## Feature: GNU error messages  
   ### Scenario: [](..\..\features\C090_GNU_Error_Msg_Format.md): 
   - OK : Given the file `t1.md`  
   - OK : When I successfully run `./bbt --strict t1.md`  
   - OK : Then the output matches `t1.md:[0-9]*: Warning: Multiple When in the same Scenario.*`  
   - [X] scenario   [](..\..\features\C090_GNU_Error_Msg_Format.md) pass  


# Document: [C110_Two_Verbs_In_The_Step.md](..\..\features\C110_Two_Verbs_In_The_Step.md)  
   ### Scenario: [Detection of ambiguous use of multiple recognized verbs in the same Step](..\..\features\C110_Two_Verbs_In_The_Step.md): 
   - OK : Given the new file `too_much_verbs_in_step.md`  
   - OK : When I run `./bbt -c --yes too_much_verbs_in_step.md`  
   - OK : then the output contains   
   - [X] scenario   [Detection of ambiguous use of multiple recognized verbs in the same Step](..\..\features\C110_Two_Verbs_In_The_Step.md) pass  


# Document: [C120_Ill_Formated_Steps.md](..\..\features\C120_Ill_Formated_Steps.md)  
  ## Feature: bbt is providing helpful messages on ill formatted step lines  
   ### Scenario: [](..\..\features\C120_Ill_Formated_Steps.md): 
   - OK : Given the new file `bad_steps.md`  
   - OK : When I run `./bbt -k -c bad_steps.md`  
   - OK : Then the output contains   
   - OK : And the output contains   
   - OK : And the output contains  
   - OK : And I get an error  
   - [X] scenario   [](..\..\features\C120_Ill_Formated_Steps.md) pass  


## Summary : **Fail**

| Status     | Count |
|------------|-------|
| Failed     | 10    |
| Successful | 155   |
| Empty      | 0     |
| Not Run    | 6     |

