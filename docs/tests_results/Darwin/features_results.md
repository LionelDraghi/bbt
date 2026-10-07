
# Document: [A005_Given.md](../../features/A005_Given.md)  
  ## Feature:   
   ### Scenario: [Checking that there is no file or dir](../../features/A005_Given.md): 
   - OK : Given there is no dir `dir1`  
   - OK : Given there is no file `file1`  
   - OK : Then there is no dir `dir1`  
   - OK : Then there is no file `file1`  
   - [X] scenario   [Checking that there is no file or dir](../../features/A005_Given.md) pass  

   ### Scenario: [Checking that there is some dir](../../features/A005_Given.md): 
   - OK : Given the directory `dir2`  
   - OK : Then there is a dir `dir2`  
   - [X] scenario   [Checking that there is some dir](../../features/A005_Given.md) pass  

   ### Scenario: [An existing directory is kept as is](../../features/A005_Given.md): 
   - OK : Given the new directory `dir6`  
   - OK : Given the new file `dir6/f1` containing `some data`  
   - OK : Given the directory `dir6`  
   - OK : Then there is a `dir6` directory  
   - OK : And there is a `dir6/f1` file  
   - [X] scenario   [An existing directory is kept as is](../../features/A005_Given.md) pass  

   ### Scenario: [Checking that there is a file with some content](../../features/A005_Given.md): 
   - OK : Given there is no file `file3`  
   - OK : Given the file `file3`  
   - OK : Then file `file3` is `Hello World!`  
   - [X] scenario   [Checking that there is a file with some content](../../features/A005_Given.md) pass  

   ### Scenario: [Creating a file with some content](../../features/A005_Given.md): 
   - OK : Given the file `file4` containing `alpha`  
   - OK : Given the file `file5` containing   
   - OK : Then file `file4` is `alpha`  
   - OK : Then file `file5` is  
   - [X] scenario   [Creating a file with some content](../../features/A005_Given.md) pass  

   ### Scenario: [An existing file with the expected content is kept as is](../../features/A005_Given.md): 
   - OK : Given the new file `config.ini` containing `Tmp_dir=/tmp`  
   - OK : Given the new `keep_content.md` file  
   - OK : When I run `./bbt -q -c keep_content.md`  
   - OK : Then the output contains `## Summary : **Success**, 1 scenarios OK`  
   - OK : And there is a `config.ini` file  
   - [X] scenario   [An existing file with the expected content is kept as is](../../features/A005_Given.md) pass  

   ### Scenario: [An existing file with a different content is replaced with --yes](../../features/A005_Given.md): 
   - OK : Given the new file `config.ini` containing `Tmp_dir=/other`  
   - OK : Given the new `replace_yes.md` file  
   - OK : When I run `./bbt -q --yes replace_yes.md`  
   - OK : Then the output contains `## Summary : **Success**, 1 scenarios OK`  
   - OK : And there is a `config.ini` file  
   - [X] scenario   [An existing file with a different content is replaced with --yes](../../features/A005_Given.md) pass  

   ### Scenario: [Replacing confirmed by a typed key](../../features/A005_Given.md): 
   - OK : Given the new file `config.ini` containing `Tmp_dir=/other`  
   - OK : Given the new `replace_confirm.md` file  
   - OK : When I run `./bbt -q replace_confirm.md`  
   - OK : Then the output is  
   - OK : Then the output contains `## Summary : **Success**, 1 scenarios OK`  
   - OK : And there is a `config.ini` file  
   - OK : Then I get no error  
   - [X] scenario   [Replacing confirmed by a typed key](../../features/A005_Given.md) pass  

   ### Scenario: [Replacing refused by a typed key](../../features/A005_Given.md): 
   - OK : Given the new file `config.ini` containing `Tmp_dir=/other`  
   - OK : Given the new `replace_refuse.md` file  
   - OK : When I run `./bbt -q replace_refuse.md`  
   - OK : Then the output is  
   - OK : Then I get an error  
   - OK : And the output contains `file "config.ini" not overwritten`  
   - OK : And there is a `config.ini` file  
   - [X] scenario   [Replacing refused by a typed key](../../features/A005_Given.md) pass  

   ### Scenario: [The current match mode drives the comparison](../../features/A005_Given.md): 
   - OK : Given the new file `config.ini` containing `Tmp_dir=/tmp`  
   - OK : Given the new `match_mode.md` file  
   - OK : When I run `./bbt -q match_mode.md`  
   - OK : Then the output contains `## Summary : **Success**, 1 scenarios OK`  
   - OK : When I run `./bbt -q --exact_match match_mode.md`  
   - OK : Then the output is  
   - OK : Then I get an error  
   - OK : And the output contains `file "config.ini" not overwritten`  
   - [X] scenario   [The current match mode drives the comparison](../../features/A005_Given.md) pass  


# Document: [A006_Given_Executable_File.md](../../features/A006_Given_Executable_File.md)  
  ## Feature: running scripts (Unix_Only)  
   ### Scenario: [trying to run a script without the executable keyword should fail](../../features/A006_Given_Executable_File.md): 
   - OK : Given the new file `cmd1.sh` containing  
   - OK : Given the file `create_exec.md`   
   - OK : When I run `./bbt -c --yes create_exec.md`  
   - OK : Then I get an error  
   - OK : And  the output contains `create_exec.md:2: Error : ./cmd1.sh not executable`  
   - [X] scenario   [trying to run a script without the executable keyword should fail](../../features/A006_Given_Executable_File.md) pass  

   ### Scenario: [trying to run the same script created with the executable attribute should succeed](../../features/A006_Given_Executable_File.md): 
   - OK : Given the new executable file `cmd2.sh` containing  
   - OK : When I run `./cmd2.sh`  
   - OK : Then I get `bbt rules!`  
   - [X] scenario   [trying to run the same script created with the executable attribute should succeed](../../features/A006_Given_Executable_File.md) pass  


# Document: [A010_Then_Contains_Code_Span.md](../../features/A010_Then_Contains_Code_Span.md)  
  ## Feature: testing that a file contains a string  
   ### Scenario: [test on a string output](../../features/A010_Then_Contains_Code_Span.md): 
   - OK : When I run `./sut -v`  
   - OK : Then the output should contain `version`  
   - [X] scenario   [test on a string output](../../features/A010_Then_Contains_Code_Span.md) pass  

   ### Scenario: [test on a multiline output](../../features/A010_Then_Contains_Code_Span.md): 
   - OK : When I run `./sut -h`  
   - OK : Then the output should contains `--version`  
   - [X] scenario   [test on a multiline output](../../features/A010_Then_Contains_Code_Span.md) pass  

   ### Scenario: [test on a file](../../features/A010_Then_Contains_Code_Span.md): 
   - OK : Given the new file `config.ini`   
   - OK : Then `config.ini` contains `mode=silent`  
   - [X] scenario   [test on a file](../../features/A010_Then_Contains_Code_Span.md) pass  


# Document: [A020_Then_Contains_Code_Block.md](../../features/A020_Then_Contains_Code_Block.md)  
  ## Feature: "contains" a string feature  
   ### Scenario: [test the standard output](../../features/A020_Then_Contains_Code_Block.md): 
   - OK : Given the new `config.ini` file  
   - OK : And the `contains_multiline.md` file  
   - OK : Then `config.ini` contains  
   - OK : And `config.ini` contains   
   - OK : And `config.ini` contains   
   - OK : And `config.ini` contains   
   - [X] scenario   [test the standard output](../../features/A020_Then_Contains_Code_Block.md) pass  

   ### Scenario: [compare with an existing file](../../features/A020_Then_Contains_Code_Block.md): 
   - OK : When I run `./bbt contains_multiline.md`  
   - OK : Then I get an error  
   - OK : And output should contain    
   - [X] scenario   [compare with an existing file](../../features/A020_Then_Contains_Code_Block.md) pass  


# Document: [A030_Then_Does_Not_Contain.md](../../features/A030_Then_Does_Not_Contain.md)  
  ## Feature: checking that some string is not present in output or file  
   ### Background: [](../../features/A030_Then_Does_Not_Contain.md): 
   - OK : Given file `flowers.txt`  
   - [X] background [](../../features/A030_Then_Does_Not_Contain.md) pass  

   ### Scenario: [Successful checks](../../features/A030_Then_Does_Not_Contain.md): 
   - OK : When I run `./sut read flowers.txt`  
   - OK : Then output contains `Rose`  
   - OK : But output doesn't contain `Cactus`  
   - OK : Then file `flowers.txt` contains `Tulip`  
   - OK : But file `flowers.txt` do not contain `Eucalyptus`  
   - [X] scenario   [Successful checks](../../features/A030_Then_Does_Not_Contain.md) pass  

   ### Background: [](../../features/A030_Then_Does_Not_Contain.md): 
   - OK : Given file `flowers.txt`  
   - [X] background [](../../features/A030_Then_Does_Not_Contain.md) pass  

   ### Scenario: [Failed "output doesn't contain"](../../features/A030_Then_Does_Not_Contain.md): 
   - OK : Given the file `failed_doesnt_1.md`  
   - OK : When I run `./bbt failed_doesnt_1.md`  
   - OK : Then I get an error  
   - OK : And output contains   
   - OK : And output contains   
   - [X] scenario   [Failed "output doesn't contain"](../../features/A030_Then_Does_Not_Contain.md) pass  

   ### Background: [](../../features/A030_Then_Does_Not_Contain.md): 
   - OK : Given file `flowers.txt`  
   - [X] background [](../../features/A030_Then_Does_Not_Contain.md) pass  

   ### Scenario: [Failed "file doesn't contain"](../../features/A030_Then_Does_Not_Contain.md): 
   - OK : Given the file `failed_doesnt_2.md`  
   - OK : When I run `./bbt failed_doesnt_2.md`  
   - OK : Then I get an error  
   - OK : And output contains   
   - [X] scenario   [Failed "file doesn't contain"](../../features/A030_Then_Does_Not_Contain.md) pass  


# Document: [A040_Example_Keyword.md](../../features/A040_Example_Keyword.md)  
  ## Feature: The keyword `Example` is a synonym of the keyword `Scenario`.  
   ### Scenario: [NOK return code](../../features/A040_Example_Keyword.md): 
   - OK : When I run `./sut -qsdqsd`  
   - OK : Then I get an error  
   - [X] scenario   [NOK return code](../../features/A040_Example_Keyword.md) pass  

   ### Scenario: [NOK return code](../../features/A040_Example_Keyword.md): 
   - OK : When I run `./sut -qsdqsd`  
   - OK : Then I get an error  
   - [X] scenario   [NOK return code](../../features/A040_Example_Keyword.md) pass  


# Document: [A050_Then_File_Is_Code_Block.md](../../features/A050_Then_File_Is_Code_Block.md)  
  ## Feature: testing the "file is" feature  
   ### Scenario: [file is as expected](../../features/A050_Then_File_Is_Code_Block.md): 
   - OK : Given the `blaise_cendrars.txt` file  
   - OK : Then `blaise_cendrars.txt` is  
   - [X] scenario   [file is as expected](../../features/A050_Then_File_Is_Code_Block.md) pass  

   ### Scenario: [file is not as expected](../../features/A050_Then_File_Is_Code_Block.md): 
   - OK : Given the `file_is_code_fence.input` file  
   - OK : When running `./bbt file_is_code_fence.input`  
   - OK : Then I get an error  
   - [X] scenario   [file is not as expected](../../features/A050_Then_File_Is_Code_Block.md) pass  


# Document: [A060_Then_File_Equal_Code_Span.md](../../features/A060_Then_File_Equal_Code_Span.md)  
  ## Feature: "file is" followed by a code span (a string)  
   ### Scenario: [test on a single line file](../../features/A060_Then_File_Equal_Code_Span.md): 
   - OK : When I run `./sut create config.ini`  
   - OK : When I run `./sut append mode=silent config.ini`  
   - OK : Then file `config.ini` is equal to `mode=silent`  
   - OK : Then file `config.ini` is          `mode=silent`  
   - [X] scenario   [test on a single line file](../../features/A060_Then_File_Equal_Code_Span.md) pass  

   ### Scenario: [adding a second line to the file, so the same test should fail](../../features/A060_Then_File_Equal_Code_Span.md): 
   - OK : Given the new `tmp.md` file  
   - OK : When I run `./sut create config.ini`  
   - OK : When I run `./sut append mode=silent config.ini`  
   - OK : When I run `./sut append recurse=false config.ini`  
   - OK : When I run `./bbt tmp.md`  
   - OK : then I get an error  
   - [X] scenario   [adding a second line to the file, so the same test should fail](../../features/A060_Then_File_Equal_Code_Span.md) pass  


# Document: [A065_Then_File_Equal_File.md](../../features/A065_Then_File_Equal_File.md)  
  ## Feature: file is equal to a file  
   ### Scenario: [test `is equal to file` form](../../features/A065_Then_File_Equal_File.md): 
   - OK : Given the file `tmp.1`  
   - OK : And the file `tmp.2`  
   - OK : Then `tmp.1` is equal to file `tmp.2`  
   - [X] scenario   [test `is equal to file` form](../../features/A065_Then_File_Equal_File.md) pass  

   ### Scenario: [test `is equal to file` when files are *not* equal](../../features/A065_Then_File_Equal_File.md): 
   - OK : Given the file `tmp.3`  
   - OK : And the file `test_that_should_fail.md`  
   - OK : When I run `./bbt test_that_should_fail.md`  
   - OK : Then output contains `| Failed     |  1 `  
   - OK : And I get an error  
   - [X] scenario   [test `is equal to file` when files are *not* equal](../../features/A065_Then_File_Equal_File.md) pass  

   ### Scenario: [test the negative form `is not equal to file`](../../features/A065_Then_File_Equal_File.md): 
   - OK : Given the file `tmp.4`  
   - OK : Then `tmp.1` is not equal to file `tmp.4`  
   - OK : Then `tmp.4` is not equal to file `tmp.1`  
   - [X] scenario   [test the negative form `is not equal to file`](../../features/A065_Then_File_Equal_File.md) pass  


# Document: [A070_Then_Get_Code_Span.md](../../features/A070_Then_Get_Code_Span.md)  
  ## Feature: checking an expected multiline output  
   ### Scenario: [asking for sut help](../../features/A070_Then_Get_Code_Span.md): 
   - OK : When I run `./sut -h`  
   - OK : Then I get   
   - [X] scenario   [asking for sut help](../../features/A070_Then_Get_Code_Span.md) pass  

   ### Scenario: [causing an sut error with a long explanation](../../features/A070_Then_Get_Code_Span.md): 
   - OK : When I run `./sut -e append `  
   - OK : Then I get on stderr  
   - [X] scenario   [causing an sut error with a long explanation](../../features/A070_Then_Get_Code_Span.md) pass  


# Document: [A080_Then_Output_Equal_To_File.md](../../features/A080_Then_Output_Equal_To_File.md)  
  ## Feature: output is equal to a file  
   ### Scenario: [test `output is equal` keyword](../../features/A080_Then_Output_Equal_To_File.md): 
   - OK : Given the file `help_message.txt`  
   - OK : When I run `./sut -h`  
   - OK : Then output is equal to file `help_message.txt`  
   - [X] scenario   [test `output is equal` keyword](../../features/A080_Then_Output_Equal_To_File.md) pass  


# Document: [A090_Then_Get_Error.md](../../features/A090_Then_Get_Error.md)  
  ## Feature: return code test  
   ### Scenario: [NOK return code](../../features/A090_Then_Get_Error.md): 
   - OK : When I run `./sut -qsdqsd`  
   - OK : Then I get an error  
   - [X] scenario   [NOK return code](../../features/A090_Then_Get_Error.md) pass  

   ### Scenario: [OK return code](../../features/A090_Then_Get_Error.md): 
   - OK : When I run `./sut -v`  
   - OK : Then I get no error  
   - [X] scenario   [OK return code](../../features/A090_Then_Get_Error.md) pass  


# Document: [A100_Then_Get_Stderr.md](../../features/A100_Then_Get_Stderr.md)  
  ## Feature: stderr test  
   ### Scenario: [missing file name](../../features/A100_Then_Get_Stderr.md): 
   - OK : When I run `./sut create`  
   - OK : Then the error output is `Missing file name`  
   - [X] scenario   [missing file name](../../features/A100_Then_Get_Stderr.md) pass  


# Document: [A110_Then_No_Output.md](../../features/A110_Then_No_Output.md)  
  ## Feature: Check that there is no output  
   ### Scenario: [silent operation](../../features/A110_Then_No_Output.md): 
   - OK : Given the new file `file.txt` containing `text1`  
   - OK : When I run `./sut append text file.txt`  
   - OK : Then there is no output  
   - OK : And I get no error  
   - [X] scenario   [silent operation](../../features/A110_Then_No_Output.md) pass  

   ### Scenario: [silent operation expected, but there is an output](../../features/A110_Then_No_Output.md): 
   - OK : Given the new `file.txt` file containing `text2`  
   - OK : Given the new file `no_output.input`  
   - OK : When I run `./bbt -c no_output.input`  
   - OK : Then I get an error  
   - OK : And output contains `**NOK** : Then there is no output`  
   - OK : And output contains `output not null`  
   - [X] scenario   [silent operation expected, but there is an output](../../features/A110_Then_No_Output.md) pass  


# Document: [A120_Then_Get_String.md](../../features/A120_Then_Get_String.md)  
  ## Feature: checking a message line on stdout  
   ### Scenario: [asking for sut version](../../features/A120_Then_Get_String.md): 
   - OK : When I run `./sut -v`  
   - OK : Then I get `sut version 1.0`  
   - [X] scenario   [asking for sut version](../../features/A120_Then_Get_String.md) pass  


# Document: [A130_Successfully_Keyword.md](../../features/A130_Successfully_Keyword.md)  
  ## Feature: The “successfully” shortcut  
   ### Scenario: [*when I successfully run* a command with successful run](../../features/A130_Successfully_Keyword.md): 
   - OK : When I successfully run `./sut --version`  
   - OK : Then I get `sut version 1.0`  
   - [X] scenario   [*when I successfully run* a command with successful run](../../features/A130_Successfully_Keyword.md) pass  

   ### Scenario: [*when I successfully run* a command with a wrong command line, returns an error status](../../features/A130_Successfully_Keyword.md): 
   - OK : Given the `vza.input` file  
   - OK : When I run `./bbt vza.input`  
   - OK : Then I get an error  
   - [X] scenario   [*when I successfully run* a command with a wrong command line, returns an error status](../../features/A130_Successfully_Keyword.md) pass  

   ### Scenario: [*when I run* a command with a wrong command line](../../features/A130_Successfully_Keyword.md): 
   - OK : When I run `./sut -vza`  
   - OK : Then I get `unknown option -vza`  
   - [X] scenario   [*when I run* a command with a wrong command line](../../features/A130_Successfully_Keyword.md) pass  

   ### Scenario: [*when I successfully run* an interactive command that finally exits cleanly](../../features/A130_Successfully_Keyword.md): 
   - OK : Given the new file `to_delete.txt` containing `some data`  
   - OK : Then the output is `Deleting to_delete.txt`  
   - OK : When I successfully run `./sut delete to_delete.txt`  
   - OK : And there is no file `to_delete.txt`  
   - [X] scenario   [*when I successfully run* an interactive command that finally exits cleanly](../../features/A130_Successfully_Keyword.md) pass  

   ### Scenario: [*when I successfully run* an interactive command that finally fails](../../features/A130_Successfully_Keyword.md): 
   - OK : Given the new `interactive_fail.md` file  
   - OK : When I run `./bbt -q -c --yes interactive_fail.md`  
   - OK : Then I get an error  
   - OK : And the output contains `Unsuccessfully run "./sut delete to_keep.txt"`  
   - [X] scenario   [*when I successfully run* an interactive command that finally fails](../../features/A130_Successfully_Keyword.md) pass  

   ### Scenario: [the deferred failure is reported before the next step](../../features/A130_Successfully_Keyword.md): 
   - OK : Given the new `deferred_fail.md` file  
   - OK : When I run `./bbt -q -c --yes deferred_fail.md`  
   - OK : Then I get an error  
   - OK : And the output contains `Unsuccessfully run "./sut delete to_keep.txt"`  
   - OK : And the output does not contain `no command is running`  
   - [X] scenario   [the deferred failure is reported before the next step](../../features/A130_Successfully_Keyword.md) pass  

   ### Scenario: [successfully run, then an explicit no error check](../../features/A130_Successfully_Keyword.md): 
   - OK : Given the new file `to_delete.txt` containing `some data`  
   - OK : When I successfully run `./sut delete to_delete.txt`  
   - OK : Then I get no error  
   - [X] scenario   [successfully run, then an explicit no error check](../../features/A130_Successfully_Keyword.md) pass  


# Document: [A140_Unordered_Keyword.md](../../features/A140_Unordered_Keyword.md)  
  ## Feature: when the modifier `unordered` is given after `get`, order of line is ignored  
   ### Background: [](../../features/A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](../../features/A140_Unordered_Keyword.md) pass  

   ### Scenario: ["I get" without the modifier](../../features/A140_Unordered_Keyword.md): 
   - OK : Given the file `scenario1.md`  
   - OK : When I run `./bbt scenario1.md`  
   - OK : Then I get an error  
   - [X] scenario   ["I get" without the modifier](../../features/A140_Unordered_Keyword.md) pass  

   ### Background: [](../../features/A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](../../features/A140_Unordered_Keyword.md) pass  

   ### Scenario: [same "I get" with the modifier](../../features/A140_Unordered_Keyword.md): 
   - OK : When I run `./sut read flowers1.txt`  
   - OK : Then I get file (unordered) `flowers2.txt`  
   - [X] scenario   [same "I get" with the modifier](../../features/A140_Unordered_Keyword.md) pass  

   ### Background: [](../../features/A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](../../features/A140_Unordered_Keyword.md) pass  

   ### Scenario: ["contains" without the modifier](../../features/A140_Unordered_Keyword.md): 
   - OK : Given the file `scenario1.md`  
   - OK : When I run `./bbt scenario1.md`  
   - OK : Then I get an error  
   - [X] scenario   ["contains" without the modifier](../../features/A140_Unordered_Keyword.md) pass  

   ### Background: [](../../features/A140_Unordered_Keyword.md): 
   - OK : Given the file `flowers1.txt`  
   - OK : Given the file `flowers2.txt`  
   - OK : Given the file `flowers3.txt`  
   - [X] background [](../../features/A140_Unordered_Keyword.md) pass  

   ### Scenario: [same "contains" but with the "unordered" modifier](../../features/A140_Unordered_Keyword.md): 
   - OK : When I run `./sut read flowers1.txt`  
   - OK : Then output contains file `flowers3.txt` (unordered)  
   - [X] scenario   [same "contains" but with the "unordered" modifier](../../features/A140_Unordered_Keyword.md) pass  


# Document: [A150_Background_Keyword.md](../../features/A150_Background_Keyword.md)  
  ## Feature: Feature 1  
   ### Background: [Background1](../../features/A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](../../features/A150_Background_Keyword.md) pass  

   ### Background: [Background2 in feature 1](../../features/A150_Background_Keyword.md): 
   - OK : Given the file `dir1/file2`  
   - [X] background [Background2 in feature 1](../../features/A150_Background_Keyword.md) pass  

   ### Scenario: [lets erase what was created by previous background runs](../../features/A150_Background_Keyword.md): 
   - OK : Given there is no `dir1` dir  
   - [X] scenario   [lets erase what was created by previous background runs](../../features/A150_Background_Keyword.md) pass  

   ### Background: [Background1](../../features/A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](../../features/A150_Background_Keyword.md) pass  

   ### Background: [Background2 in feature 1](../../features/A150_Background_Keyword.md): 
   - OK : Given the file `dir1/file2`  
   - [X] background [Background2 in feature 1](../../features/A150_Background_Keyword.md) pass  

   ### Scenario: [Two Background executed](../../features/A150_Background_Keyword.md): 
   - OK : then there is a `dir1/file1` file  
   - OK : then there is a `dir1/file2` file  
   - [X] scenario   [Two Background executed](../../features/A150_Background_Keyword.md) pass  

  ## Feature: Feature 2  
   ### Background: [Background1](../../features/A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](../../features/A150_Background_Keyword.md) pass  

   ### Scenario: [lets erase once more what was created by previous background runs](../../features/A150_Background_Keyword.md): 
   - OK : Given there is no `dir1` dir  
   - [X] scenario   [lets erase once more what was created by previous background runs](../../features/A150_Background_Keyword.md) pass  

   ### Background: [Background1](../../features/A150_Background_Keyword.md): 
   - OK : Given the new dir `dir1`  
   - OK : Given the file `dir1/file1`  
   - [X] background [Background1](../../features/A150_Background_Keyword.md) pass  

   ### Scenario: [only first background should apply](../../features/A150_Background_Keyword.md): 
   - OK : then there is a `dir1/file1` file  
   - OK : then there is no `dir1/file2` file  
   - [X] scenario   [only first background should apply](../../features/A150_Background_Keyword.md) pass  


# Document: [A160_Ignoring_Blank_Lines.md](../../features/A160_Ignoring_Blank_Lines.md)  
  ## Feature: Ignoring blank lines  
   ### Background: [](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [some obvious tests](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : Then file `le_Cid_1.txt` is equal to file `le_Cid_1.txt`  
   - OK : Then file `le_Cid_1.txt` contains file `le_Cid_1.txt`  
   - [X] scenario   [some obvious tests](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Background: [](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [default behavior, non sensible to blank lines](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : when I run `./bbt is.md`  
   - OK : then there is no error   
   - OK : when I run `./bbt contains.md`  
   - OK : then there is no error   
   - [X] scenario   [default behavior, non sensible to blank lines](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Background: [](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [with --exact_match, sensible to blank lines](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : when I run `./bbt -em is.md`  
   - OK : then there is an error   
   - OK : and output contains `Error : le_Cid_1.txt not equal to expected:`  
   - OK : when I run `./bbt -em contains.md`  
   - OK : then there is an error   
   - OK : and output contains `Error : le_Cid_1.txt does not contain expected:`  
   - [X] scenario   [with --exact_match, sensible to blank lines](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Background: [](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [with --exact_match and --ignore_blank_lines, non sensible to blank lines 1/2](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : when I run `./bbt -em -ibl is.md`  
   - OK : then there is no error   
   - [X] scenario   [with --exact_match and --ignore_blank_lines, non sensible to blank lines 1/2](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Background: [](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : Given the file `le_Cid_1.txt`  
   - OK : Given the file `le_Cid_2.txt`   
   - OK : Given the file `le_Cid_3.txt`   
   - OK : Given the file `is.md`  
   - OK : Given the file `contains.md`  
   - [X] background [](../../features/A160_Ignoring_Blank_Lines.md) pass  

   ### Scenario: [with --exact_match and --ignore_blank_lines, non sensible to blank lines 2/2](../../features/A160_Ignoring_Blank_Lines.md): 
   - OK : when I run `./bbt --exact_match --ignore_blank_lines contains.md`  
   - OK : then there is no error   
   - [X] scenario   [with --exact_match and --ignore_blank_lines, non sensible to blank lines 2/2](../../features/A160_Ignoring_Blank_Lines.md) pass  


# Document: [A170_File_vs_File_Name.md](../../features/A170_File_vs_File_Name.md)  
  ## Feature: expected content in a file  
   ### Scenario: ["Then I get file" form](../../features/A170_File_vs_File_Name.md): 
   - OK : Given the file `list_1.txt`  
   - OK : Given the file `list_2.txt`  
   - OK : When I run `./sut read list_1.txt`  
   - OK : Then I get file `list_1.txt`  
   - [X] scenario   ["Then I get file" form](../../features/A170_File_vs_File_Name.md) pass  

   ### Scenario: ["Then output is | contains | does not contain file" form](../../features/A170_File_vs_File_Name.md): 
   - OK : When I run `./sut read list_1.txt`  
   - OK : Then output is the `list_1.txt` file  
   - OK : Then output contains file `list_1.txt`  
   - OK : Then output does not contain file `list_2.txt`  
   - [X] scenario   ["Then output is | contains | does not contain file" form](../../features/A170_File_vs_File_Name.md) pass  

   ### Scenario: ["Then file is | contains | does not contain file" form](../../features/A170_File_vs_File_Name.md): 
   - OK : Then `list_2.txt` contains file `list_1.txt`  
   - OK : Then `list_1.txt` does not contain file `list_2.txt`  
   - [X] scenario   ["Then file is | contains | does not contain file" form](../../features/A170_File_vs_File_Name.md) pass  


# Document: [A190_Run.md](../../features/A190_Run.md)  
  ## Feature:   
   ### Scenario: [the command relative path is given](../../features/A190_Run.md): 
   - OK : when i run `./bbt lf`  
   - OK : then I get no error  
   - OK : and  I get no output  
   - [X] scenario   [the command relative path is given](../../features/A190_Run.md) pass  

   ### Scenario: [the command is in the PATH](../../features/A190_Run.md): 
   - OK : when i run `git --version`  
   - OK : then I get no error  
   - OK : and  output contains `git version`  
   - [X] scenario   [the command is in the PATH](../../features/A190_Run.md) pass  

   ### Scenario: [command not found](../../features/A190_Run.md): 
   - OK : Given the `cmd_not_found.md` file   
   - OK : When I run `./bbt -c cmd_not_found.md`  
   - OK : Then I get an error  
   - OK : And  the output contains `cmd_not_found.md:2: Error : xyzabc not found`  
   - [X] scenario   [command not found](../../features/A190_Run.md) pass  

   ### Scenario: [the command is not executable (Unix_Only)](../../features/A190_Run.md): 
   - OK : Given the `lambda_file` containing `nothing`  
   - OK : Given the `cmd_not_exe.md` file   
   - OK : When I run `./bbt -c cmd_not_exe.md`  
   - OK : Then I get an error  
   - OK : And  the output contains `cmd_not_exe.md:2: Error : ./lambda_file not executable`  
   - [X] scenario   [the command is not executable (Unix_Only)](../../features/A190_Run.md) pass  


# Document: [A195_Run_Or.md](../../features/A195_Run_Or.md)  
  ## Feature: “when I run X or Y” test  
   ### Scenario: [normal use case](../../features/A195_Run_Or.md): 
   - OK : Given the new file `simple_or.md`   
   - OK : When I run `./bbt simple_or.md`  
   - OK : Then I get no error  
   - OK : And the output contains  
   - [X] scenario   [normal use case](../../features/A195_Run_Or.md) pass  

   ### Scenario: [The last command does not meet expectation (test should fail)](../../features/A195_Run_Or.md): 
   - OK : Given the new file `erroneous_or.md`   
   - OK : When I run `./bbt -c erroneous_or.md`  
   - OK : Then I get an error  
   - OK : And the output contains  
   - [X] scenario   [The last command does not meet expectation (test should fail)](../../features/A195_Run_Or.md) pass  

  ## Feature: Syntactic and Semantic error related to "or" should be detected  
   ### Scenario: [Missing command before "or"](../../features/A195_Run_Or.md): 
   - OK : Given the new file `erroneous1.md`   
   - OK : When I run `./bbt -c erroneous1.md`  
   - OK : Then I get no error  
   - OK : And the output contains  
   - [X] scenario   [Missing command before "or"](../../features/A195_Run_Or.md) pass  

   ### Scenario: [Missing command after "or"](../../features/A195_Run_Or.md): 
   - OK : Given the new file `erroneous2.md`   
   - OK : When I run `./bbt -c erroneous2.md`  
   - OK : Then I get an error  
   - OK : And the output contains  
   - [X] scenario   [Missing command after "or"](../../features/A195_Run_Or.md) pass  

   ### Scenario: [Two consecutive "or"](../../features/A195_Run_Or.md): 
   - OK : Given the new file `erroneous3.md`   
   - OK : When I run `./bbt -c erroneous3.md`  
   - OK : Then I get no error  
   - OK : And the output contains  
   - [X] scenario   [Two consecutive "or"](../../features/A195_Run_Or.md) pass  


# Document: [A200_Regexp.md](../../features/A200_Regexp.md)  
  ## Feature: identifying expected output with regexp  
   ### Scenario: [version number match](../../features/A200_Regexp.md): 
   - OK : When I run `./sut -v`  
   - OK : Then output matches `sut version [0-9]+\.[0-9]+`  
   - [X] scenario   [version number match](../../features/A200_Regexp.md) pass  

   ### Scenario: [version number mismatch](../../features/A200_Regexp.md): 
   - OK : Given the new file `wrong_regexp.md`  
   - OK : When I run `./bbt wrong_regexp.md`  
   - OK : Then I get an error  
   - OK : and output contains   
   - [X] scenario   [version number mismatch](../../features/A200_Regexp.md) pass  

   ### Scenario: [Test of "output does not match"](../../features/A200_Regexp.md): 
   - OK : When I run `./sut -v`  
   - OK : Then output does not match `sut version [0-9]+\.[0-9]+\.[0-9]+`  
   - [X] scenario   [Test of "output does not match"](../../features/A200_Regexp.md) pass  

   ### Scenario: [Test of "output does not match" that indeed matches](../../features/A200_Regexp.md): 
   - OK : Given the new file `wrong_regexp.md`  
   - OK : When I run `./bbt wrong_regexp.md`  
   - OK : Then I get an error  
   - OK : and output contains   
   - [X] scenario   [Test of "output does not match" that indeed matches](../../features/A200_Regexp.md) pass  

   ### Scenario: [test of "file match"](../../features/A200_Regexp.md): 
   - OK : Given the new file `Cities.txt`  
   - OK : Then `Cities.txt` match `.*ehera.*`  
   - OK : And  `Cities.txt` does not match `.*York.*`  
   - [X] scenario   [test of "file match"](../../features/A200_Regexp.md) pass  


# Document: [A210_Exact_Match.md](../../features/A210_Exact_Match.md)  
   ### Background: [](../../features/A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - OK : Given the `compare_crlf.md` file  
   - OK : Given I successfully run `./sut cr_lf text.4`  
   - [X] background [](../../features/A210_Exact_Match.md) pass  

   ### Scenario: [Human match](../../features/A210_Exact_Match.md): 
   - OK : When I run `./bbt compare.md`  
   - OK : Then I get no error  
   - OK : When I run `./bbt --human_match compare.md`  
   - OK : Then I get no error  
   - [X] scenario   [Human match](../../features/A210_Exact_Match.md) pass  

   ### Background: [](../../features/A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - OK : Given the `compare_crlf.md` file  
   - OK : Given I successfully run `./sut cr_lf text.4`  
   - [X] background [](../../features/A210_Exact_Match.md) pass  

   ### Scenario: [exact match](../../features/A210_Exact_Match.md): 
   - OK : When I run `./bbt -k --exact_match compare.md`  
   - OK : Then I get an error  
   - OK : And output contains  
   - OK : And output contains  
   - OK : And output contains  
   - [X] scenario   [exact match](../../features/A210_Exact_Match.md) pass  

   ### Background: [](../../features/A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - OK : Given the `compare_crlf.md` file  
   - OK : Given I successfully run `./sut cr_lf text.4`  
   - [X] background [](../../features/A210_Exact_Match.md) pass  

   ### Scenario: [exact match except for casing](../../features/A210_Exact_Match.md): 
   - OK : When I run `./bbt -k -em -ic compare.md`  
   - OK : Then I get an error  
   - OK : And output contains  
   - OK : And output contains  
   - OK : And output contains  
   - [X] scenario   [exact match except for casing](../../features/A210_Exact_Match.md) pass  

   ### Background: [](../../features/A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - OK : Given the `compare_crlf.md` file  
   - OK : Given I successfully run `./sut cr_lf text.4`  
   - [X] background [](../../features/A210_Exact_Match.md) pass  

   ### Scenario: [exact match except for casing and blank lines](../../features/A210_Exact_Match.md): 
   - OK : When I run `./bbt -k -em --ignore_casing -ibl compare.md`  
   - OK : Then I get an error  
   - OK : And output contains  
   - OK : And output contains  
   - OK : And output contains  
   - [X] scenario   [exact match except for casing and blank lines](../../features/A210_Exact_Match.md) pass  

   ### Background: [](../../features/A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - OK : Given the `compare_crlf.md` file  
   - OK : Given I successfully run `./sut cr_lf text.4`  
   - [X] background [](../../features/A210_Exact_Match.md) pass  

   ### Scenario: [human match ignores CR at line ends](../../features/A210_Exact_Match.md): 
   - OK : When I run `./bbt compare_crlf.md`  
   - OK : Then I get no error  
   - [X] scenario   [human match ignores CR at line ends](../../features/A210_Exact_Match.md) pass  

   ### Background: [](../../features/A210_Exact_Match.md): 
   - OK : Given the file `text.ref`  
   - OK : Given the file `text.1`  
   - OK : Given the file `text.2`  
   - OK : Given the file `text.3`  
   - OK : Given the `compare.md` file  
   - OK : Given the `compare_crlf.md` file  
   - OK : Given I successfully run `./sut cr_lf text.4`  
   - [X] background [](../../features/A210_Exact_Match.md) pass  

   ### Scenario: [exact match is sensitive to CR at line ends](../../features/A210_Exact_Match.md): 
   - OK : When I run `./bbt -k --exact_match compare_crlf.md`  
   - OK : Then I get an error  
   - OK : And output contains  
   - [X] scenario   [exact match is sensitive to CR at line ends](../../features/A210_Exact_Match.md) pass  


# Document: [A220_AsciiDoc_gcc_hello_world.adoc](../../features/A220_AsciiDoc_gcc_hello_world.adoc)  
   ### Scenario: [gcc version?](../../features/A220_AsciiDoc_gcc_hello_world.adoc): 
   - OK : When I run `gcc -v`  
   - OK : Then the output contains `version `  
   - OK : Then the output matches `(gcc|.* clang) version [0-9]+\.[0-9]+\.[0-9]+ .*`  
   - [X] scenario   [gcc version?](../../features/A220_AsciiDoc_gcc_hello_world.adoc) pass  

   ### Scenario: [compiling and executing an hello word](../../features/A220_AsciiDoc_gcc_hello_world.adoc): 
   - OK : Given the new file `main.c`  
   - OK : And given there is no `main` file  
   - OK : When I successfully run `gcc main.c -o main`  
   - OK : And  I run `./main`  
   - OK : Then the output is `Hello, World!`  
   - [X] scenario   [compiling and executing an hello word](../../features/A220_AsciiDoc_gcc_hello_world.adoc) pass  


# Document: [A230_select_exclude_include.md](../../features/A230_select_exclude_include.md)  
  ## Feature: Filter  
   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [no filtering](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt suite_1.md`    
   - OK : Then file `output_1.txt` is  
   - [X] scenario   [no filtering](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [step filtering](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt --exclude Windows suite_1.md`    
   - OK : Then file `output_1.txt` is  
   - [X] scenario   [step filtering](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [step selection](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select create suite_1.md`    
   - OK : Then file `output_1.txt` contains  
   - [X] scenario   [step selection](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [Selecting a scenario](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select Robustness suite_1.md suite_2.md`    
   - OK : Then there is no file `output_1.txt`  
   - OK : And `output_2.txt` is  
   - [X] scenario   [Selecting a scenario](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [selection is empty](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select xzscskfjhs suite_1.md suite_2.md`    
   - OK : Then there is no file `output_1.txt`  
   - OK : And  there is no file `output_2.txt`  
   - [X] scenario   [selection is empty](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [Selecting a Background only](../../features/A230_select_exclude_include.md): 
   - OK : Given the file `background.md`  
   - OK : When I run `./bbt --select new_file background.md`    
   - OK : Then there is no file `input.txt`  
   - OK : And  there is no file `output_2.txt`  
   - [X] scenario   [Selecting a Background only](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [Select followed by an exclude](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt --select Robustness --exclude Linux suite_2.md`  
   - OK : Then `output_2.txt` is  
   - [X] scenario   [Select followed by an exclude](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [scenario Excluded followed by an include of a step inside](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt --exclude Robustness --include create --include Linux suite_2.md`    
   - OK : Then `output_2.txt` is  
   - [X] scenario   [scenario Excluded followed by an include of a step inside](../../features/A230_select_exclude_include.md) pass  

   ### Background: [Lets cleanup the place](../../features/A230_select_exclude_include.md): 
   - OK : Given there is no file `output_1.txt`  
   - OK : Given there is no file `output_2.txt`  
   - OK : Given there is no file `input.txt`  
   - OK : Given the file `suite_1.md`  
   - OK : Given the file `suite_2.md`  
   - [X] background [Lets cleanup the place](../../features/A230_select_exclude_include.md) pass  

   ### Scenario: [Exclude by file name](../../features/A230_select_exclude_include.md): 
   - OK : When I run `./bbt --exclude suite_2.md suite_2.md`    
   - OK : Then there is no `output_2.txt` file  
   - [X] scenario   [Exclude by file name](../../features/A230_select_exclude_include.md) pass  


# Document: [A240_Then_I_Succesfully_Run.md](../../features/A240_Then_I_Succesfully_Run.md)  
   ### Scenario: [Running something in postcondition](../../features/A240_Then_I_Succesfully_Run.md): 
   - OK : Then I successfully run `./sut --help`   
   - [X] scenario   [Running something in postcondition](../../features/A240_Then_I_Succesfully_Run.md) pass  


# Document: [A250_Fails.md](../../features/A250_Fails.md)  
   ### Scenario: [Checking that a command fails](../../features/A250_Fails.md): 
   - OK : Then `./sut -zwq` fails   
   - OK : And  the output is `unknown option -zwq`  
   - OK : And  `./sut -zwq` should fail   
   - OK : And  the output is `unknown option -zwq`  
   - [X] scenario   [Checking that a command fails](../../features/A250_Fails.md) pass  


# Document: [A260_Then_Exit_Code_Is.md](../../features/A260_Then_Exit_Code_Is.md)  
  ## Feature: exact exit code  
   ### Scenario: [exit code of a successful command](../../features/A260_Then_Exit_Code_Is.md): 
   - OK : When I run `./sut -v`  
   - OK : Then the exit code is `0`  
   - OK : And I get no error  
   - [X] scenario   [exit code of a successful command](../../features/A260_Then_Exit_Code_Is.md) pass  

   ### Scenario: [exit code of a failing command](../../features/A260_Then_Exit_Code_Is.md): 
   - OK : When I run `./sut delay 0 3`  
   - OK : Then the exit code is `3`  
   - OK : And I get an error  
   - [X] scenario   [exit code of a failing command](../../features/A260_Then_Exit_Code_Is.md) pass  

   ### Scenario: [the exit code of the last command is checked](../../features/A260_Then_Exit_Code_Is.md): 
   - OK : When I run `./sut delay 0 4`  
   - OK : And I run `./sut -v`  
   - OK : Then the exit code is `0`  
   - [X] scenario   [the exit code of the last command is checked](../../features/A260_Then_Exit_Code_Is.md) pass  

   ### Scenario: [wrong exit code](../../features/A260_Then_Exit_Code_Is.md): 
   - OK : Given the new `wrong_exit_code.md` file  
   - OK : When I run `./bbt wrong_exit_code.md`  
   - OK : Then I get an error  
   - OK : And the output contains `Expected exit code 2, got 3`  
   - [X] scenario   [wrong exit code](../../features/A260_Then_Exit_Code_Is.md) pass  

   ### Scenario: [the code must be an integer](../../features/A260_Then_Exit_Code_Is.md): 
   - OK : Given the new `not_a_code.md` file  
   - OK : When I run `./bbt not_a_code.md`  
   - OK : Then I get an error  
   - OK : And the output contains `Exit code expected in object phrase, got "zero"`  
   - [X] scenario   [the code must be an integer](../../features/A260_Then_Exit_Code_Is.md) pass  


# Document: [A270_Then_Error_Output.md](../../features/A270_Then_Error_Output.md)  
  ## Feature: error output  
   ### Scenario: [error message on the error output](../../features/A270_Then_Error_Output.md): 
   - OK : When I run `./sut create`  
   - OK : Then the error output is `Missing file name`  
   - OK : And there is no output  
   - [X] scenario   [error message on the error output](../../features/A270_Then_Error_Output.md) pass  

   ### Scenario: [nothing on the error output](../../features/A270_Then_Error_Output.md): 
   - OK : When I run `./sut -v`  
   - OK : Then there is no error output  
   - OK : And the output is `sut version 1.0`  
   - [X] scenario   [nothing on the error output](../../features/A270_Then_Error_Output.md) pass  

   ### Scenario: [error output contains and does not contain](../../features/A270_Then_Error_Output.md): 
   - OK : When I run `./sut read_env BBT_SURELY_UNSET_VARIABLE`  
   - OK : Then the error output contains `BBT_SURELY_UNSET_VARIABLE`  
   - OK : And the error output does not contain `version`  
   - OK : When I run `./sut -v`  
   - OK : Then I get no error output  
   - [X] scenario   [error output contains and does not contain](../../features/A270_Then_Error_Output.md) pass  

   ### Scenario: [error output in a code block](../../features/A270_Then_Error_Output.md): 
   - OK : When I run `./sut read_env BBT_SURELY_UNSET_VARIABLE`  
   - OK : Then the error output is  
   - [X] scenario   [error output in a code block](../../features/A270_Then_Error_Output.md) pass  

   ### Scenario: [without error output step, the outputs stay merged](../../features/A270_Then_Error_Output.md): 
   - OK : When I run `./sut create`  
   - OK : Then the output is `Missing file name`  
   - [X] scenario   [without error output step, the outputs stay merged](../../features/A270_Then_Error_Output.md) pass  

   ### Scenario: [wrong error output](../../features/A270_Then_Error_Output.md): 
   - OK : Given the new `wrong_error_output.md` file  
   - OK : When I run `./bbt wrong_error_output.md`  
   - OK : Then I get an error  
   - [X] scenario   [wrong error output](../../features/A270_Then_Error_Output.md) pass  


# Document: [A280_Given_Environment_Variable.md](../../features/A280_Given_Environment_Variable.md)  
  ## Feature: environment variables  
   ### Scenario: [set a variable](../../features/A280_Given_Environment_Variable.md): 
   - OK : Given the environment variable `BBT_TEST_VARIABLE` is `hello`  
   - OK : When I run `./sut read_env BBT_TEST_VARIABLE`  
   - OK : Then the output is `hello`  
   - [X] scenario   [set a variable](../../features/A280_Given_Environment_Variable.md) pass  

   ### Scenario: [the variable is back to its previous state](../../features/A280_Given_Environment_Variable.md): 
   - OK : When I run `./sut read_env BBT_TEST_VARIABLE`  
   - OK : Then I get an error  
   - OK : And the output contains `No BBT_TEST_VARIABLE environment variable`  
   - [X] scenario   [the variable is back to its previous state](../../features/A280_Given_Environment_Variable.md) pass  

   ### Scenario: [unset a variable](../../features/A280_Given_Environment_Variable.md): 
   - OK : Given the environment variable `PATH` is not set  
   - OK : When I run `./sut read_env PATH`  
   - OK : Then I get an error  
   - OK : And the output contains `No PATH environment variable`  
   - [X] scenario   [unset a variable](../../features/A280_Given_Environment_Variable.md) pass  

   ### Scenario: [the unset variable is back](../../features/A280_Given_Environment_Variable.md): 
   - OK : When I run `./sut read_env PATH`  
   - OK : Then I get no error  
   - [X] scenario   [the unset variable is back](../../features/A280_Given_Environment_Variable.md) pass  

   ### Scenario: [set twice in the same scenario](../../features/A280_Given_Environment_Variable.md): 
   - OK : Given the environment variable `BBT_TEST_VARIABLE` is `first`  
   - OK : And the environment variable `BBT_TEST_VARIABLE` is `second`  
   - OK : When I run `./sut read_env BBT_TEST_VARIABLE`  
   - OK : Then the output is `second`  
   - [X] scenario   [set twice in the same scenario](../../features/A280_Given_Environment_Variable.md) pass  

   ### Scenario: [variable set in a background](../../features/A280_Given_Environment_Variable.md): 
   - OK : Given the new `env_background.md` file  
   - OK : When I run `./bbt env_background.md`  
   - OK : Then I get no error  
   - [X] scenario   [variable set in a background](../../features/A280_Given_Environment_Variable.md) pass  


# Document: [A290_When_I_Type_Or_Enter.md](../../features/A290_When_I_Type_Or_Enter.md)  
  ## Feature: interaction with the command under test  
   ### Scenario: [answering a key prompt with type](../../features/A290_When_I_Type_Or_Enter.md): 
   - OK : Given the new file `to_delete.txt` containing `some data`  
   - OK : When I run `./sut delete to_delete.txt`  
   - OK : Then the output is  
   - OK : Then the output is `Deleting to_delete.txt`  
   - OK : And there is no file `to_delete.txt`  
   - [X] scenario   [answering a key prompt with type](../../features/A290_When_I_Type_Or_Enter.md) pass  

   ### Scenario: [refusing with type](../../features/A290_When_I_Type_Or_Enter.md): 
   - OK : Given the new file `to_keep.txt` containing `some data`  
   - OK : When I run `./sut delete to_keep.txt`  
   - OK : Then there is no output  
   - OK : And there is a file `to_keep.txt`  
   - [X] scenario   [refusing with type](../../features/A290_When_I_Type_Or_Enter.md) pass  

   ### Scenario: [several inputs in a row](../../features/A290_When_I_Type_Or_Enter.md): 
   - OK : Given the new file `to_delete.txt` containing `some data`  
   - OK : When I run `./sut delete to_delete.txt`  
   - OK : Then the output is `Deleting to_delete.txt`  
   - OK : And there is no file `to_delete.txt`  
   - [X] scenario   [several inputs in a row](../../features/A290_When_I_Type_Or_Enter.md) pass  

   ### Scenario: [answering a line prompt with enter](../../features/A290_When_I_Type_Or_Enter.md): 
   - OK : Given there is no `new_name.txt` file  
   - OK : Given the new file `old_name.txt` containing `some data`  
   - OK : When I run `./sut rename old_name.txt`  
   - OK : Then the output is  
   - OK : Then the output is `Renamed to new_name.txt`  
   - OK : And there is no file `old_name.txt`  
   - OK : And there is a file `new_name.txt`  
   - [X] scenario   [answering a line prompt with enter](../../features/A290_When_I_Type_Or_Enter.md) pass  

   ### Scenario: [enter without a running command](../../features/A290_When_I_Type_Or_Enter.md): 
   - OK : Given the `no_input.md` file  
   - OK : When I run `./bbt -c no_input.md`  
   - OK : Then I get an error  
   - OK : And the output contains `no_input.md:2: Error : no command is running when reaching this step`  
   - [X] scenario   [enter without a running command](../../features/A290_When_I_Type_Or_Enter.md) pass  


# Document: [B010_Deleting_created_files.md](../../features/B010_Deleting_created_files.md)  
  ## Feature: tmp files and directories deletion  
   ### Background: [](../../features/B010_Deleting_created_files.md): 
   - OK : Given there is no dir `dir1`  
   - OK : Given there is no dir `dir3`  
   - OK : Given there is no file `f1`  
   - OK : Given the new `create_tree.md` file  
   - [X] background [](../../features/B010_Deleting_created_files.md) pass  

   ### Scenario: [run without --cleanup](../../features/B010_Deleting_created_files.md): 
   - OK : When I run `./bbt --yes create_tree.md`  
   - OK : Then there is a `dir1/dir2/f2` file  
   - OK : Then there is a `dir3/dir4/dir5` dir  
   - OK : And  there is a `f1` file  
   - [X] scenario   [run without --cleanup](../../features/B010_Deleting_created_files.md) pass  

   ### Background: [](../../features/B010_Deleting_created_files.md): 
   - OK : Given there is no dir `dir1`  
   - OK : Given there is no dir `dir3`  
   - OK : Given there is no file `f1`  
   - OK : Given the new `create_tree.md` file  
   - [X] background [](../../features/B010_Deleting_created_files.md) pass  

   ### Scenario: [run with --cleanup](../../features/B010_Deleting_created_files.md): 
   - OK : When I run `./bbt --cleanup create_tree.md`  
   - OK : Then there is no `dir1` directory  
   - OK : And  there is no `dir3` directory  
   - OK : And  there is no `f1` file  
   - [X] scenario   [run with --cleanup](../../features/B010_Deleting_created_files.md) pass  


# Document: [B020_Exec_dir.md](../../features/B020_Exec_dir.md)  
  ## Feature: **Exec Dir**  
   ### Background: [create some dir and file](../../features/B020_Exec_dir.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given there is no `dir2` directory  
   - OK : Given the new `create_tree.md` file  
   - [X] background [create some dir and file](../../features/B020_Exec_dir.md) pass  

   ### Scenario: [Lets run "create_tree.md" in the current dir](../../features/B020_Exec_dir.md): 
   - OK : When I run `./bbt create_tree.md`  
   - OK : Then there is a `dir1` dir  
   - OK : And there is a `dir1/file1` file  
   - [X] scenario   [Lets run "create_tree.md" in the current dir](../../features/B020_Exec_dir.md) pass  

   ### Background: [create some dir and file](../../features/B020_Exec_dir.md): 
   - OK : Given there is no `dir1` directory  
   - OK : Given there is no `dir2` directory  
   - OK : Given the new `create_tree.md` file  
   - [X] background [create some dir and file](../../features/B020_Exec_dir.md) pass  

   ### Scenario: [Lets run "create_tree.md" in ./dir2](../../features/B020_Exec_dir.md): 
   - OK : Given the new `dir2` directory  
   - OK : When I run `./bbt create_tree.md --exec_dir dir2`  
   - OK : Then there is a `dir2/dir1` dir  
   - OK : And there is a `dir2/dir1/file1` file  
   - OK : And there is no `dir1` dir  
   - [X] scenario   [Lets run "create_tree.md" in ./dir2](../../features/B020_Exec_dir.md) pass  


# Document: [B030_File_creation_in_Given_steps.md](../../features/B030_File_creation_in_Given_steps.md)  
  ## Feature: testing the existence of a file  
   ### Scenario: [a required file does not exist](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given there is no file `config.ini`  
   - OK : When I run `./sut read config.ini`  
   - OK : Then I get error  
   - [X] scenario   [a required file does not exist](../../features/B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: ["Given there is no", when there actually is, should erase the file](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given my favorite and so useful `config.ini` file  
   - OK : Given there is no `config.ini` file    
   - OK : Then there is no more `config.ini` file  
   - [X] scenario   ["Given there is no", when there actually is, should erase the file](../../features/B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: [erasing confirmed by a typed key](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given the new file `config.ini` containing `Tmp_dir=/tmp`  
   - OK : Given the new `erase_confirm.md` file  
   - OK : When I run `./bbt -q -c erase_confirm.md`  
   - OK : Then the output is  
   - OK : Then the output contains `## Summary : **Success**, 1 scenarios OK`  
   - OK : And there is no `config.ini` file  
   - [X] scenario   [erasing confirmed by a typed key](../../features/B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: [erasing refused by a typed key](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given the new file `config.ini` containing `Tmp_dir=/tmp`  
   - OK : Given the new `erase_confirm.md` file  
   - OK : When I run `./bbt -q -c erase_confirm.md`  
   - OK : Then the output is  
   - OK : Then I get an error  
   - OK : And the output contains `file "config.ini" not deleted`  
   - OK : And there is a `config.ini` file  
   - [X] scenario   [erasing refused by a typed key](../../features/B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: [directory tree erasing confirmed by a typed key](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given the new `d1` directory  
   - OK : Given the new `d1/f1` file containing `some data`  
   - OK : Given the new `erase_dir_confirm.md` file  
   - OK : When I run `./bbt -q -c erase_dir_confirm.md`  
   - OK : Then the output is  
   - OK : Then the output contains `## Summary : **Success**, 1 scenarios OK`  
   - OK : And there is no `d1` directory  
   - [X] scenario   [directory tree erasing confirmed by a typed key](../../features/B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: [directory tree erasing refused by a typed key](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given the new `d1` directory  
   - OK : Given the new `d1/f1` file containing `some data`  
   - OK : Given the new `erase_dir_refuse.md` file  
   - OK : When I run `./bbt -q -c erase_dir_refuse.md`  
   - OK : Then the output is  
   - OK : Then I get an error  
   - OK : And the output contains `dir "d1" not deleted`  
   - OK : And there is a `d1` directory  
   - [X] scenario   [directory tree erasing refused by a typed key](../../features/B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: [new directory erasing confirmed by a typed key](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given the new `d2` directory  
   - OK : Given the new `d2/f1` file containing `some data`  
   - OK : Given the new `erase_new_dir_confirm.md` file  
   - OK : When I run `./bbt -q erase_new_dir_confirm.md`  
   - OK : Then the output is  
   - OK : Then the output contains `## Summary : **Success**, 1 scenarios OK`  
   - OK : And there is a `d2` directory  
   - OK : And there is no `d2/f1` file  
   - [X] scenario   [new directory erasing confirmed by a typed key](../../features/B030_File_creation_in_Given_steps.md) pass  

   ### Scenario: [new directory erasing refused by a typed key](../../features/B030_File_creation_in_Given_steps.md): 
   - OK : Given the new `d3` directory  
   - OK : Given the new `d3/f1` file containing `some data`  
   - OK : Given the new `erase_new_dir_refuse.md` file  
   - OK : When I run `./bbt -q erase_new_dir_refuse.md`  
   - OK : Then the output is  
   - OK : Then I get an error  
   - OK : And the output contains `dir "d3" not deleted`  
   - OK : And there is a `d3` directory  
   - OK : And there is a `d3/f1` file  
   - [X] scenario   [new directory erasing refused by a typed key](../../features/B030_File_creation_in_Given_steps.md) pass  


# Document: [B040_Find_scenarios.md](../../features/B040_Find_scenarios.md)  
  ## Feature: multiples scenarios given in command line  
   ### Background: [](../../features/B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](../../features/B040_Find_scenarios.md) pass  

   ### Scenario: [no file or dir on the command line](../../features/B040_Find_scenarios.md): 
   - OK : When I run `./bbt`  
   - OK : Then the output contains  
   - OK : And there is no error  
   - [X] scenario   [no file or dir on the command line](../../features/B040_Find_scenarios.md) pass  

   ### Background: [](../../features/B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](../../features/B040_Find_scenarios.md) pass  

   ### Scenario: [running all scenarios in dir1](../../features/B040_Find_scenarios.md): 
   - OK : When I run `./bbt lf dir1`  
   - OK : Then the output matches `dir1[/\\]scen1.md`  
   - OK : And the output matches `dir1[/\\]scen2.md`  
   - [X] scenario   [running all scenarios in dir1](../../features/B040_Find_scenarios.md) pass  

   ### Background: [](../../features/B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](../../features/B040_Find_scenarios.md) pass  

   ### Scenario: [running multiple scenarios given on command line](../../features/B040_Find_scenarios.md): 
   - OK : When I run `./bbt list_files dir1/scen3.scen dir1/scen4.scen`    
   - OK : Then the output is  
   - [X] scenario   [running multiple scenarios given on command line](../../features/B040_Find_scenarios.md) pass  

   ### Background: [](../../features/B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](../../features/B040_Find_scenarios.md) pass  

   ### Scenario: [running scenarios in a tree thanks to `-r`](../../features/B040_Find_scenarios.md): 
   - OK : Given the `dir1/dir2/` dir  
   - OK : Given the `dir1/dir3/dir4` dir  
   - OK : Given the `dir1/dir2/scen5.md` file containing `foo`  
   - OK : Given the `dir1/dir3/dir4/scen6.md` file containing `bar`  
   - OK : When I run `./bbt lf -r dir1`  
   - OK : Then the output matches `dir1[/\\]scen1.md`  
   - OK : And the output matches `dir1[/\\]scen2.md`  
   - OK : And the output matches `dir1[/\\]dir2[/\\]scen5.md`  
   - OK : And the output matches `dir1[/\\]dir3[/\\]dir4[/\\]scen6.md`  
   - [X] scenario   [running scenarios in a tree thanks to `-r`](../../features/B040_Find_scenarios.md) pass  

   ### Background: [](../../features/B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](../../features/B040_Find_scenarios.md) pass  

   ### Scenario: [error msg when trying to run scenarios, but none found in given directories](../../features/B040_Find_scenarios.md): 
   - OK : Given the `dir5` dir  
   - OK : Given the `dir6` dir  
   - OK : Given the `dir6/dir7` dir  
   - OK : When I run `./bbt dir5 dir6`  
   - OK : Then the output contains  
   - OK : And I get no error  
   - [X] scenario   [error msg when trying to run scenarios, but none found in given directories](../../features/B040_Find_scenarios.md) pass  

   ### Background: [](../../features/B040_Find_scenarios.md): 
   - OK : Given the new `dir1` directory  
   - OK : Given the `dir1/scen1.md` file  
   - OK : Given the `dir1/scen2.md` file  
   - OK : Given the `dir1/scen3.scen` file  
   - OK : Given the `dir1/scen4.scen` file  
   - [X] background [](../../features/B040_Find_scenarios.md) pass  

   ### Scenario: [empty list file on list_files if there is no scenario in given directories](../../features/B040_Find_scenarios.md): 
   - OK : When I run `./bbt lf dir5 dir6`  
   - OK : Then I get no output  
   - OK : And I get no error  
   - [X] scenario   [empty list file on list_files if there is no scenario in given directories](../../features/B040_Find_scenarios.md) pass  


# Document: [B050_Strict_gherkin.md](../../features/B050_Strict_gherkin.md)  
  ## Feature: Strict Gherkin rules  
   ### Scenario: [Multiple When in a scenario](../../features/B050_Strict_gherkin.md): 
   - OK : Given there is no file `tmp.txt`  
   - OK : Given the file `t1.md` :  
   - OK : When I successfully run `./bbt --strict t1.md`  
   - OK : Then the output contains  
   - OK : And `tmp.txt` contains   
   - OK : When I successfully run `./bbt t1.md`  
   - OK : Then the output do not contain `Warning`  
   - OK : And `tmp.txt` contains   
   - [X] scenario   [Multiple When in a scenario](../../features/B050_Strict_gherkin.md) pass  


# Document: [B060_Return_code_on_test_failure.md](../../features/B060_Return_code_on_test_failure.md)  
  ## Feature: return code on test failure  
   ### Scenario: [return code on test success](../../features/B060_Return_code_on_test_failure.md): 
   - OK : Given the new `good_option.md` file  
   - OK : When I run `./bbt good_option.md`  
   - OK : Then I get no error   
   - OK : and `good_option.md.out` is   
   - [X] scenario   [return code on test success](../../features/B060_Return_code_on_test_failure.md) pass  

   ### Scenario: [return code when the test fail](../../features/B060_Return_code_on_test_failure.md): 
   - OK : Given the new `wrong_option.md` file  
   - OK : When I run `./bbt --cleanup wrong_option.md`  
   - OK : Then I get an error  
   - [X] scenario   [return code when the test fail](../../features/B060_Return_code_on_test_failure.md) pass  

   ### Scenario: [return code when one fail and the other succeed](../../features/B060_Return_code_on_test_failure.md): 
   - OK : Given the new `wrong_and_good_option.md` file  
   - OK : When I run `./bbt --cleanup wrong_and_good_option.md`  
   - OK : Then I get an error  
   - [X] scenario   [return code when one fail and the other succeed](../../features/B060_Return_code_on_test_failure.md) pass  


# Document: [B080_Keep_Going.md](../../features/B080_Keep_Going.md)  
  ## Feature: on error, Keep going or stop  
   ### Background: [setup](../../features/B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](../../features/B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_syntax_error run without `-k`](../../features/B080_Keep_Going.md): 
   - OK : When I run `./bbt scenario_with_syntax_error.md`  
   - OK : then output is  
   - [X] scenario   [scenario_with_syntax_error run without `-k`](../../features/B080_Keep_Going.md) pass  

   ### Background: [setup](../../features/B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](../../features/B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_syntax_error run with `-k`](../../features/B080_Keep_Going.md): 
   - OK : When I run `./bbt -k scenario_with_syntax_error.md`  
   - OK : then output contains  
   - OK : and output contains  
   - OK : and output contains  
   - OK : and output contains  
   - OK : and output contains  
   - OK : and output contains  
   - [X] scenario   [scenario_with_syntax_error run with `-k`](../../features/B080_Keep_Going.md) pass  

   ### Background: [setup](../../features/B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](../../features/B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_assertion_error run without `-k`](../../features/B080_Keep_Going.md): 
   - OK : When I run `./bbt scenario_with_assertion_error.md`  
   - OK : then output is  
   - [X] scenario   [scenario_with_assertion_error run without `-k`](../../features/B080_Keep_Going.md) pass  

   ### Background: [setup](../../features/B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](../../features/B080_Keep_Going.md) pass  

   ### Scenario: [scenario_with_assertion_error run with `-k`](../../features/B080_Keep_Going.md): 
   - OK : When I run `./bbt -k scenario_with_assertion_error.md`  
   - OK : then output is  
   - [X] scenario   [scenario_with_assertion_error run with `-k`](../../features/B080_Keep_Going.md) pass  

   ### Background: [setup](../../features/B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](../../features/B080_Keep_Going.md) pass  

   ### Scenario: [explain scenario_with_syntax_error without `-k`](../../features/B080_Keep_Going.md): 
   - OK : When I run `./bbt ex scenario_with_syntax_error.md`  
   - OK : then output is  
   - [X] scenario   [explain scenario_with_syntax_error without `-k`](../../features/B080_Keep_Going.md) pass  

   ### Background: [setup](../../features/B080_Keep_Going.md): 
   - OK : Given the file `scenario_with_syntax_error.md`  
   - OK : Given the file `scenario_with_assertion_error.md`  
   - [X] background [setup](../../features/B080_Keep_Going.md) pass  

   ### Scenario: [explain scenario_with_syntax_error with `-k`](../../features/B080_Keep_Going.md): 
   - OK : When I run `./bbt -k ex scenario_with_syntax_error.md`  
   - OK : then output is  
   - [X] scenario   [explain scenario_with_syntax_error with `-k`](../../features/B080_Keep_Going.md) pass  


# Document: [B090_tmp_dir.md](../../features/B090_tmp_dir.md)  
  ## Feature: Tmp dir  
   ### Background: [](../../features/B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](../../features/B090_tmp_dir.md) pass  

   ### Scenario: [Current dir, no cleanup](../../features/B090_tmp_dir.md): 
   - OK : When I run `./bbt scen1.md`   
   - OK : Then there is a `scen1.md.out` file  
   - [X] scenario   [Current dir, no cleanup](../../features/B090_tmp_dir.md) pass  

   ### Background: [](../../features/B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](../../features/B090_tmp_dir.md) pass  

   ### Scenario: [Current dir, with cleanup](../../features/B090_tmp_dir.md): 
   - OK : When I run `./bbt --cleanup scen1.md`   
   - OK : Then there is no `scen1.md.out` file  
   - [X] scenario   [Current dir, with cleanup](../../features/B090_tmp_dir.md) pass  

   ### Background: [](../../features/B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](../../features/B090_tmp_dir.md) pass  

   ### Scenario: [Within dir1, no cleanup](../../features/B090_tmp_dir.md): 
   - OK : When I run `./bbt --tmp_dir dir1 scen1.md`   
   - OK : Then there is a `dir1` directory  
   - OK : And  there is a `dir1/scen1.md.out` file  
   - [X] scenario   [Within dir1, no cleanup](../../features/B090_tmp_dir.md) pass  

   ### Background: [](../../features/B090_tmp_dir.md): 
   - OK : Given there is no `tmp.txt` file  
   - OK : Given there is no `dir1` dir  
   - OK : Given there is no `dir2` dir  
   - OK : Given the new file `scen1.md`  
   - [X] background [](../../features/B090_tmp_dir.md) pass  

   ### Scenario: [Within dir1, with cleanup](../../features/B090_tmp_dir.md): 
   - OK : When I run `./bbt --cleanup -v --tmp_dir dir2 scen1.md`   
   - OK : Then there is no `dir2/scen1.md.out` file  
   - OK : And  there is no `dir2` directory  
   - [X] scenario   [Within dir1, with cleanup](../../features/B090_tmp_dir.md) pass  


# Document: [B100_Results_Output_In_MD_Format.md](../../features/B100_Results_Output_In_MD_Format.md)  
   ### Background: [](../../features/B100_Results_Output_In_MD_Format.md): 
   - OK : Given the new file `OK_scen.md`  
   - OK : Given the new file `NOK_scen.md`  
   - [X] background [](../../features/B100_Results_Output_In_MD_Format.md) pass  

   ### Scenario: [default mode run](../../features/B100_Results_Output_In_MD_Format.md): 
   - OK : When I run `./bbt -c --yes OK_scen.md`  
   - OK : Then the output is  
   - [X] scenario   [default mode run](../../features/B100_Results_Output_In_MD_Format.md) pass  

   ### Background: [](../../features/B100_Results_Output_In_MD_Format.md): 
   - OK : Given the new file `OK_scen.md`  
   - OK : Given the new file `NOK_scen.md`  
   - [X] background [](../../features/B100_Results_Output_In_MD_Format.md) pass  

   ### Scenario: [verbose mode run](../../features/B100_Results_Output_In_MD_Format.md): 
   - OK : When I run `./bbt -v -c --yes OK_scen.md`  
   - OK : Then the output contains  
   - [X] scenario   [verbose mode run](../../features/B100_Results_Output_In_MD_Format.md) pass  


# Document: [B105_Expected_vs_Actual_Output.md](../../features/B105_Expected_vs_Actual_Output.md)  
   ### Background: [](../../features/B105_Expected_vs_Actual_Output.md): 
   - OK : Given the new file `reference.txt`  
   - [X] background [](../../features/B105_Expected_vs_Actual_Output.md) pass  

   ### Scenario: [full comparison with no error](../../features/B105_Expected_vs_Actual_Output.md): 
   - OK : Then file `reference.txt` is  
   - [X] scenario   [full comparison with no error](../../features/B105_Expected_vs_Actual_Output.md) pass  

   ### Background: [](../../features/B105_Expected_vs_Actual_Output.md): 
   - OK : Given the new file `reference.txt`  
   - [X] background [](../../features/B105_Expected_vs_Actual_Output.md) pass  

   ### Scenario: [full comparison with error in the middle of the file](../../features/B105_Expected_vs_Actual_Output.md): 
   - OK : Given the new file `scen1.md`  
   - OK : When I run `./bbt --yes scen1.md`  
   - OK : Then the output contains  
   - OK : And the output contains  
   - [X] scenario   [full comparison with error in the middle of the file](../../features/B105_Expected_vs_Actual_Output.md) pass  

   ### Background: [](../../features/B105_Expected_vs_Actual_Output.md): 
   - OK : Given the new file `reference.txt`  
   - [X] background [](../../features/B105_Expected_vs_Actual_Output.md) pass  

   ### Scenario: [full comparison with blank lines on the left and on the right](../../features/B105_Expected_vs_Actual_Output.md): 
   - OK : Given the new file `scen2.md`  
   - OK : Then I successfully run `./bbt --cleanup --yes scen2.md`  
   - OK : When I run `./bbt --exact_match --cleanup --yes scen2.md`  
   - OK : Then there is an error   
   - OK : And the output contains   
   - OK : And the output contains  
   - OK : And the output contains  
   - [X] scenario   [full comparison with blank lines on the left and on the right](../../features/B105_Expected_vs_Actual_Output.md) pass  


# Document: [B110_Spawn.md](../../features/B110_Spawn.md)  
  ## Feature: Command line to spawn processing  
   ### Scenario: [Directory with space in the name (Unix_Only)](../../features/B110_Spawn.md): 
   - OK : Given the directory `dir 1`  
   - OK : when I run `./sut create dir\ 1/file\ 1`   
   - OK : then I get no error  
   - OK : And there is a file `dir 1/file 1`  
   - [X] scenario   [Directory with space in the name (Unix_Only)](../../features/B110_Spawn.md) pass  

   ### Scenario: [Command with quoted arguments (Unix_Only)](../../features/B110_Spawn.md): 
   - OK : Given the new file `tmp.txt`  
   - OK : When I run `./sut append "Hello world" tmp.txt`  
   - OK : Then I get no error  
   - OK : And file `tmp.txt` is `Hello world`  
   - OK : When I run `./sut append Bye tmp.txt`  
   - OK : Then I get no error  
   - OK : And file `tmp.txt` is   
   - [X] scenario   [Command with quoted arguments (Unix_Only)](../../features/B110_Spawn.md) pass  


# Document: [B115_Warning_On_Shell_Metacharacters.md](../../features/B115_Warning_On_Shell_Metacharacters.md)  
  ## Feature: warning on shell metacharacters in commands  
   ### Scenario: [warning when a command contains a pipe](../../features/B115_Warning_On_Shell_Metacharacters.md): 
   - OK : Given the new file `pipe_test.md`  
   - OK : When I run `./bbt -c pipe_test.md`  
   - OK : Then output is  
   - [X] scenario   [warning when a command contains a pipe](../../features/B115_Warning_On_Shell_Metacharacters.md) pass  

   ### Scenario: [warning also displayed by bbt explain](../../features/B115_Warning_On_Shell_Metacharacters.md): 
   - OK : When I run `./bbt explain pipe_test.md`  
   - OK : Then output is  
   - [X] scenario   [warning also displayed by bbt explain](../../features/B115_Warning_On_Shell_Metacharacters.md) pass  

   ### Scenario: [no warning when the metacharacter is quoted (Unix_Only)](../../features/B115_Warning_On_Shell_Metacharacters.md): 
   - OK : Given the new file `quoted_glob_test.md`  
   - OK : When I run `./bbt -c quoted_glob_test.md`  
   - OK : Then output is  
   - [X] scenario   [no warning when the metacharacter is quoted (Unix_Only)](../../features/B115_Warning_On_Shell_Metacharacters.md) pass  

   ### Scenario: [no warning on a quoted metacharacter in bbt explain](../../features/B115_Warning_On_Shell_Metacharacters.md): 
   - OK : Given the new file `quoted_glob_test.md`  
   - OK : When I run `./bbt explain quoted_glob_test.md`  
   - OK : Then output is  
   - [X] scenario   [no warning on a quoted metacharacter in bbt explain](../../features/B115_Warning_On_Shell_Metacharacters.md) pass  


# Document: [B120_Output_Verbosity.md](../../features/B120_Output_Verbosity.md)  
   ### Background: [](../../features/B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](../../features/B120_Output_Verbosity.md) pass  

   ### Scenario: [Quiet mode run](../../features/B120_Output_Verbosity.md): 
   - OK : When I successfully run `./bbt -c --yes -q OK_scen.md`  
   - OK : Then the output is `## Summary : **Success**, 2 scenarios OK`  
   - [X] scenario   [Quiet mode run](../../features/B120_Output_Verbosity.md) pass  

   ### Background: [](../../features/B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](../../features/B120_Output_Verbosity.md) pass  

   ### Scenario: [Default mode run](../../features/B120_Output_Verbosity.md): 
   - OK : When I successfully run `./bbt -c --yes OK_scen.md`  
   - OK : Then the output is  
   - [X] scenario   [Default mode run](../../features/B120_Output_Verbosity.md) pass  

   ### Background: [](../../features/B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](../../features/B120_Output_Verbosity.md) pass  

   ### Scenario: [Verbose mode run](../../features/B120_Output_Verbosity.md): 
   - OK : When I successfully run `./bbt -c --yes -v OK_scen.md`  
   - OK : Then the output is  
   - [X] scenario   [Verbose mode run](../../features/B120_Output_Verbosity.md) pass  

   ### Background: [](../../features/B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](../../features/B120_Output_Verbosity.md) pass  

   ### Scenario: [Normal mode with an error](../../features/B120_Output_Verbosity.md): 
   - OK : When I run `./bbt -c --yes NOK_scen.md`  
   - OK : Then there is an error  
   - OK : And  the output is  
   - [X] scenario   [Normal mode with an error](../../features/B120_Output_Verbosity.md) pass  

   ### Background: [](../../features/B120_Output_Verbosity.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : Given the file `NOK_scen.md`  
   - [X] background [](../../features/B120_Output_Verbosity.md) pass  

   ### Scenario: [Quiet mode with an error](../../features/B120_Output_Verbosity.md): 
   - OK : When I run `./bbt -c --yes --quiet NOK_scen.md`  
   - OK : Then there is an error  
   - OK : And  the output is  
   - [X] scenario   [Quiet mode with an error](../../features/B120_Output_Verbosity.md) pass  


# Document: [B130_Cmd_Line_Help.md](../../features/B130_Cmd_Line_Help.md)  
  ## Feature: Clear command line help  
   ### Scenario: [filtering help](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt he filtering`   
   - OK : then the output is equal to file `../docs/help/filtering.txt`  
   - [X] scenario   [filtering help](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [matching help](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help matching`   
   - OK : then the output is equal to file `../docs/help/matching.txt`  
   - [X] scenario   [matching help](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [others help](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help other`   
   - OK : then the output is equal to file `../docs/help/other.txt`  
   - [X] scenario   [others help](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [On_All help](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help on_all`   
   - OK : then the output contains file `../docs/help/base.txt`  
   - OK : and  the output contains file `../docs/help/filtering.txt`  
   - OK : and  the output contains file `../docs/help/matching.txt`  
   - OK : and  the output contains file `../docs/help/other.txt`  
   - [X] scenario   [On_All help](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [tutorial generation](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help tutorial`   
   - OK : then the output is equal to file `../docs/help/tutorial.md`  
   - [X] scenario   [tutorial generation](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [grammar generation](../../features/B130_Cmd_Line_Help.md): 
   - OK : Given the file `expected_grammar`  
   - OK : When I run `./bbt help grammar`   
   - OK : then the output is equal to file `expected_grammar`  
   - [X] scenario   [grammar generation](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [listing keywords](../../features/B130_Cmd_Line_Help.md): 
   - OK : Given the file `expected_keywords`  
   - OK : When I run `./bbt help keywords`   
   - OK : then the output is equal to file `expected_keywords`  
   - [X] scenario   [listing keywords](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [generated example is OK](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help example`   
   - OK : then the output is equal to file `../docs/help/example.md`  
   - [X] scenario   [generated example is OK](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [calling bbt without parameter or with -h put the normal help 1/3](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt`  
   - OK : then the output contains file `../docs/help/base.txt`  
   - OK : and  the output matches `bbt version [0-9]+\.[0-9]+\.[0-9]+.*`  
   - [X] scenario   [calling bbt without parameter or with -h put the normal help 1/3](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [calling bbt without parameter or with -h put the normal help 2/3](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt help`  
   - OK : then the output contains file `../docs/help/base.txt`  
   - OK : and  the output matches `bbt version [0-9]+\.[0-9]+\.[0-9]+.*`  
   - [X] scenario   [calling bbt without parameter or with -h put the normal help 2/3](../../features/B130_Cmd_Line_Help.md) pass  

   ### Scenario: [calling bbt without parameter or with -h put the normal help 3/3](../../features/B130_Cmd_Line_Help.md): 
   - OK : When I run `./bbt he`  
   - OK : then the output contains file `../docs/help/base.txt`  
   - OK : and  the output matches `bbt version [0-9]+\.[0-9]+\.[0-9]+.*`  
   - [X] scenario   [calling bbt without parameter or with -h put the normal help 3/3](../../features/B130_Cmd_Line_Help.md) pass  


# Document: [B140_Index_File.md](../../features/B140_Index_File.md)  
   ### Scenario: [Successful run index file](../../features/B140_Index_File.md): 
   - OK : Given the file `OK_scen.md`  
   - OK : And there is no `index1.md` file  
   - OK : And there is no `index2.md` file  
   - OK : And there is no `verbose_output_OK.md` file  
   - OK : When I successfully run `./bbt -c --yes -v OK_scen.md --index verbose_output_OK.md`  
   - OK : When I successfully run `./bbt -c --yes -q OK_scen.md --index index_1.md`  
   - OK : Then `index_1.md` is equal to file `verbose_output_OK.md`  
   - OK : When I successfully run `./bbt -c --yes    OK_scen.md --index index_2.md`  
   - OK : Then `index_2.md` is equal to file `verbose_output_OK.md`  
   - [X] scenario   [Successful run index file](../../features/B140_Index_File.md) pass  

   ### Scenario: [Unsuccessful run index file](../../features/B140_Index_File.md): 
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
   - [X] scenario   [Unsuccessful run index file](../../features/B140_Index_File.md) pass  

   ### Scenario: [index links use web separators](../../features/B140_Index_File.md): 
   - OK : Given the new directory `dir1`  
   - OK : Given the file `dir1/OK_scen.md` containing  
   - OK : Given the directory `dir1/sub`  
   - OK : When I successfully run `./bbt -c --yes dir1/OK_scen.md --index dir1/sub/index_web.md`  
   - OK : Then the file `dir1/sub/index_web.md` contains `](../OK_scen.md)`  
   - OK : And the file `dir1/sub/index_web.md` does not contain `\`  
   - [X] scenario   [index links use web separators](../../features/B140_Index_File.md) pass  


# Document: [B150_Deprecated_Options.md](../../features/B150_Deprecated_Options.md)  
  ## Feature: Deprecated and no more supported options  
   ### Scenario: [--output option](../../features/B150_Deprecated_Options.md): 
   - OK : Given the new `ver.md` file  
   - OK : When I run `./bbt -c --yes ver.md --output tmp.md`   
   - OK : Then the output contains   
   - [X] scenario   [--output option](../../features/B150_Deprecated_Options.md) pass  


# Document: [B160_JUnit_XML_Export.md](../../features/B160_JUnit_XML_Export.md)  
  ## Feature: JUnit XML Export  
   ### Scenario: [1 : Basic file with only a single scenario](../../features/B160_JUnit_XML_Export.md): 
   - OK : Given there is no file `result.xml`  
   - OK : Given the file `simple_test.md` containing  
   - OK : When I run `./bbt --junit result.xml simple_test.md`  
   - OK : Then I get no error  
   - OK : And file `result.xml` contains `<?xml version="1.0" encoding="UTF-8"?>`  
   - OK : And file `result.xml` matches `<testsuites name="result" tests="1" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`  
   - OK : And file `result.xml` matches `  <testsuite name="" tests="1" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`  
   - OK : And file `result.xml` matches `    <testcase name="Passing test" classname="" time="[0-9]*\.[0-9]*"/>`  
   - OK : And file `result.xml` contains  
   - [X] scenario   [1 : Basic file with only a single scenario](../../features/B160_JUnit_XML_Export.md) pass  

  ## Feature: Skipped tests count  
   ### Background: [](../../features/B160_JUnit_XML_Export.md): 
   - OK : Given there is no `file1` file  
   - OK : Given there is no `file2` file  
   - OK : Given the scenarios file `skipped_count_test.md`   
   - [X] background [](../../features/B160_JUnit_XML_Export.md) pass  

   ### Scenario: [1: both scenario are run](../../features/B160_JUnit_XML_Export.md): 
   - OK : Given there is no `all_results.xml` file  
   - OK : When I successfully run `./bbt --junit all_results.xml skipped_count_test.md`  
   - OK : Then I get no error  
   - OK : And file `all_results.xml` matches `<testsuites name="all_results" tests="2" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`  
   - [X] scenario   [1: both scenario are run](../../features/B160_JUnit_XML_Export.md) pass  

   ### Background: [](../../features/B160_JUnit_XML_Export.md): 
   - OK : Given there is no `file1` file  
   - OK : Given there is no `file2` file  
   - OK : Given the scenarios file `skipped_count_test.md`   
   - [X] background [](../../features/B160_JUnit_XML_Export.md) pass  

   ### Scenario: [2 : one scenario is skipped](../../features/B160_JUnit_XML_Export.md): 
   - OK : Given there is no `windows_results.xml` file  
   - OK : When I successfully run `./bbt --junit windows_results.xml --select @Windows_Specific skipped_count_test.md`  
   - OK : Then file `windows_results.xml` matches `<testsuites name="windows_results" tests="2" failures="0" errors="0" skipped="1" time="[0-9]*\.[0-9]*">`  
   - [X] scenario   [2 : one scenario is skipped](../../features/B160_JUnit_XML_Export.md) pass  

  ## Feature: xml robustness  
   ### Scenario: [Test XML escaping with special characters](../../features/B160_JUnit_XML_Export.md): 
   - OK : Given there is no file `result2.xml`  
   - OK : Given the file `escape_test.md` containing  
   - OK : When I run `./bbt --junit result2.xml escape_test.md`  
   - OK : Then I get no error  
   - OK : And file `result2.xml` matches `    <testcase name="Title with Quotation mark &quot; Ampersand &amp; Greater and less than &lt; &gt; or Apostrophe &apos;&quot;" classname="" time="[0-9]*\.[0-9]*"/>`  
   - [X] scenario   [Test XML escaping with special characters](../../features/B160_JUnit_XML_Export.md) pass  

  ## Feature: Time Attribute (@Flaky results depends on execution time on the plafform)  

# Document: [B170_Explain.md](../../features/B170_Explain.md)  
  ## Feature: Explain  
   ### Scenario: [explain elements other than steps](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain elements other than steps](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Setup_No_File](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Setup_No_File](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Setup_No_Dir](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Setup_No_Dir](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Check_File_Existence](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_File_Existence](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Check_Dir_Existence](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_Dir_Existence](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Erase_And_Create](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Erase_And_Create](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Create_If_None](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Create_If_None](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Run](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Run](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Run_Without_Error](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Run_Without_Error](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Check_No_File](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_No_File](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Check_No_Dir](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Check_No_Dir](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Error_Return_Code and No_Error_Return_Code](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Error_Return_Code and No_Error_Return_Code](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Output_Is](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Output_Is](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Output_Contains and Output_Does_Not_Contain](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Output_Contains and Output_Does_Not_Contain](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action Output|File_Matches and Output|File_Does_Not_Match](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action Output|File_Matches and Output|File_Does_Not_Match](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action File_Is and File_Is_Not](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action File_Is and File_Is_Not](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action File_Contains and File_Does_Not_Contain](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action File_Contains and File_Does_Not_Contain](../../features/B170_Explain.md) pass  

   ### Scenario: [explain action No_Output](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain action No_Output](../../features/B170_Explain.md) pass  

   ### Scenario: [explain Invalid Step](../../features/B170_Explain.md): 
   - OK : Given the new file `scenario_to_explain.md`   
   - OK : When I run `./bbt explain scenario_to_explain.md`  
   - OK : Then the output contains  
   - OK : And the output contains  
   - OK : When I run `./bbt -q explain scenario_to_explain.md`  
   - OK : Then I get   
   - [X] scenario   [explain Invalid Step](../../features/B170_Explain.md) pass  


# Document: [B180_Ignored_Files.md](../../features/B180_Ignored_Files.md)  
  ## Feature: some files are ignored by *bbt*  
   ### Scenario: [Checking that the index file is ignored](../../features/B180_Ignored_Files.md): 
   - OK : Given the new dir `tmp77`  
   - OK : When running  `./bbt list_files --tmp_dir tmp77 --index tmp77/index.md ../docs/examples/gcc_hello_world.md tmp77`  
   - OK : Then I should get   
   - [X] scenario   [Checking that the index file is ignored](../../features/B180_Ignored_Files.md) pass  


# Document: [B190_Clean_Error_When_No_Output.md](../../features/B190_Clean_Error_When_No_Output.md)  
  ## Feature: clean error when checking the output of a command that did not run  
   ### Scenario: [check the output of a command that does not exist](../../features/B190_Clean_Error_When_No_Output.md): 
   - OK : Given the new file `no_cmd_test.md`  
   - OK : When I run `./bbt -k no_cmd_test.md`  
   - OK : Then output contains  
   - OK : and output contains  
   - OK : Then I get an error  
   - [X] scenario   [check the output of a command that does not exist](../../features/B190_Clean_Error_When_No_Output.md) pass  


# Document: [C010_Empty_scenarios.md](../../features/C010_Empty_scenarios.md)  
   ### Scenario: [No step test A](../../features/C010_Empty_scenarios.md): 
   - OK : Given the `no_step_in_scenario.input` file  
   - OK : When I successfully run `./bbt no_step_in_scenario.input`   
   - OK : Then the output contains `scenario [My_Scenario](no_step_in_scenario.input) is empty, nothing tested`  
   - [X] scenario   [No step test A](../../features/C010_Empty_scenarios.md) pass  

   ### Scenario: [No step test B](../../features/C010_Empty_scenarios.md): 
   - OK : Given the `no_step_in_scenario_b.input` file  
   - OK : When I successfully run `./bbt no_step_in_scenario_b.input`   
   - OK : Then the output contains `scenario [My_Scenario](no_step_in_scenario_b.input) is empty, nothing tested`  
   - [X] scenario   [No step test B](../../features/C010_Empty_scenarios.md) pass  

   ### Scenario: [No scenario in Feature](../../features/C010_Empty_scenarios.md): 
   - OK : Given the `no_step_or_scenario_in_feature.input` file  
   - OK : When I successfully run `./bbt no_step_or_scenario_in_feature.input`   
   - OK : Then the output contains `Warning : No scenario in feature "My_Feature"`  
   - [X] scenario   [No scenario in Feature](../../features/C010_Empty_scenarios.md) pass  


# Document: [C030_Markdown_syntax.md](../../features/C030_Markdown_syntax.md)  
  ## Feature: Tolerance to formatting variations  
   ### Scenario: [Heading variations](../../features/C030_Markdown_syntax.md): 
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
   - [X] scenario   [Heading variations](../../features/C030_Markdown_syntax.md) pass  

   ### Scenario: [Missing heading marker](../../features/C030_Markdown_syntax.md): 
   - OK : Given the new file `no_heading_marker.input`  
   - OK : When I run `./bbt -d no_heading_marker.input`  
   - [X] scenario   [Missing heading marker](../../features/C030_Markdown_syntax.md) pass  

   ### Scenario: [Steps using [Emphasis and strong emphasis](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis)](../../features/C030_Markdown_syntax.md): 
   - OK : Given the new file `italic_and_bold_in_steps.md`  
   - OK : When I successfully run `./bbt explain italic_and_bold_in_steps.md`  
   - OK : Then the output contains  
   - [X] scenario   [Steps using [Emphasis and strong emphasis](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis)](../../features/C030_Markdown_syntax.md) pass  


# Document: [C040_Missing_title.md](../../features/C040_Missing_title.md)  
   ### Scenario: [Missing tittle in scenario, background and feature](../../features/C040_Missing_title.md): 
   - OK : Given the new file `no_title.md`  
   - OK : When I run `./bbt -c --yes no_title.md`  
   - OK : Then output is  
   - [X] scenario   [Missing tittle in scenario, background and feature](../../features/C040_Missing_title.md) pass  


# Document: [C050_Step_marker.md](../../features/C050_Step_marker.md)  
  ## Feature:   
   ### Scenario: [Different markers within the scenario](../../features/C050_Step_marker.md): 
   - OK : Given the file `step_markers.md`  
   - OK : When I run `./bbt -c -q step_markers.md`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [Different markers within the scenario](../../features/C050_Step_marker.md) pass  


# Document: [C060_code_block.md](../../features/C060_code_block.md)  
   ### Scenario: [Code fenced blocks](../../features/C060_code_block.md): 
   - OK : Given the new file `lot_of_code_blocks.md`  
   - OK : When I run `./bbt -c lot_of_code_blocks.md`  
   - OK : Then I get no error  
   - [X] scenario   [Code fenced blocks](../../features/C060_code_block.md) pass  


# Document: [C070_missing_code_block.md](../../features/C070_missing_code_block.md)  
  ## Feature: missing or erroneous code blocks  
   ### Scenario: [Code block missing at the end of the file](../../features/C070_missing_code_block.md): 
   - OK : Given the new file `code_block_missing_at_EOF.md`  
   - OK : When I run `./bbt -c code_block_missing_at_EOF.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Code block missing at the end of the file](../../features/C070_missing_code_block.md) pass  

   ### Scenario: [Code block missing while reaching next step](../../features/C070_missing_code_block.md): 
   - OK : Given the new file `code_block_missing_in_step.md`  
   - OK : When I run `./bbt -c code_block_missing_in_step.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Code block missing while reaching next step](../../features/C070_missing_code_block.md) pass  

   ### Scenario: [Code block missing while reaching next scenario](../../features/C070_missing_code_block.md): 
   - OK : Given the new file `code_block_missing_in_scenario.md`  
   - OK : When I run `./bbt -c code_block_missing_in_scenario.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Code block missing while reaching next scenario](../../features/C070_missing_code_block.md) pass  

   ### Scenario: [Closing code block mark missing](../../features/C070_missing_code_block.md): 
   - OK : Given the new file `code_block_not_closed.md`  
   - OK : When I run `./bbt -c code_block_not_closed.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - OK : Given the new file `code_block_not_closed2.md`  
   - OK : When I run `./bbt -c code_block_not_closed2.md`  
   - OK : Then there is an error   
   - OK : And  the output contains   
   - [X] scenario   [Closing code block mark missing](../../features/C070_missing_code_block.md) pass  


# Document: [C080_missing_scenario.md](../../features/C080_missing_scenario.md)  
  ## Feature:   
   ### Scenario: [Steps without scenario header](../../features/C080_missing_scenario.md): 
   - OK : Given the file `No_Scenario.md`  
   - OK : When I run `./bbt No_Scenario.md`  
   - OK : Then output contains   
   - [X] scenario   [Steps without scenario header](../../features/C080_missing_scenario.md) pass  

   ### Scenario: [Empty file](../../features/C080_missing_scenario.md): 
   - OK : Given the file `empty.md`  
   - OK : When I run `./bbt empty.md`  
   - OK : Then output contains   
   - [X] scenario   [Empty file](../../features/C080_missing_scenario.md) pass  


# Document: [C090_GNU_Error_Msg_Format.md](../../features/C090_GNU_Error_Msg_Format.md)  
  ## Feature: GNU error messages  
   ### Scenario: [](../../features/C090_GNU_Error_Msg_Format.md): 
   - OK : Given the file `t1.md`  
   - OK : When I successfully run `./bbt --strict t1.md`  
   - OK : Then the output matches `t1.md:[0-9]*: Warning: Multiple When in the same Scenario.*`  
   - [X] scenario   [](../../features/C090_GNU_Error_Msg_Format.md) pass  


# Document: [C110_Two_Verbs_In_The_Step.md](../../features/C110_Two_Verbs_In_The_Step.md)  
   ### Scenario: [Detection of ambiguous use of multiple recognized verbs in the same Step](../../features/C110_Two_Verbs_In_The_Step.md): 
   - OK : Given the new file `too_much_verbs_in_step.md`  
   - OK : When I run `./bbt -c --yes too_much_verbs_in_step.md`  
   - OK : then the output contains   
   - [X] scenario   [Detection of ambiguous use of multiple recognized verbs in the same Step](../../features/C110_Two_Verbs_In_The_Step.md) pass  


# Document: [C120_Ill_Formated_Steps.md](../../features/C120_Ill_Formated_Steps.md)  
  ## Feature: bbt is providing helpful messages on ill formatted step lines  
   ### Scenario: [](../../features/C120_Ill_Formated_Steps.md): 
   - OK : Given the new file `bad_steps.md`  
   - OK : When I run `./bbt -k -c bad_steps.md`  
   - OK : Then the output contains   
   - OK : And the output contains   
   - OK : And the output contains  
   - OK : And I get an error  
   - [X] scenario   [](../../features/C120_Ill_Formated_Steps.md) pass  


## Summary : **Success**, 222 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 222   |
| Empty      | 0     |
| Not Run    | 1     |

