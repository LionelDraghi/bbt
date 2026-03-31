# Feature: some files are ignored by *bbt*

Ignore the file given as index, with a warning, as it is probably a md file. 
This is to avoid being disturbed when running some scenarios several times in a row in the same place.

## Scenario: Checking that the index file is ignored

- Given the new dir `tmp77`
- When running  `./bbt list_files --tmp_dir tmp77 --index tmp77/index.md ../docs/examples/gcc_hello_world.md tmp77`
- Then I should get 
  ```
  Warning: Ignoring file tmp77/index.md  
  ../docs/examples/gcc_hello_world.md  
  ```
