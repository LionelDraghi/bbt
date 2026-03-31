# Feature: some files are ignored by *bbt*

Ignore the file given as index, with a warning, as it is probably a md file. 
This is to avoid being disturbed when running some scenarios several times in a row in the same place.

## Scenario: Checking that the index file is ignored

- Given the new dir `tmp`
- When running  `./bbt list_files --tmp_dir tmp --index index.md ../docs/examples/gcc_hello_world.md .`
- Then I should get 
  ```
  Warning: Ignoring file ./index.md  
  ../docs/examples/gcc_hello_world.md  
  ```
