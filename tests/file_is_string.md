## Feature : testing that a file is equal to a single line

### Scenario outline : test on a single line file

  - When I run `uut create config.ini`
  - When I run `uut append mode=silent config.ini`
  - Then `config.ini` is equal to `mode=silent`

### Scenario outline : adding a second line, should fail

  - When I run `uut create config.ini`
  - When I run `uut append mode=silent config.ini`
  - When I run `uut append recurse=false config.ini`
  - Then file `config.ini` is `mode=silent`
 