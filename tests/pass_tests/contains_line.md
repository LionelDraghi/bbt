## Feature : testing that a file contains a line

### Scenario outline : test on a one line output

  - When I run `uut -v`
  - Then the output should contains `version`

### Scenario outline : test on a multiline output

  - When I run `uut -h`
  - Then the output should contains `--version`

### Scenario outline : test on a file

  - When I run `uut create config.ini`
  - When I run `uut append mode=silent config.ini`
  - When I run `uut append recurse=false config.ini`
  - Then `config.ini` contains `mode=silent`

 