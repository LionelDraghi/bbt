## Feature : testing that a file contains a line

### Scenario outline : test the standard output

  - When I run `uut -h`
  - Then stdout contains "-v”

### Scenario outline : test on a file

  - When I run `uut -write config.ini “mode=silent”`
  - Then `config.ini` contains `mode=silent`

 