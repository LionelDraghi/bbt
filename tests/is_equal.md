## Feature : testing that an output file is equal to another file

### Scenario outline : test `output is equal` keyword

  - **When** I run `uut -h`
  - **Then** output is equal to `help_message.txt`
