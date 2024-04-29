## Feature : testing that an output file is equal to another file

### Scenario outline : test `output is equal` keyword

  - **Given** file help_message.txt

```
uut …..  

….
```
  - **When** I run `uut -h`
  - **Then** output is equal to `help_message.txt`

Synonymes :

  Stderr = error output  
  Stdout = output