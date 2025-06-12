# Feature: bbt is providing helpful messages on ill formatted step lines

# Scenario:

- Given the file `bad_steps.md`
  ~~~
  ## Scenario: knowing grep version
  - Given the file
  - Given the dir
  - when I run `grep --version`
  - then I get file
  - then I get dir
  - then the output should contain "grep version"

  ## Scenario: no problem
  - when I run `grep --version`
  - then I get no error
  ~~~

- When I run `./bbt bad_steps.md`

- Then the output contains 
  ~~~
  bad_steps.md:2: Error : File name expected in subject phrase
  bad_steps.md:3: Error : Dir name expected in subject phrase
  bad_steps.md:5: Error : File name expected in object phrase
  bad_steps.md:6: Error : Dir name expected in object phrase
  bad_steps.md:9: Error : Missing expected Code Block expected line 7
  ~~~

- And the output contains
  ~~~  
  | Failed     | 1     |
  | Successful | 1     |
  ~~~

- And I get an error
