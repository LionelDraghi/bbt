# Feature: bbt is providing helpful messages on ill formatted step lines

# Scenario:

- Given the file `bad_steps.md`
  ~~~md
  ## Scenario: knowing grep version
  - Given the file
    Missing file name

  - Given the dir
    Missing dir name
  
  - when I run `grep --version`
    No problem on this one!
  
  - then I get file
    Missing file name
 
  - then I get dir
    Missing dir name

  - then the output should contain "grep version"
    Missing code span (the parameter should be between backticks, not double quotes)

  ## Scenario: no problem
  - when I run `grep --version`
  - then I get no error
  ~~~

- When I run `./bbt -k -c bad_steps.md`

- Then the output contains 
  ~~~
  bad_steps.md:2: Error: File name expected in subject phrase
  bad_steps.md:5: Error: Dir name expected in subject phrase
  bad_steps.md:11: Error: File name expected in object phrase
  bad_steps.md:14: Error: Dir name expected in object phrase
  bad_steps.md:20: Error: Missing Code Block expected line 17
  ~~~

- And the output contains
  ~~~  
  | Failed     | 1     |
  | Successful | 1     |
  ~~~

- And I get an error
