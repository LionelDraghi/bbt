# Feature: bbt is providing helpful messages on ill formatted step lines

Partial formater are very tolerant to alternate wording. 
The downside being that it is more difficult to identify grammar problem.
Here are some detected problem.

# Scenario:

- Given the new file `bad_steps.md`
  ~~~md
  ## Scenario: knowing grep version
  - Given the file
    Missing file name

  - Given the dir
    Missing dir name
  
  - when I run 
    Missing command to run
  
  - then I get file
    Missing file name
 
  - then I get dir
    Missing dir name

  - then the output should contain "grep version"
    Missing code span (the parameter should be between backticks, not double quotes)

  - when I run `grep --version`
    This one is OK

  ## Scenario: no problem
  - when I run `grep --version`
  - then I get no error
  ~~~

- When I run `./bbt -k -c bad_steps.md`

- Then the output contains  
  Most important part, we check the error messages
  ~~~
  bad_steps.md:2: Error: File name expected in subject phrase (should be between backticks)  
  bad_steps.md:5: Error: Dir name expected in subject phrase (should be between backticks)  
  bad_steps.md:8: Error: Unrecognized step "when I run "  
  bad_steps.md:11: Error: File name expected in object phrase (should be between backticks)  
  bad_steps.md:14: Error: Unrecognized step "then I get dir"  
  bad_steps.md:20: Error: Missing Code Block expected line 17 
  ~~~

- And the output contains  
  We also check that when using --keep_going option, skipped steps are explicit.
  Note the subtlety here respect to analysis here above : the error on the step line 17 was detected on line 20. 
  But the erroneous step that is skipped is the one line 17.
  The step line 20 is run.
  ~~~
  bad_steps.md:2: Warning: Skipping step with syntax error  
  bad_steps.md:5: Warning: Skipping step with syntax error  
  bad_steps.md:8: Warning: Skipping step with syntax error  
  bad_steps.md:11: Warning: Skipping step with syntax error  
  bad_steps.md:14: Warning: Skipping step with syntax error 
  bad_steps.md:17: Warning: Skipping step with syntax error  
  - OK : when I run `grep --version`  
~~~

- And the output contains
  ~~~  
  | Failed     | 1     |
  | Successful | 1     |
  ~~~

- And I get an error
