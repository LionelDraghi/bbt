# Feature: bbt is providing helpful messages on ill formatted step lines

Partial formater are very tolerant to alternate wording. 
The downside being that it is more difficult to identify grammar problem.
Here are some detected problem.

# Scenario:

- Given the new file `bad_steps.md`
  ~~~md
  ## Scenario: knowing grep version

  - and I run `grep --version`
    A scenario cannot start with "and" or "but", otherwise we cannot determine which kind of step it is.

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
  ~~~
  bad_steps.md:3: Error: No previous step to determine step kind of "and I run `grep --version`"
  bad_steps.md:6: Error: File name expected in subject phrase (should be between backticks)  
  bad_steps.md:9: Error: Dir name expected in subject phrase (should be between backticks)  
  bad_steps.md:12: Error: Unrecognized step "when I run "  
  bad_steps.md:15: Error: File name expected in object phrase (should be between backticks)  
  bad_steps.md:18: Error: Unrecognized step "then I get dir"  
  bad_steps.md:24: Error: Missing Code Block expected line 21 
  ~~~
  Here we first check the most important part, error messages.  

- And the output contains 
  ~~~
  bad_steps.md:3: Warning: Skipping step with syntax error  
  bad_steps.md:6: Warning: Skipping step with syntax error  
  bad_steps.md:9: Warning: Skipping step with syntax error  
  bad_steps.md:12: Warning: Skipping step with syntax error  
  bad_steps.md:15: Warning: Skipping step with syntax error  
  bad_steps.md:18: Warning: Skipping step with syntax error 
  bad_steps.md:21: Warning: Skipping step with syntax error  
  - OK : when I run `grep --version`  
  ~~~
  Then we also check that when using `--keep_going` option, skipped steps are explicit.  
  Note the subtlety here respect to analysis here above : the error on the step line 17 was detected on line 24.  
  But the erroneous step that is skipped is the one line 21.  
  The step line 24 is run.

- And the output contains
  ~~~  
  | Failed     | 1     |
  | Successful | 1     |
  ~~~

- And I get an error
