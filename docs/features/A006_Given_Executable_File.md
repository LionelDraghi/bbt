## Feature: running scripts

btt is able to create a shell script, that will have the executable attributes thanks to the "executable" keyword.
  
### Scenario: trying to run a script without the executable keyword should fail

- Given the file `cmd1.sh` containing
~~~
#!/usr/bin/bash
echo "bbt rules!"
~~~

- Given the file `create_exec.md` 
~~~
# Scenario:
- When I run `./cmd1.sh`
~~~

- When I run `./bbt -c --yes create_exec.md`
- Then I get an error
- And  the output contains `create_exec.md:2: Error : ./cmd1.sh not executable`

### Scenario: trying to run the same script created with the executable attribute should succeed

- Given the executable file `cmd2.sh` containing
~~~
#!/usr/bin/bash
echo "bbt rules!"
~~~

- When I run `./cmd2.sh`

- Then I get `bbt rules!`
