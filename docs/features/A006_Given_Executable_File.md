## Feature: running scripts

btt is able to create a shell script, that will have the executable attributes thanks to the "executable" keyword.

Note : this feature is pretty much useless. 
Without the +x, you cannot run "./cmd.txt", but you can still run the script with ". ./cmd.txt" or "source cmd.txt".
  
### Scenario: trying to run a script without the executable keyword should fail

- Given the new file `cmd1.sh` containing
~~~
echo "bbt rules!"
~~~

#### Notes on shebang : #!/bin/bash
/bin/bash on MacOS
/usr/bin/bash on Windows, but useless
both on Debian
Note that there is a shebang utility on https://github.com/insomnimus/shebang to interpret the shebang prefix on Windows

On windows, the file should be suffixed .bat or .cmd, no need to chmod +x.
Fixme : here the model is too different between Windows and Unix, we need an alternate mechanism 
to run different scripts, with different command.

- Given the file `create_exec.md` 
~~~
# Scenario:
- When I run `./cmd1.sh`
~~~

- When I run `./bbt -c --yes create_exec.md`
- Then I get an error
- And  the output contains `create_exec.md:2: Error : ./cmd1.sh not executable`

### Scenario: trying to run the same script created with the executable attribute should succeed

- Given the new executable file `cmd2.sh` containing
~~~
echo "bbt rules!"
~~~

- When I run `./cmd2.sh`
- Then I get `bbt rules!`
