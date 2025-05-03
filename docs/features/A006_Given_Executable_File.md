<!-- omit from toc -->
## Feature: running scripts (Unix_Only)

btt is able to create a shell script, that will have the executable attributes thanks to the "executable" keyword. 

This is a Unix only feature. On windows, the file should be suffixed .bat or .cmd, no need to chmod +x.

And note that on Unix, without the +x you cannot run "./cmd.txt", but you still can run the script with ". ./cmd.txt" or "source cmd.txt".

_Table of Contents:_
- [Scenario: trying to run a script without the executable keyword should fail](#scenario-trying-to-run-a-script-without-the-executable-keyword-should-fail)
  - [Notes on shebang : #!/bin/bash](#notes-on-shebang--binbash)
- [Scenario: trying to run the same script created with the executable attribute should succeed](#scenario-trying-to-run-the-same-script-created-with-the-executable-attribute-should-succeed)

### Scenario: trying to run a script without the executable keyword should fail

- Given the new file `cmd1.sh` containing
~~~
#!/bin/sh
echo "bbt rules!"
~~~

#### Notes on shebang : #!/bin/bash
/bin/bash on MacOS
/usr/bin/bash on Windows, but useless
Note that there is a shebang utility on https://github.com/insomnimus/shebang to interpret the shebang prefix on Windows, but as said before, this feature is useless on Windows.

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
#!/bin/sh
echo "bbt rules!"
~~~

- When I run `./cmd2.sh`
- Then I get `bbt rules!`
