## Feature

When running a command, bbt try to locate the first parameter. 
If it's an existing app (for example "/usr/bin/gcc").
If not, we try to locate it in the PATH. 
If not, we put explicit error "command not found".

The return code is the one of the command if it could be run, error otherwise.

When a command fails, bbt make it clear to the user if it was a spawn fail, that is bbt wasn't able to find or to run the command, or a command fail (that is the command returns an error code).

### scenario: the command relative path is given
- when i run `./bbt -lf`
- then I get no error
- and  I get no output

### scenario: the command is in the PATH
([Issue #15](https://github.com/LionelDraghi/bbt/issues/15))

- when i run `git --version`
- then I get no error
- and  output contains `git version`

### scenario: command not found
([Issue #6](https://github.com/LionelDraghi/bbt/issues/6))

- Given the `cmd_not_found.md` file 
~~~ 
# Scenario:
- when i run `xyzabc -v`
- then I get an error
- and  I get `xyzabc not found`
~~~ 
- When I run `./bbt -c cmd_not_found.md`
- Then I get an error
- And  the output contains `cmd_not_found.md:2: Error : xyzabc not found`

### scenario: the command is not executable (Unix_Only)

On Windows, Spawn return Success even if the file is not executable.

- Given the `lambda_file` containing `nothing`
- Given the `cmd_not_exe.md` file 
~~~ 
# Scenario:
- when I run `./lambda_file -v`
~~~ 
- When I run `./bbt -c cmd_not_exe.md`
- Then I get an error
- And  the output contains `cmd_not_exe.md:2: Error : ./lambda_file not executable`
