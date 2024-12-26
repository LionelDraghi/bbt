## Feature

When a command fails, bbt make it clear to the user if it was a spawn fail, that is bbt wasn't able to find or to run the command, or a command fail, that is the command returns an error code.

### scenario: successful run with no error
- when i run `./bbt -lf`
- then I get no error
- and  I get no output

### scenario: command run with error return code
- when i run `./bbt -c`
- then I get error `1`
- and  I get `Error : No md file found`

### scenario: trying to run an unknown command
- when i run `xxx -h`
- then I get error `55`
- and  I get `xxxx`
