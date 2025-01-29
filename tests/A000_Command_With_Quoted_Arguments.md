## Feature : Command line

bbt is using quotes for joining arguments as the shell, but the quotes are
passed to the executed program, contrary to what usual shells do.

### Scenario : command with quoted arguments
  - When I run `/usr/bin/echo        "Hello        world"`
  - Then I get no error
  - And Output is `Hello         world`