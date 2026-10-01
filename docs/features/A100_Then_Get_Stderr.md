## Feature : stderr test

Test expected msg on the error output (see also
[A270_Then_Error_Output.md](A270_Then_Error_Output.md)).

### Scenario : missing file name

  - When I run `./sut create`
  - Then the error output is `Missing file name`
