## Scenario: knowing grep version
- Given the file
- Given the dir
- when I run `grep --version`
- then I get file
- then I get dir
- then the output should contain "grep version"

## Scenario: no problem
- when I run `grep --version`
- then i get no error