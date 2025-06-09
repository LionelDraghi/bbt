## Scenario: knowing grep version
- when I run `grep --version`
- then the output should contain "grep version"