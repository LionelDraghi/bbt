# Scenario : Checking that a command fails

- Then `./sut -zwq` fails 
- And  the output matches `.*Opening and ending tag mismatch.*`

- And  `./sut -zwq` should fail 
- And  the output matches `.*Opening and ending tag mismatch.*`

