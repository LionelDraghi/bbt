# Feature: --keep_going option

During debug, bbt help the developer focusing on the first problem by just stopping the run.
On the other hand, in a batch run, one usually wants to run all tests even if some fails.
btt allows this second behavior through the `--keep_going` (or `-k`) option.  

## Background:

- Given the file `flowers.txt`
```
Rose
Tulip
Orchids
```

- And the file `scenario1.md`
```
# Scenario: 
- Then file `flowers.txt` contains `Orchids`
- Then file `flowers.txt` contains `Petunia`
- Then file `flowers.txt` contains `Tulip`
```

The second assertion is False.


## Scenario: default behavior
The scenario is run without the --keep_going option`, bbt should exit after the second assertion.

- When I run `bbt -v scenario1.md`
- Then output should contain "OK  : Then file `flowers.txt` contains `Orchids`"
- Then output should contain "NOK : Then file `flowers.txt` contains `Petunia`"
- Then output should not contain "OK  : Then file `flowers.txt` contains `Tulip`"

## Scenario: "Don't stop me now"
The same scenario is run with the `--keep_going` option, bbt should finish and run also the third assertion.

- When I run `bbt -v --keep_going scenario1.md`
- Then output should contain "OK  : Then file `flowers.txt` contains `Orchids`"
- Then output should contain "NOK : Then file `flowers.txt` contains `Petunia`"
- Then output should contain "OK  : Then file `flowers.txt` contains `Tulip`"
