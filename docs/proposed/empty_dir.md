
## Feature: test empty directory detection / Creation

## Scenario: no Dir1, test creation

- Given there is no dir `dir1`
- Given the new directory `dir1`
- Then there is a directory `dir1`

## Scenario: dir1 is not empty (should fail)

- Given file `dir1/tmp.txt`
```
Mémoire d'outre-tombe
---------------------
```
- Given empty directory `dir1`

