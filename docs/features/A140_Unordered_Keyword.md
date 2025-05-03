<!-- omit from toc -->
## Feature: when the modifier `unordered` is given after `get`, order of line is ignored

Comparing the expected output with a predefined list may be tedious, if, for example, the tested command list the file in access time order.  
Our intent in that case is to verify that each expected line and only those lines are output.  
bbt provide the `unordered` modifier (that is an adjective) to be mentioned near the file name, that will cause the compare to ignore order.  

_Table of Contents:_
- [Scenario: same "contains" but with the "unordered" modifier](#scenario-same-contains-but-with-the-unordered-modifier)

### Background:

- Given the file `flowers1.txt`
```
Rose
Tulip
Orchids
Petunia
```

- Given the file `flowers2.txt`
```
Tulip
Petunia
Rose
Orchids
```

- Given the file `flowers3.txt`
```
Tulip
Rose
```

### Scenario: "I get" without the modifier

- Given the file `scenario1.md`
```
# Scenario: Scenario1
- When I run `./sut read flowers1.txt`
- Then I get file `flowers2.txt`
```

- When I run `./bbt scenario1.md`
- Then I get an error

### Scenario: same "I get" with the modifier
- When I run `./sut read flowers1.txt`
- Then I get file (unordered) `flowers2.txt`

## Scenario: "contains" without the modifier

- Given the file `scenario1.md`
```
# Scenario: Scenario1
- When I run `./sut read flowers1.txt`
- Then output contains file `flowers3.txt`
```

- When I run `./bbt scenario1.md`
- Then I get an error
(because Rose stands before Orchids in the original file)

# Scenario: same "contains" but with the "unordered" modifier

- When I run `./sut read flowers1.txt`
- Then output contains file `flowers3.txt` (unordered)

