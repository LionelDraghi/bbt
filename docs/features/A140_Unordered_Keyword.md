<!-- omit from toc -->
## Feature: ignoring the order of lines in comparisons

Comparing the expected output with a predefined list may be tedious, if, for example, the tested command list files in access time order.  
Our intent in that case is to verify that each expected line and only those lines are output.  
The comparison steps take an **`(ignoring order)`** clause that makes the comparison ignore the order of the lines, as the other ignoring patterns do (cf. [feature `ignoring`](A300_Ignoring.md)):

    - Then I get file `flowers2.txt` (ignoring order)

The clause combines with the other patterns:

    - Then file `log_a.txt` is equal to file `log_b.txt` (ignoring time and order)

The historical `unordered` adjective, written near the file name, remains
recognized and legal, but is **deprecated**: it brings nothing that the
clause does not, is less regular (it sits in the middle of the step, not
at its end) and does not combine with the other patterns. No warning is
raised for now; it will be removed in a future version.

_Table of Contents:_
- [Scenario: "I get" without the clause](#scenario-i-get-without-the-clause)
- [Scenario: same "I get" with the clause](#scenario-same-i-get-with-the-clause)
- [Scenario: same "I get" with the deprecated unordered adjective](#scenario-same-i-get-with-the-deprecated-unordered-adjective)
- [Scenario: "contains" without the clause](#scenario-contains-without-the-clause)
- [Scenario: same "contains" with the clause](#scenario-same-contains-with-the-clause)
- [Scenario: same "contains" with the deprecated unordered adjective](#scenario-same-contains-with-the-deprecated-unordered-adjective)
- [Scenario: order combines with the masking patterns](#scenario-order-combines-with-the-masking-patterns)

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

### Scenario: "I get" without the clause

- Given the file `scenario1.md`
```
# Scenario: Scenario1
- When I run `./sut read flowers1.txt`
- Then I get file `flowers2.txt`
```

- When I run `./bbt scenario1.md`
- Then I get an error

### Scenario: same "I get" with the clause

- When I run `./sut read flowers1.txt`
- Then I get file `flowers2.txt` (ignoring order)

### Scenario: same "I get" with the deprecated unordered adjective

- When I run `./sut read flowers1.txt`
- Then I get file (unordered) `flowers2.txt`

The two previous scenarios check the same thing: only the syntax differs.

### Scenario: "contains" without the clause

- Given the file `scenario1.md`
```
# Scenario: Scenario1
- When I run `./sut read flowers1.txt`
- Then output contains file `flowers3.txt`
```

- When I run `./bbt scenario1.md`
- Then I get an error
(because Rose stands before Orchids in the original file)

### Scenario: same "contains" with the clause

- When I run `./sut read flowers1.txt`
- Then output contains file `flowers3.txt` (ignoring order)

### Scenario: same "contains" with the deprecated unordered adjective

- When I run `./sut read flowers1.txt`
- Then output contains file `flowers3.txt` (unordered)

### Scenario: order combines with the masking patterns

- Given the file `build_a.txt`
```
[12:34:56] Building foo
[12:34:57] Done
```

- Given the file `build_b.txt`
```
[13:45:12] Done
[13:45:11] Building foo
```

- Then file `build_a.txt` is equal to file `build_b.txt` (ignoring time and order)

The time tags are masked on both sides, then the masked lines are
compared without regard to their order: the scenario passes although
the lines are shuffled and the times differ.
