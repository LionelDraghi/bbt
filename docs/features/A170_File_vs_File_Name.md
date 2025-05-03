<!-- omit from toc -->
## Feature : expected content in a file

Steps with an expected content may ends with a String :  

> - Then the output contains `20 files processed`

The string may also denote a file name, where bbt is supposed to find the expected content.

> - Then the output contains **file** `expected.txt`

As we are using the same code span format for both, the keyword **file** make the difference.

_Table of Contents:_
- [Scenario: "Then I get file" form](#scenario-then-i-get-file-form)
- [Scenario: "Then output is | contains | does not contain file" form](#scenario-then-output-is--contains--does-not-contain-file-form)
- [Scenario: "Then file is | contains | does not contain file" form](#scenario-then-file-is--contains--does-not-contain-file-form)

### Scenario: "Then I get file" form
- Given the file `list_1.txt`
```
Rose
```
- Given the file `list_2.txt`
```
Rose
Tulip
```
  
- When I run `./sut read list_1.txt`
- Then I get file `list_1.txt`

### Scenario: "Then output is | contains | does not contain file" form
- When I run `./sut read list_1.txt`
- Then output is the `list_1.txt` file
  (the **file** keyword may be put after the file name)
- Then output contains file `list_1.txt`
- Then output does not contain file `list_2.txt`

### Scenario: "Then file is | contains | does not contain file" form
- Then `list_2.txt` contains file `list_1.txt`
- Then `list_1.txt` does not contain file `list_2.txt`

