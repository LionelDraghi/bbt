<!-- omit from toc -->
## Feature : checking that some string is not present in output or file

All "doesnt", "doesn't" and "do not" are accepted in  
"output doesn't contain" and  
"file `x` doesn't contain". 

_Table of Contents:_
- [Background](#background)
- [Scenario : Successful checks](#scenario--successful-checks)
- [Scenario : Failed "output doesn't contain"](#scenario--failed-output-doesnt-contain)
- [Scenario : Failed "file doesn't contain"](#scenario--failed-file-doesnt-contain)
- [Scenario : Successful "output is not" check](#scenario--successful-output-is-not-check)
- [Scenario : Failed "output is not"](#scenario--failed-output-is-not)
- [Scenario : a negation word on a non negatable step warns](#scenario--a-negation-word-on-a-non-negatable-step-warns)

### Background

- Given file `flowers.txt`
```
Rose
Tulip
```

### Scenario : Successful checks

- When I run `./sut read flowers.txt`
- Then output contains `Rose`
- But output doesn't contain `Cactus`

- Then file `flowers.txt` contains `Tulip`
- But file `flowers.txt` do not contain `Eucalyptus`

### Scenario : Failed "output doesn't contain"

- Given the file `failed_doesnt_1.md`
```
# Scenario :
- When I run `./sut read flowers.txt`
- Then output doesn't contain `Rose`
```
- When I run `./bbt failed_doesnt_1.md`
- Then I get an error
- And output contains 
```
  - **NOK** : Then output doesn't contain `Rose` (failed_doesnt_1.md:3:)    
```
- And output contains 
```
failed_doesnt_1.md:3: Error: Output contains unexpected at line 1:  
Rose  
```

### Scenario : Failed "file doesn't contain"

- Given the file `failed_doesnt_2.md`
```
# Scenario :
- When I run `./sut read flowers.txt`
- Then file `flowers.txt` do not contain `Tulip`
```
- When I run `./bbt failed_doesnt_2.md`
- Then I get an error
- And output contains 
```
failed_doesnt_2.md:3: Error: flowers.txt contains unexpected at line 2:  
Tulip  
```

### Scenario : Successful "output is not" check

- When I run `./sut read flowers.txt`
- Then output is not `Cactus`
- But the output contains `Rose`
- And I do not get `Cactus`
- And output doesn't contain `Cactus`

### Scenario : Failed "output is not"

- Given the file `failed_is_not.md`
~~~
# Scenario :
- When I run `./sut read flowers.txt`
- Then output is not
```
Rose
Tulip
```
~~~
- When I run `./bbt failed_is_not.md`
- Then I get an error
- And output contains `**NOK** : Then output is not`
- And output contains `Output equal to unexpected content:`

### Scenario : a negation word on a non negatable step warns

A negation word in the free, non interpreted, part of a step line is
not an error: the step is parsed in its positive form, and a warning
points out that the negation is ignored.

- Given the file `warn_negation.md`
```
# Scenario :
- When I run `./sut read flowers.txt`, not twice
- Then the output contains `Rose`
```
- When I run `./bbt warn_negation.md`
- Then I get no error
- And output contains `"no"/"not" ignored: this step form has no negative variant`
