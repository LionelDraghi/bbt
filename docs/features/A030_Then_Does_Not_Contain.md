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
contains unexpected:    
~~~
Rose        
~~~
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
failed_doesnt_2.md:3: Error : flowers.txt shouldn't contain :    
~~~  
Tulip  
~~~  
```
