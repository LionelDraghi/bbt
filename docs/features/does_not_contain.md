# Feature : checking that some string is not present in output or file

All "doesnt", "doesn't" and "do not" are accepted in  
"output doesn't contain" and  
"file `x` doesn't contain". 


# Scenario : Successful checks

- Given file `flowers.txt`
```
Rose
Tulip
```

- When I run `./uut read flowers.txt`
- Then output contains `Rose`
- But output doesn't contain `Cactus`

- Then file `flowers.txt` contains `Tulip`
- But file `flowers.txt` do not contain `Eucalyptus`

# Scenario : Failed "output doesnt contain"

- Given the file `failed_doesnt_1.md`
```
# Scenario :
- When I run `./uut read flowers.txt`
- Then output doesn't contain `Rose`
```
- When I run `./bbt failed_doesnt_1.md`
- Then I get an error
- And output contains 
```
*** NOK : Then output doesn't contain `Rose` (failed_doesnt_1.md:3:)    
Output:    
| Rose    
| Tulip
```
- And output contains 
```
contains unexpected:    
| Rose    
```

## Scenario : Failed "file doesn't contain"

- Given the file `failed_doesnt_2.md`
```
# Scenario :
- When I run `./uut read flowers.txt`
- Then file `flowers.txt` do not contain `Tulip`
```
- When I run `./bbt failed_doesnt_2.md`
- Then I get an error
- And output contains 
```
*** NOK : Then file `flowers.txt` do not contain `Tulip` (failed_doesnt_2.md:3:)    
file flowers.txt  
| Rose    
| Tulip    
```
  
- and output contains 
```
contain unexpected:    
| Tulip    
```
