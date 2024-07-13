## Feature : "contains" a string feature

### Scenario: test the standard output

- When I run `pwd`
- When I run `uut create               config.ini`
- When I run `uut append mode=silent   config.ini`
- And  I run `uut append lang=fr       config.ini`
- And  I run `uut append recurse=true  config.ini`
- And  I run `uut append autosave=true config.ini`
- Then `config.ini` contains
```
Lang=fr
recurse=true
```
   - And `config.ini` contains 
```
Mode=silent
Lang=fr
```
   - And `config.ini` contains 
```
autosave=true
```
  - And `config.ini` contains 
  
  This is the exact config.ini contents (here "contains" is equivalent to "is") :

```
Mode=silent
Lang=fr
recurse=true
autosave=true
```

### Scenario : should fail

- When I run `bbt contains_multiline.keep`
- Then I get an error
- And output contains `file config.ini does not contain`
