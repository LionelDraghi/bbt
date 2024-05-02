

## Feature : testing that a file contains a text

### Scenario outline : test the standard output

  -  When I run `uut create mode=silent config.ini `
  -  And  I run `uut append lang=fr config.ini`
  -  And  I run `uut append recurse=true config.ini`
  -  And  I run `uut append config.ini autosave=true config.ini`
  -  Then `config.ini` contains 
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
```
Mode=silent
Lang=fr
recurse=true
autosave=true
```
   - But `config.ini` do not contains 
```
Mode=silent
Lang=uk
recurse=true
autosave=true
```

 

 

 
