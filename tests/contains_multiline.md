

## Feature : testing that a file contains a text

### Scenario outline : test the standard output

  -  When I run `uut -create config.ini “mode=silent”`
  -  And    I run `uut -append config.ini “lang=fr”`
  -  And    I run `uut -append config.ini “recurse=true”`
  -  And    I run `uut -append config.ini “autosave=true”`
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

 

 

 
