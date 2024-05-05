

## Feature : testing that a file contains a string  

### Scenario : test the standard output

  -  When I run `uut create               config.ini`
  -  When I run `uut append mode=silent   config.ini`
  -  And  I run `uut append lang=fr       config.ini`
  -  And  I run `uut append recurse=true  config.ini`
  -  And  I run `uut append autosave=true config.ini`
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
  
  This is the exact config.ini contents :

```
Mode=silent
Lang=fr
recurse=true
autosave=true
```
### Scenario : should fail
  - Then `config.ini` contains 
  
  Lang changed for "uk"   (Comment in the middle of the step!)

```
Mode=silent
Lang=uk
recurse=true
autosave=true
```
