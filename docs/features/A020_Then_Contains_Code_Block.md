<!-- omit from toc -->
## Feature : "contains" a string feature

No surprise, `contains` is the to keyword to use when checking that the output or a file contain a sequence of line.

_Table of Contents:_
- [Scenario: test the standard output](#scenario-test-the-standard-output)
- [Scenario : compare with an existing file](#scenario--compare-with-an-existing-file)

### Scenario: test the standard output

- Given `config.ini` file
```
mode=silent
lang=fr  
recurse=true 
autosave=true
```
- And the `contains_multiline.md` file
~~~
# scenario:

- then `config.ini` contains 
```
mode=silent
lang=uk
recurse=true
autosave=true
```

(the lang=uk line is wrong)
~~~


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

### Scenario : compare with an existing file

- When I run `./bbt contains_multiline.md`
- Then I get an error
- And output should contain  
```
contains_multiline.md:3: Error : config.ini does not contain expected:    
~~~
mode=silent    
lang=uk    
recurse=true    
autosave=true    
~~~
```
