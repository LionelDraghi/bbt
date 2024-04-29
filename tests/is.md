

## Feature : testing that a file is equal to a given text

### Scenario : test that should pass

   - When I run `uut -create config.ini “mode=silent”`
   - And  I run `uut -append config.ini “lang=fr”`
   - Then `config.ini` is  
```
Mode=silent
Lang=fr
```

### Scenario : test that should fail

Note the code fence surrounding expected file, with tildes instead of backtick 

   - When I run `uut -create config.ini “mode=silent”`
   - And    I run `uut -append config.ini “lang=fr”`
   - And    I run `uut -append config.ini “site=http:”`
   - Then `config.ini` is  
~~~
Mode=silent
Lang=fr
~~~
