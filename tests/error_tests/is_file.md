

## Feature : testing that a file is equal to a given text

### Scenario : test that should pass

   - When running `uut create config.ini `
   - And  I run   `uut append mode=silent config.ini `

Note the test of the "running" keyword, the casing difference on Mode= vs mode=, and the comment in the middle of the steps.

   - And  I run   `uut append lang=fr config.ini `
   - Then `config.ini` is  


```
Mode=silent
Lang=fr
```

### Scenario : test that should fail

Note the code fence surrounding expected file, with tildes instead of backtick 

   - When I run `uut append site=http:  config.ini `
   - Then `config.ini` is  
~~~
Mode=silent
Lang=fr
~~~
