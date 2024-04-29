## Feature : checking an expected multiline output

Test an app returning a long text

### Scenario : asking for uut help

  - When I run `uut -h`
  - Then I get 

```
uut …..
…..
```

### Scenario : causing an uut error with a long explanation

  - When I run `uut -e append `
  - Then I get on stderr

```
Cannot append to file
…..
```

 