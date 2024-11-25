## Feature : file is equal to a file

Expected results may be very long.  
It's handy to be able to keep the expected output in an external file.  

### Scenario : test `is equal to file` form

- Given the file `tmp.1`
```
I’ve been trying this out a bit on Windows, and it seems like a great fit to test something that I’m working on.
```
- And the file `tmp.2`
```
I’ve been trying this out a bit on Windows, and it seems like a great fit to test something that I’m working on.
```

- Then `tmp.1` is equal to file `tmp.2`
