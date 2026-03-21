Fixme: bug 26 oct 2024 : the `Given the file whatever` is not overwriting an existing `whatever` file, even if it has not the same content.

This test should be replaced in the future by the complete test of `Given the file` / `Given the new file` with prompting.

### Background:

- Given the file `whatever.txt`
```
Cactus
```

### Scenario: 

- Given the file `whatever.txt`
```
Rose
```

- Then `whatever.txt` contains
```
Rose
```
