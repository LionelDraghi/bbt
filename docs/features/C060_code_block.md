### Scenario: Code fenced blocks

bbt uses code blocks in its own syntax, while pretending to let the user the freedom to also use code blocks in it's comments.

This scenario puts code blocks pretty much everywhere, and check that only code blocks following a step are taken into account.
The code blocks that should be taken into account contain `Rose`, code blocks that should be ignored contains `Lily` 

- Given the file `lot_of_code_blocks.md`
~~~md
```
Lily
Popy
```

## Feature: 

```
Lily
Popy
```

```
Lily
Popy
```

### Background:

```
Lily
Popy
```

### Scenario:
```
Lily
Popy
```

- Given the file `flowers.txt`
```
Rose
Cactus
```
```
Lily
Popy
```
```
Daisy
Popy
```
Note here that the second code block should not be added to the first one!

- When I run `./sut read flowers.txt`

```
Lily
Popy
```

- Then I get
```
Rose
Cactus
```
```
Lily
Daisy
Sunflower
Popy
```
~~~

- When I run `./bbt -c lot_of_code_blocks.md`
- Then I get no error
```
- And output contains  
```
Success, 1 scenarios OK
```
