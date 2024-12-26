## Feature

When a command fails, bbt make it clear to the user if it was a spawn fail, that is bbt wasn't able to find or to run the command, or a command fail, that is the command returns an error code.

### scenario: successful run with no error
- when i run `./bbt -lf`
- then I get 
```
pas glop
```

