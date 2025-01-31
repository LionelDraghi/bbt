### Scenario : test that `is equal to file` no more raise an exception when files are of different sizes, check Issue: #7 

- Given the file `tmp.1`
```
I’ve been trying this out a bit on Windows, and it seems like a great fit to test something that I’m working on.
```
- Given the file `tmp.2`
```
I’ve been trying this out a bit on Windows, and it seems like a great fit to test something that I’m working on.
One more line.
```

- And the file `test_that_should_fail.md`
```
### Scenario
- Then `tmp.1` is equal to file `tmp.2`
- Then `tmp.2` is equal to file `tmp.1`
```

- When I run `./bbt test_that_should_fail.md`
- Then output contains `| Failed     |  1`
- Then output do not contain `Exception`
- And I get an error

