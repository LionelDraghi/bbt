# Feature: when the modifyer `unordered` is given after `contains`, order of line is ignored

# Background:

- Given the file `flowers.txt`
```
Rose
Tulip
Orchids
Petunia
```

# Scenario: "contains" without the modifyer

- Given the file `scenario1.md`
```
# Scenario: Scenario1
- When I run `uut read flowers.txt`
- Then output contains 
~~~
Rose
Orchids
~~~
```

- When I run `bbt scenario1.md`
- Then I get an error
(because Rose stands after Orchids in the original file)

# Scenario: same "contains" but with the "unordered" modifyer

- When I run `uut read flowers.txt`
- Then output contains (unordered)
~~~
Rose
Orchids
~~~
```
