# Feature: when the modifyer `unordered` is given after `get`, order of line is ignored

# Background:

- Given the file `flowers.txt`
```
Rose
Tulip
Orchids
Petunia
```

# Scenario: "I get" without the modifyer

- Given the file `scenario1.md`
```
# Scenario: Scenario1
- When I run `uut read flowers.txt`
- Then I get 
~~~
Orchids
Tulip
Petunia
Rose
~~~
```

- When I run `bbt scenario1.md`
- Then I get an error

# Scenario: same "I get" with the modifyer
- When I run `uut read flowers.txt`
- Then I get (unordered)
~~~
Orchids
Tulip
Petunia
Rose
~~~
