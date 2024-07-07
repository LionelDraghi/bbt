## Scenario: "containing" test

- Given new file `rules.txt` containing `Interfaces use is forbidden`
- When I successfully run `uut read rules.txt`
- Then I get
```
Interfaces use is forbidden
```