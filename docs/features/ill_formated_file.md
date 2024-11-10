# Feature: Ill formatted file detection

`bbt` aim at giving helpful to help user on errors in writing file.

## Scenario: Missing scenario

A classic error is to forget the heading marker `#` 

- Given the new file `no_heading_marker.input`
```md
Background: 
- When I successfully run `uut --version`
```

- When I run `./bbt -d no_heading_marker.input`
```
- Then output contain `Warning : No scenario in document "no_heading_marker.input`
- ANd  output contain `Empty      tests =  1`
```
