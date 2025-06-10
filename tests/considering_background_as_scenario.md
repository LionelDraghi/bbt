# Feature:

# Background:

- Given the file `cities.lst` 
  ~~~
  London
  Paris
  Roma
  ~~~

## Scenario: basic grep test

- When I run `grep Roma cities.lst` 

- Then I get `Roma`

## Scenario: case insensitive grep

- When I run `grep -i paris cities.lst` 

- Then I get `Paris`