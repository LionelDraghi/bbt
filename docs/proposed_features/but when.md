`grep` can perform a case-insensitive search.

## Example 1: Case-insensitive search

- Given the file `Cities.txt`
  ~~~
  Barcelona
  Lima
  Ankara
  Osaka
  Tulsa
  ~~~

- When I run `grep -i barcelona Cities.txt`
- Then the output is `Barcelona`

- and When I run `grep barcelona Cities.txt`
- Then there is no output