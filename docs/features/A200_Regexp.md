<!-- omit from toc -->
## Feature: identifying expected output with regexp 

You sometimes need to check the output not for an exact match, but for a pattern.  
bbt uses the specific `match` / `matches` keyword for that purpose.  
bbt is using simple regexp (no posix subtleties for now).  

Note that the regexp should match the whole line :
- to find `word` in a line you should use `.*word.*`
- to find line ending with word, use `.*word`

_Table of Contents:_
- [scenario: version number match](#scenario-version-number-match)
- [scenario: version number mismatch](#scenario-version-number-mismatch)
- [Scenario: Test of "does not match"](#scenario-test-of-does-not-match)
- [Scenario: Test of "does not match" that indeed matches](#scenario-test-of-does-not-match-that-indeed-matches)

# scenario: version number match

- When I run `./sut -v`
- Then output matches `sut version [0-9]+\.[0-9]+`

# scenario: version number mismatch

- Given the new file `wrong_regexp.md`
~~~
# Scenario:
- When I run `./sut -v`
- Then output matches `sut version [0-9]+\.[0-9]+\.[0-9]+`
~~~

- When I run `./bbt wrong_regexp.md`
- Then I get an error
- and output contains 
```
sut version 1.0  
~~~  
does not match expected:  sut version [0-9]+\.[0-9]+\.[0-9]+    
```

# Scenario: Test of "does not match" 

- When I run `./sut -v`
Let's check that the number is of the form "major.minor" and not "major.minor.patch"
- Then output does not match `sut version [0-9]+\.[0-9]+\.[0-9]+`

# Scenario: Test of "does not match" that indeed matches

- Given the new file `wrong_regexp.md`
~~~
# Scenario:
- When I run `./sut -v`
- Then output does not match `sut version [0-9]+\.[0-9]+`
~~~
- When I run `./bbt wrong_regexp.md`
- Then I get an error
- and output contains 
```
sut version 1.0  
~~~  
match unexpected:  sut version [0-9]+\.[0-9]+
```
