<!-- omit from toc -->
## Features: Exact match versus Human match

bbt default behavior is pretty unusual, because it ignores difference between expected and actual results in casing, white spaces and blank lines.
This is what we call "Human match".

This behavior is usually handy, but prevent testing apps that precisely transform the casing or more generally the text presentation.
To allow those kind of tests, there is a command line option that completely switch the default behavior, `--exact_match`.

It is possible to amend `--exact_match` with one or more of `--ignore_casing`, `--ignore_whitespaces` and `--ignore_blank_lines`.
Each of those option returns to one of the default behavior.

This test scenario is organized around :
- a reference file
- a file that differ from the reference only in casing;
- a file that differ from the reference only in white spaces;
- a file that differ from the reference only in blank lines.

and a scenario that compare the ref file with the three other.

This scenario is run with the various options.

_Table of Contents_:
- [Background:](#background)
- [Scenario: Human match](#scenario-human-match)
- [Scenario: exact match](#scenario-exact-match)
- [Scenario: exact match except for casing](#scenario-exact-match-except-for-casing)
- [Scenario: exact match except for casing and blank lines](#scenario-exact-match-except-for-casing-and-blank-lines)

### Background:

- Given the file `text.ref`
```
Rose
Tulip
```

- Given the file `text.1`
```
rose
Tulip
```

- Given the file `text.2`
```
Rose
  Tulip
```

- Given the file `text.3`
```

  
Rose

Tulip
```

- Given the `compare.md` file
~~~
# Scenario 1:
- Then `text.ref` is equal to file `text.1`
# Scenario 2:
- Then `text.ref` is equal to file `text.2`
# Scenario 3:
- Then `text.ref` is equal to file `text.3`
~~~

### Scenario: Human match

- When I run `./bbt compare.md`
- Then I get no error
- When I run `./bbt --human_match compare.md`
- Then I get no error


### Scenario: exact match

- When I run `./bbt -k --exact_match compare.md`
- Then I get an error
  The three tests should fail :
- And output contains
~~~
*** NOK : Then `text.ref` is equal to file `text.1` (compare.md:2:)    
~~~
- And output contains
~~~
*** NOK : Then `text.ref` is equal to file `text.2` (compare.md:4:)    
~~~
- And output contains
~~~
*** NOK : Then `text.ref` is equal to file `text.3` (compare.md:6:)    
~~~

### Scenario: exact match except for casing

- When I run `./bbt -k -em -ic compare.md`
- Then I get an error
  Two of the three tests should fail :
- And output contains
~~~
  - [X] scenario [1:](compare.md) pass    
~~~
- And output contains
~~~
*** NOK : Then `text.ref` is equal to file `text.2` (compare.md:4:)    
~~~
- And output contains
~~~
*** NOK : Then `text.ref` is equal to file `text.3` (compare.md:6:)    
~~~

### Scenario: exact match except for casing and blank lines

- When I run `./bbt -k -em --ignore_casing -ibl compare.md`
- Then I get an error
  Two of the three tests should fail :
- And output contains
~~~
  - [X] scenario [1:](compare.md) pass    
~~~
- And output contains
~~~
*** NOK : Then `text.ref` is equal to file `text.2` (compare.md:4:)    
~~~
- And output contains
~~~
  - [X] scenario [3:](compare.md) pass    
~~~
