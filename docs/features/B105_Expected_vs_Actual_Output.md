<!-- omit from toc -->
## Features : run results are output in Markdown format

To easily spot diffs in expected output, *bbt* is using an sdiff like output format,
with `-s` options, that is without common lines.

_Table of Contents:_
- [Background:](#background)
  - [Scenario: full comparison with no error](#scenario-full-comparison-with-no-error)
  - [Scenario: full comparison with error in the middle of the file](#scenario-full-comparison-with-error-in-the-middle-of-the-file)
  - [Scenario: full comparison with blank lines on the left and on the right](#scenario-full-comparison-with-blank-lines-on-the-left-and-on-the-right)
    - [Scenario: full comparison with an inserted line](#scenario-full-comparison-with-an-inserted-line)

## Background:

- Given the new file `reference.txt`
  ~~~
  The quick brown fox
  jumps over the lazy dog
  This is a test file
  for diff operations
  It contains exactly
  ten lines of text

  Each line is unique
  and clearly identified
  for easy comparison
  in diff tests
  ~~~

### Scenario: full comparison with no error

- Then file `reference.txt` is
  ```
  The quick brown fox
  jumps over the lazy dog
  This is a test file
  for diff operations
  It contains exactly
  ten lines of text
  
  Each line is unique
  and clearly identified
  for easy comparison
  in diff tests
  ```

### Scenario: full comparison with error in the middle of the file

- Given the new file `scen1.md`
  ~~~
  # Scenario
  - Given the new file `input.1`
    ```
    The quick brown fox
    jumps over the lazy dog
    This is a test file
    for diff operations
    It should contains more or less
    ten lines of text
    
    Each line is unique
    and clearly identified
    for easy comparison
    in diff tests
    ```
  - Then file `input.1` is equal to file `reference.txt` 
  ~~~


- When I run `./bbt --yes scen1.md`

- Then the output contains
  ```md
scen1.md:16: Error: input.1 not equal to expected 
@@ -5 +5 @@
  jumps over the lazy dog
  This is a test file
  for diff operations
  It contains exactly      |     It should contains more or less
  ```

- And the output contains
  ```
    - [ ] scenario [](scen1.md) **fails**
  ```

### Scenario: full comparison with blank lines on the left and on the right

- Given the new file `scen2.md`
  ~~~
  # Scenario
  - Given the new file `input.2`
    ```
    The quick brown fox
    jumps over the lazy dog
    This is a test file
    for diff operations
    It contains exactly
    ten lines of text
    Each line is unique
    and clearly identified
    
    for easy comparison
    in diff tests
    ```
  - Then file `input.2` is equal to file `reference.txt` 
  ~~~


- Then I successfully run `./bbt --cleanup --yes scen2.md`

But

- When I run `./bbt --exact_match --cleanup --yes scen2.md`

- Then there is an error 

- And the output contains 
  ```md
scen2.md:16: Error: input.2 not equal to expected   
@@ -1,11 +1,11 @@
  ```

- And the output contains
  ```
                           |     Each line is unique
  Each line is unique      |     and clearly identified
  and clearly identified   |      
  ```

- And the output contains
  ```
    - [ ] scenario [](scen2.md) **fails**
  ```

### Scenario: full comparison with an inserted line

The expected and actual texts are aligned before displaying the
differences, as `diff` or `git diff` do
(cf. the [design discussion D10](../dev/design_discussions.md#d10-diff-display-alignment)):
a line inserted in the actual text is reported as an insertion, and must
not turn the following lines into changed pairs.

- Given the new file `scen3.md`
  ~~~
  # Scenario
  - Given the new file `input.3`
    ```
    The quick brown fox
    jumps over the lazy dog
    an inserted line
    This is a test file
    for diff operations
    It contains exactly
    ten lines of text
    
    Each line is unique
    and clearly identified
    for easy comparison
    in diff tests
    ```
  - Then file `input.3` is equal to file `reference.txt` 
  ~~~

- When I run `./bbt --cleanup --yes scen3.md`

- Then there is an error 

- And the output contains
  ```md
scen3.md:17: Error: input.3 not equal to expected:
@@ -2,0 +3 @@
The quick brown fox
jumps over the lazy dog
                         >   an inserted line
This is a test file
for diff operations
It contains exactly
  ```

- And the output does not contain `This is a test file | an inserted line`

- And the output contains
  ```
    - [ ] scenario   [](scen3.md) **fails**
  ```








