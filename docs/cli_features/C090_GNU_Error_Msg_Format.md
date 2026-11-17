<!-- omit from toc -->
## Feature : GNU error messages
   
bbt error and warning messages are prefixed with line and column number using [the GNU Standard](https://www.gnu.org/prep/standards/html_node/Errors.html),
That is :
> program:sourcefile:lineno: message
when there is an appropriate source file, or :
> program: message
otherwise.

_Table of Contents:_
- [Scenario:](#scenario)

# Scenario: 

We create a scenario with multiple When, and run it with the "strict" option, in order to generate Warnings.

- Given the file `t1.md`
 ```
  # Scenario
  - When I run `./sut create tmp.txt`
  - And  I run `./sut append fillet  tmp.txt`
  - And  I run `./sut append chamfer tmp.txt`
  ```

- When I successfully run `./bbt --strict t1.md`
- Then the output contains `t1.md:0: Warning: Multiple When in the same Scenario` (ignoring location)
- But the output does not contain `t1.md:0: Warning: Multiple When in the same Scenario`

The actual warnings point at the real `t1.md` lines, so the literal
location 0 of the expected text is never in the output: the `But` step
proves that the `Then` step matches only thanks to the `ignoring
location` clause.
