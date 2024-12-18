## Feature : GNU error messages
   
bbt error and warning messages are prefixed with line and column number using [the GNU Standard](https://www.gnu.org/prep/standards/html_node/Errors).html),
That is :
> program:sourcefile:lineno: message
when there is an appropriate source file, or :
> program: message
otherwise.

This is a good test case for bbt pattern!

Scenario: 
- Given the file t1.md
 ```
  # Scenario
  - When I run `./sut create tmp.txt`
  - When I run `./sut append fillet  tmp.txt`
  - When I run `./sut append chamfer tmp.txt`
  ```

- When I successfully run `./bbt --strict t1.md`
- Then the output contains `t1.md:[0-9]*: Warning`
- And the output contain          `t1.md:[0-9]*: Warn.ng`
- But the output does not contain `t1.md:[0-9]*: Warn..ng`