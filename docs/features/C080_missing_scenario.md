## Feature

The only constraint (for now) for bbt scenario is that there should be at least one scenario.
When there is none, it deserve a clear error message!

### Scenario : No scenario
- Given the file `No_Scenario.md`
~~~
- When I run `./sut --version`
- Then I get `God save the Queen`
Raise an error if executed.
But should not be executed, unless -k is used.
~~~

- When I run `./bbt No_Scenario.md`
- Then output contains 
~~~
Warning : No scenario in document "No_Scenario.md"
~~~
- And output contains 
~~~
- Empty      =  1
~~~
