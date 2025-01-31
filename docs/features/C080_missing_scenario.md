## Feature

The only constraint (for now) for bbt scenario is that there should be at least one scenario.
When there is none, it deserve a clear error message!

### Scenario : Steps without scenario header
- Given the file `No_Scenario.md`
~~~
- When I run `./sut --version`
- Then I get `God save the Queen`
~~~

- When I run `./bbt No_Scenario.md`
- Then output contains 
~~~
Warning : No scenario in document "No_Scenario.md"
~~~
- And output contains 
~~~
| Empty      |  1
~~~

### Scenario : Empty file
- Given the file `empty.md`
~~~
~~~

- When I run `./bbt empty.md`
- Then output contains 
~~~
Warning : No scenario in document "empty.md"
~~~
- And output contains 
~~~
| Empty      |  1
~~~
