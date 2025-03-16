# Scenario:

- Given the new file `simple.ads`
~~~
package Simple is
end Simple;
~~~

- And the file `scen.md`
```
# Scenario
- When I run `./sut read simple.ads`
- then the output is
~~~
package Simple is
end Simple;
~~~
```

- When I run `./bbt -em scen.md`
- Then I get no error  
