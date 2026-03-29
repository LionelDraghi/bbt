# Scenario : Running something in postcondition

To check a final state, you sometime need to run some utilities, for example lint.

- Given the new file `simple.xml`
~~~
<?xml version="1.0" encoding="UTF-8"?>
<agreement>
    <partyA>Name of Party A</partyA>
</agreement>
~~~

- Then I successfully run `xmllint simple.xml` 
  (to check that the xml structure is OK)
