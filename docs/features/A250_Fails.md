# Scenario : Checking that a command fails

- Given the new file `mismatched_tag.xml`
~~~
<?xml version="1.0" encoding="UTF-8"?>
<agreement>
    <partyA>Name of Party A</partyB>
</agreement>
~~~

**(notice the mismatched closing tag `</partyB>` instead of `</partyA>`)**  

- Then `xmllint mismatched_tag.xml` fails 
- And  the output matches `.*Opening and ending tag mismatch.*`

- And  `xmllint mismatched_tag.xml` should fail 
- And  the output matches `.*Opening and ending tag mismatch.*`

