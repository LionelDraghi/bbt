### Scenario: Comparing ignoring blank lines

bbt default behavior is to ignore blank lines.
There is no keyword to change this setting right now.
When implemented, another scenario will be added here, and the file renamed `A160_xxx_keyword.md`.

- Given the file `le Cid.txt`
```  
– Va, je ne te hais point.
– Tu le dois.


– Je ne puis. […]
– Ô miracle d'amour !
– Ô comble de misères !

```  

- Then file `le Cid.txt` is
```  

– Va, je ne te hais point.

– Tu le dois.
– Je ne puis. […]

– Ô miracle d'amour !
– Ô comble de misères !


```  

- And the file `le Cid.txt` contains
```  

– Va, je ne te hais point.

– Tu le dois.

– Je ne puis. […]


```  
