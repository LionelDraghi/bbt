<!-- omit from toc -->
# Feature: Ignoring blank lines

bbt default behavior is to ignore blank lines.
There is no keyword to change this setting right now, but there is command line option, --exact_match.
Cf. to `bbt -h` for more details.

- [Background](#background)
- [Scenario : some obvious tests](#scenario--some-obvious-tests)
- [Scenario : default behavior, non sensible to blank lines](#scenario--default-behavior-non-sensible-to-blank-lines)
- [scenario : with --exact\_match, sensible to blank lines](#scenario--with---exact_match-sensible-to-blank-lines)
- [Scenario : with --exact\_match and --ignore\_blank\_lines, non sensible to blank lines](#scenario--with---exact_match-and---ignore_blank_lines-non-sensible-to-blank-lines)

## Background
- Given the file `le_Cid_1.txt`
```  
– Va, je ne te hais point.
– Tu le dois.


– Je ne puis. […]
– Ô miracle d'amour !
– Ô comble de misères !

```  

- Given the file `le_Cid_2.txt` 
```  

– Va, je ne te hais point.

– Tu le dois.
– Je ne puis. […]

– Ô miracle d'amour !
– Ô comble de misères !


```  

- Given the file `le_Cid_3.txt` 
```  

– Va, je ne te hais point.

– Tu le dois.

– Je ne puis. […]
```

- Given the file `is.md`
~~~
# Scenario
- Then `le_Cid_1.txt` is equal to the file `le_Cid_2.txt`
~~~

- Given the file `contains.md`
~~~
# Scenario
- Then `le_Cid_1.txt` contains the file `le_Cid_3.txt`
~~~

## Scenario : some obvious tests
- Then file `le_Cid_1.txt` is equal to file `le_Cid_1.txt`
- Then file `le_Cid_1.txt` contains file `le_Cid_1.txt`

## Scenario : default behavior, non sensible to blank lines
- when I run `./bbt is.md`
- then there is no error 

- when I run `./bbt contains.md`
- then there is no error 

## scenario : with --exact_match, sensible to blank lines
- when I run `./bbt -em is.md`
- then there is an error 
- and output contains `Error : le_Cid_1.txt not equal to expected:`

- when I run `./bbt -em contains.md`
- then there is an error 
- and output contains `Error : le_Cid_1.txt does not contain expected:`
  
## Scenario : with --exact_match and --ignore_blank_lines, non sensible to blank lines
- when I run `./bbt -em -ibl is.md`
- then there is no error 

- when I run `./bbt --exact_match --ignore_blank_lines contains.md`
- then there is no error 



