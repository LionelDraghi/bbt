<!-- omit from toc -->
## Feature: testing the "file is" feature

Basic bbt feature : checking a file content.  

_Table of Contents:_
- [Scenario: file is as expected](#scenario-file-is-as-expected)
- [Scenario: file is not as expected](#scenario-file-is-not-as-expected)

### Scenario: file is as expected

- Given the `blaise_cendrars.txt` file
```
Seigneur rien n'a changé depuis que vous n'êtes plus Roi
Le mal s'est fait une béquille de votre croix
```

- Then `blaise_cendrars.txt` is
```
Seigneur rien n'a changé depuis que vous n'êtes plus Roi
Le mal s'est fait une béquille de votre croix
```

### Scenario: file is not as expected

- Given the `file_is_code_fence.input` file
```md
# Scenario: 
- Then `blaise_cendrars.txt` is
~~~
Un effroyable drôle m'a jeté un regard Aigu, puis a passé, mauvais, comme un poignard.
~~~
```

- When running `./bbt file_is_code_fence.input`

- Then I get an error
