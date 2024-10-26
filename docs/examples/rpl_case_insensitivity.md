# Feature : Case insensitivity

`rpl` is able to replace different occurrences of the same string with different casing thanks to the `--ignore-case` option.

## Scenario: 

- Given the new file `config.ini` :
  ```
  lang=fr
  keyboard=FR
  ```

- When I run `/usr/bin/rpl --ignore-case FR UK config.ini`
  
- Then the `config.ini` file contains 
  ```
  lang=UK
  keyboard=UK
  ```
  