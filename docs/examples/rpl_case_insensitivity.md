# Overview 
[`rpl`](https://manpages.debian.org/bookworm/rpl/rpl.1.en.html) is a utility to replace multiple strings in multiple files. 


## Feature 1 : Case insensitivity

`rpl` is able to replace different occurrences of the same string with different casing thanks to the `--ignore-case` option.

### Scenario 1.1 : simple use (single file, no globbing)

- Given the new file `config.ini` :
  ```
  lang=fr
  keyboard=FR
  ```

- When I run `rpl -i FR UK config.ini`  
  -i = --ignore-case  
  Note that on Windows, we are using rpl 1.4.1 by Joe Laffey, that do not accept long options unlike the rpl packaged on Debian 

- Then the `config.ini` file contains 
  ```
  lang=UK
  keyboard=UK
  ```
  
