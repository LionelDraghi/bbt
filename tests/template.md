## Feature : Command line

No special marks for comments, it's just normal markdown text.

### Scenario : version message
  - When I run `uut -v` or `uut --version`
  - Then I get no error
  - And I get `uut version 1.0`

### Scenario : Help message
  - When I run `uut -h` or `uut --help`
  - Then I get no error
  - And output contains
```
Usage:  
```




### Scenario : Unknow option
  - When I run `uut -xyz`
  - Then I get an error
  - And output contains `unknown option -xyz`

## Feature : File manipulation

### Scenario : append
  - Given the `config.ini` file
```
verbose=false
```
  - When I successfully run `uut append lang=uk config.ini`
  - When I successfully run `uut read config.ini`
  - Then I get
```
verbose=false
lang=uk
```

More extensive explanations : https://github.com/LionelDraghi/bbt/tree/main

File generated with BBT 0.0.4
