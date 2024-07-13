## Feature : Command line

No special marks for comments, it's just normal markdown text.

### Scenario : version message
  - When I run `uut -v` or `uut --version`
  - Then I get no error
  - And I get 'uut v0.1.0'

### Scenario : Help message
  - When I run `uut -h` or `uut --help`
  - Then I get no error
```
uut usage :
uut file_name create|read|append [-with text]
```

## Feature : File manipulation

### Scenario : append
  - Given the `config.ini` file
```
verbose=false
```
  - When I successfully run `uut append lang=uk config.ini`
  - Then I get
```
verbose=false
lang=uk
```

More extensive explanations : https://github.com/LionelDraghi/bbt/tree/main

File generated with BBT 0.0.4
