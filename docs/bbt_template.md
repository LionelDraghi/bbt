## Feature : Command line

No special marks for comments, it's just normal markdown text.

### Scenario : version message
  - When I run `sut --version`
  - Then I get no error
  - And I get 'sut v0.1.0'

### Scenario : Help message
  - When I run `sut --help`
  - Then output contains
```
Return code:
Return code is set to 1 when :
- there is a command line error (unknown option for example)
- there is a file error (unable to open the given file, for example)
Return code is set to 0 otherwise.
```
  - And output contains `Usage:`
  - And output contains `Errors:`

## Feature : File manipulation

### Scenario : append
  - Given the `config.ini` file
```
verbose=false
```
  - When I successfully run `sut append lang=uk config.ini`
  - Then I get
```
verbose=false
lang=uk
```

More extensive explanations : https://github.com/LionelDraghi/bbt/tree/main

File generated with BBT 0.0.6
