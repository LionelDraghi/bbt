## Feature : Command line

Your comments here : it's just normal markdown text.

### Scenario : version message
  - Given the `config.ini` file
  ```
  verbose=false
  ```
  - When I run `sut --version`
  - Then I get no error
  - And I get `sut v0.1.0`
  (or "- And Output is `sut v0.1.0`")

  Both above form test that the output is exactly `sut v0.1.0`
  If what you want is just test that the output contains that string, then use:
  - Then output contains `sut v0.1.0`
  If what you want is just test that the output contains more lines, then use:
  - Then output contains `expected.txt`

Preconditions common to several scenarios may be put in a Background section, before scenarios :
### Background:
  - Given there is no `input.txt` file
  - Given there is a `tmp` dir

More extensive explanations : https://github.com/LionelDraghi/bbt/tree/main

File generated with BBT 0.0.6
