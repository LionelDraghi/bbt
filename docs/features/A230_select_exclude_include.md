<!-- omit from toc -->
## Feature: Filter

`bbt` allows for fine-grained selection of what is executed in the set of files passed in the command line, at the document (file), feature, scenario, and even step level.  

This can be done:

1. Either by exclusion, i.e., by removing items that will not be executed, using the `--exclude` option,
2. Or by selecting the items to be executed with the `--select` option.

`--exclude` may be followed by some `--include` and `-- select` maybe followed by some `--include`.

Filter are processed in order of appearance on the command line.

<!-- omit from toc -->
### Auto propagation
To maintain consistency, when a Step is selected, the parents (Scenario, Feature) and necessary background are selected.

<!-- omit from toc -->
### Tags
There is no special processing for tags. From bbt point of view *@regression* is a string, and you can filter on that basis.  
But if it's your convention you may as well use *#regression*.  

Important Note : there is no filtering on the lines before header, meaning that **unlike Gherkin, tags must be in the title line**.

_Table of Contents_:
- [Background: Lets cleanup the place](#background-lets-cleanup-the-place)
- [Scenario: no filtering](#scenario-no-filtering)
- [Scenario: step filtering](#scenario-step-filtering)
- [Scenario: step selection](#scenario-step-selection)
- [Scenario: Selecting a scenario](#scenario-selecting-a-scenario)
- [Scenario: selection is empty](#scenario-selection-is-empty)
- [Scenario: Selecting a Background only](#scenario-selecting-a-background-only)
- [Scenario: Select followed by an exclude](#scenario-select-followed-by-an-exclude)
- [Scenario: scenario Excluded followed by an include of a step inside](#scenario-scenario-excluded-followed-by-an-include-of-a-step-inside)
- [Scenario: Exclude by file name](#scenario-exclude-by-file-name)

### Background: Lets cleanup the place 
  - Given there is no file `output_1.txt`
  - Given there is no file `output_2.txt`
  - Given there is no file `input.txt`
  - Given the file `suite_1.md`
  ~~~
  # Scenario:
  - When I run `./sut create         output_1.txt`
  - When I run `./sut append Linux   output_1.txt`
  - When I run `./sut append Windows output_1.txt`
  ~~~
  - Given the file `suite_2.md`
  ~~~
  # Scenario: Robustness
  - When I run `./sut create         output_2.txt`
  - When I run `./sut append Linux   output_2.txt`
  - When I run `./sut append Windows output_2.txt`
  ~~~



### Scenario: no filtering

  - When I run `./bbt suite_1.md`  
  - Then file `output_1.txt` is
  ```
  Linux
  Windows
  ```

  Because both steps are run


### Scenario: step filtering

  - When I run `./bbt --exclude Windows suite_1.md`  
  - Then file `output_1.txt` is
    ```
    Linux
    ```

  Because only the create and Linux Steps are run

### Scenario: step selection

  - When I run `./bbt --select create suite_1.md`  
  - Then file `output_1.txt` contains
    ~~~
    ~~~

  Because only the create step is executed, so the output file should be empty


### Scenario: Selecting a scenario

  - When I run `./bbt --select Robustness suite_1.md suite_2.md`  
  - Then there is no file `output_1.txt`
  - And `output_2.txt` is
    ~~~
    Linux
    Windows
    ~~~

  Because only the scenario in the second file is executed


### Scenario: selection is empty

  - When I run `./bbt --select xzscskfjhs suite_1.md suite_2.md`  
  - Then there is no file `output_1.txt`
  - And  there is no file `output_2.txt`

  Because nothing should be executed


### Scenario: Selecting a Background only

  - Given the file `background.md`
    ~~~
    # Background : new_file
    - Given the new file `input.txt`
    ```
    new file
    ```
    # Scenario: Robustness
    - When I run `./sut create         output_2.txt`
    - When I run `./sut append Linux   output_2.txt`
    - When I run `./sut append Windows output_2.txt`
    ~~~

  - When I run `./bbt --select new_file background.md`  
  - Then there is no file `input.txt`
  - And  there is no file `output_2.txt`

  Because no scenario is going to invoke the Background scenario.


### Scenario: Select followed by an exclude

  - When I run `./bbt --select Robustness --exclude Linux suite_2.md`

  - Then `output_2.txt` is
    ~~~
    Windows
    ~~~

### Scenario: scenario Excluded followed by an include of a step inside

  - When I run `./bbt --exclude Robustness --include create --include Linux suite_2.md`  
  - Then `output_2.txt` is
    ~~~
    Linux
    ~~~
  Because only the creation and "append Linux" should be run


### Scenario: Exclude by file name

  - When I run `./bbt --exclude suite_2.md suite_2.md`  
  - Then there is no `output_2.txt` file

  Because nothing is run

