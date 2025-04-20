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
The is no special processing for tags. From bbt point of view *@regression* is a string, and you can filter on that basis.  
But if it's your convention you may as well use *#regression*.  

Important Note : there is no filtering on the lines before header, meaning that unlike Gherkin, tags must be in the title line.

The following Scenarios are based on two files containing several scenarios and tags.  
To facilitate understanding, tags are prefixed here in the usual way, with '@'.

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
- Given there is no file `output.txt`
- Given there is no file `output2.txt`
- Given there is no file `input.txt`

### Scenario: no filtering

- Given the file `filtered_step.md`
~~~
# Scenario:
- When I run `./sut create         output.txt`
- When I run `./sut append Linux   output.txt`
- When I run `./sut append Windows output.txt`
~~~

- When I run `./bbt filtered_step.md`
  Both steps are run

- Then file `output.txt` is
```
Linux
Windows
```

### Scenario: step filtering

- When I run `./bbt --exclude Windows filtered_step.md`
  Only the create and Linux Steps are run

- Then file `output.txt` is
```
Linux
```

### Scenario: step selection

- When I run `./bbt --select create filtered_step.md`
  Only the create step is executed, so the output file should be empty

- Then file `output.txt` contains
~~~
~~~

### Scenario: Selecting a scenario

- Given the file `robustness.md`
~~~
# Scenario: Robustness
- When I run `./sut create         output2.txt`
- When I run `./sut append Linux   output2.txt`
- When I run `./sut append Windows output2.txt`
~~~

- When I run `./bbt --select Robustness filtered_step.md robustness.md`
  Only the scenario in the second file is executed

- Then there is no file `output.txt`
- And `output2.txt` is
~~~
Linux
Windows
~~~

### Scenario: selection is empty

- Given the file `robustness.md`
~~~
# Scenario: Robustness
- When I run `./sut create         output2.txt`
- When I run `./sut append Linux   output2.txt`
- When I run `./sut append Windows output2.txt`
~~~

- When I run `./bbt --select xzscskfjhs filtered_step.md robustness.md`
  Nothing should be executed

- Then there is no file `output.txt`
- And  there is no file `output2.txt`

### Scenario: Selecting a Background only

- Given the file `background.md`
~~~
# Background : new_file
- Given the new file `input.txt`
```
new file
```
# Scenario: Robustness
- When I run `./sut create         output2.txt`
- When I run `./sut append Linux   output2.txt`
- When I run `./sut append Windows output2.txt`
~~~

- When I run `./bbt --select new_file background.md`
  As no scenario is going to invoke the Background scenario, it is not run

- Then there is no file `input.txt`
- And  there is no file `output2.txt`

### Scenario: Select followed by an exclude

- Given the file `robustness.md`
~~~
# Scenario: Robustness
- When I run `./sut create         output2.txt`
- When I run `./sut append Linux   output2.txt`
- When I run `./sut append Windows output2.txt`
~~~

- When I run `./bbt --select Robustness --exclude Linux robustness.md`

- Then `output2.txt` is
~~~
Windows
~~~

### Scenario: scenario Excluded followed by an include of a step inside

- Given the file `robustness.md`
~~~
# Scenario: Sanity
- When I run `./sut create         output2.txt`
- When I run `./sut append Linux   output2.txt`
- When I run `./sut append Windows output2.txt`
~~~

- When I run `./bbt --exclude Sanity --include create --include Linux robustness.md`
Only the creation should be run

- Then `output2.txt` is
~~~
Linux
~~~

### Scenario: Exclude by file name

- Given the file `robustness.md`
~~~
# Scenario: 
- When I run `./sut create         output2.txt`
- When I run `./sut append Linux   output2.txt`
- When I run `./sut append Windows output2.txt`
~~~

- When I run `./bbt --exclude robustness.md robustness.md`
Nothing is run

- Then there is no `output2.txt` file
