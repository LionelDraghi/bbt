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

- [Background: Lets create the two files scen1.md and scen2.md](#background-lets-create-the-two-files-scen1md-and-scen2md)
- [Scenario: List all files](#scenario-list-all-files)
- [Scenario: Excluding @security tagged items](#scenario-excluding-security-tagged-items)
- [Scenario: Select tagged items](#scenario-select-tagged-items)
- [Scenario: Select only a Background](#scenario-select-only-a-background)
- [Scenario: Select tagged scenarios in two docs](#scenario-select-tagged-scenarios-in-two-docs)
- [Scenario: Select followed by an exclude](#scenario-select-followed-by-an-exclude)
- [Scenario: no step filtering](#scenario-no-step-filtering)
- [Scenario: step filtering](#scenario-step-filtering)

### Background: Lets create the two files scen1.md and scen2.md 
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

### Scenario: Exclude followed by an Include

- Given the file `robustness.md`
~~~
# Scenario: Robustness
- When I run `./sut create         output2.txt`
- When I run `./sut append Linux   output2.txt`
- When I run `./sut append Windows output2.txt`
~~~

- When I run `./bbt --exclude Robustness --include create robustness.md`
Only the creation should be run

- Then `output2.txt` is
~~~
~~~
