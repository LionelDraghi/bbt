## Features : Output verbosity is controlled through command line options

`bbt` has three verbosity levels : Quiet / Normal / Verbose 

Those levels are controlled with `--quiet` and `--verbose` options.

- Quiet mode  
  Only the global result is output.
  There is a single line summary.

- Normal mode  
  Result is output for each Feature and Scenario

- Verbose mode  
  Result is output for each step

Errors are always output in verbose mode, meaning that you have the info on the failing step even in Quiet mode. 

## Background:

- Given the file `OK_scen.md`
~~~
# Feature : Getting info
# Scenario : Getting the version
- When I run `./sut -v`
- Then I get `sut version 1.0`
# Scenario : Getting help
- When I run `./sut -h`
- Then output contains `Usage:`
~~~

- Given the file `NOK_scen.md`
~~~
# Scenario
- When I run `./sut -v`
- Then I get `v3.1`
~~~

### Scenario: Quiet mode run

- When I run `./bbt -q OK_scen.md`

- Then the output is
~~~
~~~
# Fixme: --quiet is maybe too quiet!

### Scenario: Default mode run

- When I run `./bbt OK_scen.md`

- Then the output is
~~~
## [OK_scen.md](OK_scen.md)  
  - [X] scenario [Getting the version](OK_scen.md) pass  
  - [X] scenario [Getting help](OK_scen.md) pass  

## Summary : **Success**

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 2     |
| Empty      | 0     |
~~~
Fixme: in normal mode, document and scenarios are displayed, but not the feature!

### Scenario: Verbose mode run

- When I run `./bbt -v OK_scen.md`

- Then the output is
~~~
## [OK_scen.md](OK_scen.md)  
  ### Feature: Getting info    
    OK  : When I run `./sut -v`  
    OK  : Then I get `sut version 1.0`  
  - [X] scenario [Getting the version](OK_scen.md) pass    

    OK  : When I run `./sut -h`    
    OK  : Then output contains `Usage:`    
  - [X] scenario [Getting help](OK_scen.md) pass    

## Summary : **Success**

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 2     |
| Empty      | 0     |
~~~

### Scenario: Normal mode with an error

- When I run `./bbt NOK_scen.md`

- Then the output contains
```
Output:    
~~~  
sut version 1.0  
~~~  
not equal to expected:    
~~~  
v3.1  
~~~   
```

- And the output contains
```
  - [ ] scenario [](NOK_scen.md) fails
```

Fixme: missing error output for Quiet and Verbose mode
