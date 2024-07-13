## Background feature

Background are special scenario, that may be used :
1. at the beginning of the file, that is before any feature or scenario.  
In that case, the background content will be executed before  
all scenarios in the file;
2. or at the beginning of a feature, and will then apply to all scenarios in the feature.

### Background: Background1 
- Given the new dir `dir1`
- Given the file `dir1/file1`
```
file 1 content
```

### Feature: Feature 1

### Background: Background2 in feature 1
- Given the file `dir1/file2`
```
file 2 content
```

### Scenario: lets erase what was created by previous background runs
- Given there is no `dir1` dir

### Scenario: Two Background executed
Here both Backgrounds apply.  
Note that execution order is not tested, not sure this is important anyway.

-  then there is a `dir1/file1` file
-  then there is a `dir1/file2` file

### Feature: Feature 2

### Scenario: lets erase once more what was created by previous background runs
- Given there is no `dir1` dir
  
### Scenario: only first background should apply
Here, we are outside Feature 1, so only the global Background should be executed
-  then there is a `dir1/file1` file
-  then there is no `dir1/file2` file
