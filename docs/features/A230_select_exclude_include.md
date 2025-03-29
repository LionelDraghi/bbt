<!-- omit from toc -->
## Feature: Filter

`bbt` allows for fine-grained selection of what is executed in the set of files passed in the command line, 
at the document (file), feature, scenario, and even step level.  

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

- Given the directory `dir7`
- Given the directory `dir8`
- Given the file `dir7/scen1.md`
~~~md
# Feature Security related tests

## Background: user's credential is recorded @smoke

## Scenario A.1 User blocked after multiple failed login attempt @login @security
  Given the user is on the login page
  When the user enters invalid credentials 5 times
  Then the user account is blocked
  And an error message "Your account has been blocked due to multiple failed login attempts" is displayed

## Scenario A.2 User banned when DOS attack detected @security 
  Given the system detects multiple rapid login attempts from the same IP address
  When the number of attempts exceeds the threshold
  Then the IP address is banned
  And a warning is logged in the security system
~~~

- Given the file `dir8/scen1.md`
~~~md
##  Feature: User Authentication @regression @smoke
  As a user
  I want to log in and log out of the system
  So that I can access my account securely

  ### Scenario: Successful login @login @positive
    Given the user navigates to the login page
    When the user enters valid credentials
    Then the user is redirected to the dashboard

  ### Scenario: Failed login with invalid credentials @login @negative
    Given the user navigates to the login page
    When the user enters invalid credentials
    Then an error message "Invalid username or password" is displayed

  ### Scenario: Successful logout @logout
    Given the user is logged in
    When the user clicks the logout button
    Then the user is redirected to the login page

##  Feature: Search Functionality @search @regression
  As a user
  I want to search for items
  So that I can find what I am looking for quickly
  
  ### Scenario: Search returns results @search @positive
    Given the user is on the search page
    When the user enters a valid search term
    Then a list of matching items is displayed

  ### Scenario: Search returns no results @search @negative
    Given the user is on the search page
    When the user enters a search term that does not match any items
    Then a message "No results found" is displayed

  ### Scenario: Filter search results @search @filter
    Given the user is on the search page
    And the user has entered a valid search term
    When the user applies a filter
    Then the search results are updated to match the filter
~~~

### Scenario: List all files

- When I run `./bbt list dir7 dir8`
- Then I get
~~~
dir7/scen1.md:1: Feature "Security related tests"  
dir7/scen1.md:3: Scenario "user's credential is recorded @smoke"  
dir7/scen1.md:5: Scenario "A.1 User blocked after multiple failed login attempt @login @security"  
dir7/scen1.md:11: Scenario "A.2 User banned when DOS attack detected @security"  
dir8/scen1.md:1: Feature "User Authentication @regression @smoke"  
dir8/scen1.md:6: Scenario "Successful login @login @positive"  
dir8/scen1.md:11: Scenario "Failed login with invalid credentials @login @negative"  
dir8/scen1.md:16: Scenario "Successful logout @logout"  
dir8/scen1.md:21: Feature "Search Functionality @search @regression"  
dir8/scen1.md:26: Scenario "Search returns results @search @positive"  
dir8/scen1.md:31: Scenario "Search returns no results @search @negative"  
dir8/scen1.md:36: Scenario "Filter search results @search @filter"
~~~

### Scenario: Excluding @security tagged items

- When I run `./bbt list --exclude @security dir7 dir8`
- Then I get
~~~
dir7/scen1.md:1: Feature "Security related tests"  
dir7/scen1.md:3: Scenario "user's credential is recorded @smoke"  
dir8/scen1.md:1: Feature "User Authentication @regression @smoke"  
dir8/scen1.md:6: Scenario "Successful login @login @positive"  
dir8/scen1.md:11: Scenario "Failed login with invalid credentials @login @negative"  
dir8/scen1.md:16: Scenario "Successful logout @logout"  
dir8/scen1.md:21: Feature "Search Functionality @search @regression"  
dir8/scen1.md:26: Scenario "Search returns results @search @positive"  
dir8/scen1.md:31: Scenario "Search returns no results @search @negative"  
dir8/scen1.md:36: Scenario "Filter search results @search @filter"
~~~

### Scenario: Select tagged items

- When I run `./bbt list --select @security dir7 dir8`
- Then I get
~~~
dir7/scen1.md:1: Feature "Security related tests"  
dir7/scen1.md:3: Scenario "user's credential is recorded @smoke"  
dir7/scen1.md:5: Scenario "A.1 User blocked after multiple failed login attempt @login @security"  
dir7/scen1.md:11: Scenario "A.2 User banned when DOS attack detected @security"  
~~~
Note that the parent Feature are also selected, but Background are not.

### Scenario: Select only a Background

- When I run `./bbt list --select "credential is recorded" dir7 dir8`
- Then I get
~~~
dir7/scen1.md:1: Feature "Security related tests"  
dir7/scen1.md:3: Scenario "user's credential is recorded @smoke"  
~~~

### Scenario: Select tagged scenarios in two docs

- When I run `./bbt list --select @smoke dir7 dir8`
- Then I get
~~~
dir7/scen1.md:1: Feature "Security related tests"  
dir7/scen1.md:3: Scenario "user's credential is recorded @smoke"  
dir8/scen1.md:1: Feature "User Authentication @regression @smoke"  
dir8/scen1.md:6: Scenario "Successful login @login @positive"  
dir8/scen1.md:11: Scenario "Failed login with invalid credentials @login @negative"  
dir8/scen1.md:16: Scenario "Successful logout @logout"  
~~~

### Scenario: Select followed by an exclude

- When I run `./bbt list --select @security --exclude DOS dir7 dir8`
- Then I get 
~~~
dir7/scen1.md:1: Feature "Security related tests"  
dir7/scen1.md:3: Scenario "user's credential is recorded @smoke"  
dir7/scen1.md:5: Scenario "A.1 User blocked after multiple failed login attempt @login @security"  
~~~

### Scenario: no step filtering

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

- When I run `./bbt --exclude Windows -d filters filtered_step.md`
  Only the create and Linux Steps are run

- Then file `output.txt` is
```
Linux
```
