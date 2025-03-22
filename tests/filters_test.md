## Feature

## Background
- Given the directory `dir7`
- Given the directory `dir8`
- Given the file `dir7/scen1.md`
~~~
# Feature A
## Scenario A.1
## Scenario A.2
~~~
- Given the file `dir8/scen1.md`
~~~
## Feature: User Authentication @regression @smoke
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


## Feature: Search Functionality @search @regression
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

# Scenario: List all files

- When I run `./bbt -lf dir7 dir8`
- Then I get (unordered) 
~~~
dir7/scen1.md
dir8/scen1.md
~~~

# Scenario: Select 

- When I run `./bbt -lf dir7 dir8`
- Then I get (unordered) 
~~~
dir7/scen1.md
dir8/scen1.md
~~~