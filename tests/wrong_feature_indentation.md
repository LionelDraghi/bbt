# Demo
We will test a new app called git

## Scenario: which version am I running?
- When I run `git --version`
- Then I get `git version 2.47.2` 

## Scenario: help is available
- When I run `git --help`
- Then the output contains `usage : git` 

## Scenario: help is available
- When I run `git --xzshelp`
- Then I get an error
- And the output contains `option inconnue` 

# Feature: file command

## Scenario:
- Given the new file `tmp.txt` containing `hello` 
- When I successfully run `file tmp.txt` 
- Then the output contains `asCII text`
