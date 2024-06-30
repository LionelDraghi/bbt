# Existing comparables

## [Aruba](https://github.com/cucumber/aruba/tree/main/features/)

Sample (from https://github.com/cucumber/aruba/blob/main/features/06_use_aruba_cli/initialize_project_with_aruba.feature) :

```
Feature: Initialize project with aruba

  To add `aruba` to your project you can use the `aruba init`-command.

  Background:
    Given I use the fixture "empty-app"

  Scenario: Create files for RSpec
    When I successfully run `aruba init --test-framework rspec`
    Then the following files should exist:
      | spec/spec_helper.rb |
    And the file "spec/support/aruba.rb" should contain:
    """
    require 'aruba/rspec'
    """
    And the file "Gemfile" should contain:
    """
    gem 'aruba'
    """
    When I successfully run `rspec`
    Then the output should contain:
    """
    0 examples, 0 failures
    """
 

  Scenario: Create files for Cucumber
    When I successfully run `aruba init --test-framework cucumber`
    Then the file "features/support/aruba.rb" should contain:
    """
    require 'aruba/cucumber'
    """
    And the file "Gemfile" should contain:
    """
    gem 'aruba'
    """
    When I successfully run `cucumber`
    Then the output should contain:
    """
    0 scenarios
    0 steps
    """
```

## [JetBlack](https://github.com/odlp/jet_black)

Same spirit, but Ruby specific.


Main features :

    Each session takes place within a unique temporary directory, outside the project
    Synchronously run commands then write assertions on:
        The stdout / stderr content
        The exit status of the process
    Exercise interactive command line interfaces
    Manipulate files in the temporary directory:
        Create files
        Create executable files
        Append content to files
        Copy fixture files from your project
    Modify the environment without changing the parent test process:
        Override environment variables
        Escape the current Bundler context
        Adjust $PATH to include your executable / Subject Under Test


## [Exactly ](https://github.com/emilkarlen/exactly/tree/master) 

Exactly is doing the job, and actually much more than what I need, but with a specialized language :

```
[setup]
stdin = -contents-of some-test-contacts.txt

[act]
my-contacts-program get-email --name 'Pablo Gauss'

[assert]
exit-code == 0

stdout equals <<EOF
pablo@gauss.org
EOF

stderr is-empty

```
Not as easy to read as English, and not suitable for immediate insertion in documentation.

## [BATS](https://github.com/bats-core/bats-core) 

example :
          @test "addition using bc" {
            result="$(echo 2+2 | bc)"
            [ "$result" -eq 4 ]
          }



