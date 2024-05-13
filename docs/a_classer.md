### Existing comparables

https://github.com/cucumber/aruba/blob/main/features/06_use_aruba_cli/initialize_project_with_aruba.feature

pour inspirer la syntaxe :


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

###


https://github.com/odlp/jet_black
 

meme esprit, mais Ruby specific.


Fonctionalité :

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

Pour inspirer les keywords :
·       Stdout have_stdout which accepts a string or regular expression
·       have_stderr which accepts a string or regular expression
·       have_no_stdout which asserts the stdout is empty
·       have_no_stderr which asserts the stderr is empty

And the following predicate matchers:
·       be_a_success / be_success asserts the exit status was zero
·       be_a_failure / be_failure asserts the exit status was not zero