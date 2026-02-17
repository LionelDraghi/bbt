# Existing comparables and inspiring references <!-- omit from toc -->

- [Introduction](#introduction)
- [Cucumber](#cucumber)
- [Aruba](#aruba)
- [Expect](#expect)
- [JetBlack](#jetblack)
- [Exactly ](#exactly-)
- [BATS](#bats)
- [Spectest](#spectest)

## Introduction

References fit into two main category, those matching the bbt objective of easily defining and run a command line test, like `bats` or `exactly`, and those using the Gherkin language to define the test.   

## [Cucumber](https://cucumber.io/)

Cucumber uses tests written in a natural language style, backed up by some glue code (Java, JavaScript, Kotlin, etc.).

To run the test, you have to :
1. first write your scenarios and steps in Gherkin (plain english);
2. write in some programming language the matching steps; 
3. And to have a nice doc, you further need to integrate the Gherkin snippet (usually .feature files) in you documentation.

The start of [Behave](https://behave.readthedocs.io/en/latest/tutorial/) (one of the Cucumber implementations) tutorial make it crystal clear :

> First, install behave.

> Now make a directory called “features”. In that directory create a file called “tutorial.feature” containing:

```
Feature: showing off behave

  Scenario: run a simple test
     Given we have behave installed
      When we implement a test
      Then behave will test it for us!
```

>Make a new directory called “features/steps”. In that directory create a file called “tutorial.py” containing:

```python
from behave import *

@given('we have behave installed')
def step_impl(context):
    pass

@when('we implement a test')
def step_impl(context):
    assert True is not False

@then('behave will test it for us!')
def step_impl(context):
    assert context.failed is False
```

> Run behave:

~~~
% behave
Feature: showing off behave # features/tutorial.feature:1

  Scenario: run a simple test        # features/tutorial.feature:3
    Given we have behave installed   # features/steps/tutorial.py:3
    When we implement a test         # features/steps/tutorial.py:7
    Then behave will test it for us! # features/steps/tutorial.py:11

1 feature passed, 0 failed, 0 skipped
1 scenario passed, 0 failed, 0 skipped
3 steps passed, 0 failed, 0 skipped, 0 undefined
~~~

## [Aruba](https://github.com/cucumber/aruba/tree/main/features/)

Aruba is a Cucumber extension for Command line applications written in any programming language. As such, it is probably the closest match to bbt.

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

## [Expect](https://en.wikipedia.org/wiki/Expect)

```
# Open a Telnet session to a remote server, and wait 
# for a username prompt.
spawn telnet $remote_server
expect "username:"

# Send the username, and then wait for a password prompt.
send "$my_user_id\r"
expect "password:"

# Send the password, and then wait for a shell prompt.
send "$my_password\r"
expect "%"
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


 
## Spectest

Generate document from end-to-end tests (opposite to bbt way), so not comparable to bbt, but interesting idea: generate sequence diagram with cli and sut exchanging signals

https://dev.to/nchika/spectest-api-thttps://dev.to/nchika/spectest-api-testing-library-for-go-that-generate-e2e-test-result-document-in-markdown-11pi


 


 