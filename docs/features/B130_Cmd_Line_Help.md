<!-- omit from toc -->
# Feature: Clear command line help 

bbt goal to have an almost zero effort learning curve rely also on a clear command line help.

_Table of Contents:_
- [Scenario: calling bbt without parameter or with -h put the normal help](#scenario-calling-bbt-without-parameter-or-with--h-put-the-normal-help)
- [Scenario: filtering help](#scenario-filtering-help)
- [Scenario: matching help](#scenario-matching-help)
- [Scenario: others help](#scenario-others-help)
- [Scenario: On\_All help](#scenario-on_all-help)
- [Scenario: tutorial generation](#scenario-tutorial-generation)
- [Scenario: generated example is OK](#scenario-generated-example-is-ok)

## Scenario: calling bbt without parameter or with -h put the normal help
- When I run `./bbt` 
  or `./bbt help` or `./bbt he` Fixme: 
- then the output is equal to file `../docs/help/base.txt`

## Scenario: filtering help
- When I run `./bbt he filtering` 
- then the output is equal to file `../docs/help/filtering.txt`

## Scenario: matching help
- When I run `./bbt help matching` 
- then the output is equal to file `../docs/help/matching.txt`

## Scenario: others help
- When I run `./bbt help other` 
- then the output is equal to file `../docs/help/other.txt`

## Scenario: On_All help
- When I run `./bbt help on_all` 
- then the output contains file `../docs/help/base.txt`
- and  the output contains file `../docs/help/filtering.txt`
- and  the output contains file `../docs/help/matching.txt`
- and  the output contains file `../docs/help/other.txt`

## Scenario: tutorial generation

- When I run `./bbt help tutorial` 
- then the output is equal to file `../docs/help/tutorial.md`

## Scenario: generated example is OK

Testing that the generated example works is the bare minimum
Fixme: but I don't know how to test it!

- When I run `./bbt help example` 
- then the output is equal to file `../docs/examples/gcc_hello_world.md`
