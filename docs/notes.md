# Why, existing tools and design decision and references

## Manipulation du code de retour avec bash :
https://debian-facile.org/doc:programmation:shells:script-bash-etat-de-sorie-et-les-tests

## What make it different :

  - Very simple : 

  - Write once :
        test cases are written in almost plain English
        within a Gherkin classical frame (given … when … then)
        using Markdown (or more precisely, [Markdown with Gherkin (MDG)]( https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md), that is a strict superset of GFM)
        the test driver directly consume this file, no intermediate step

        **“documentation is the code“**

  - Run everywhere :
        Language Independent (no fixture, stub, moq… )
        It’s written in Ada, but you don’t care, could be whatever

        Platform independent (run on Linux/Windows/MacOS) :
        No more unreadable makefile, no more CI complex script

## What I don’t want :

  - Cram uses snippets of interactive shell sessions. This is not portable, unless providing a complete “bash like” environment in Windows, for example.
    And it’s using a “shell like” syntax, with meaningful indentation (horror), a $ sign at the beginning of the command, and a > sign for continuation line.
    I want something clear, as close as English as possible, with no cryptic signs.
    Same apply to output : when comparing the actual output with the expected one, I don’t care to have a classical [unified diff format](https://en.wikipedia.org/wiki/Diff#Unified_format) that could be used by some other tools.
    What a want is directly a clear side by side diff that is understandable by humans.

    [Exactly ]( https://github.com/emilkarlen/exactly/tree/master) is doing the job, and actually much more than what I need, but with a specialized language :

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

- [BATS]( https://github.com/bats-core/bats-core) example :

          @test "addition using bc" {
            result="$(echo 2+2 | bc)"
            [ "$result" -eq 4 ]
          }



## Design

### AST

Gerkhin file data model :  https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast

### Markdown compliance and vocabulary
The BBT Markdown subset try to comply with [CommonMark Spec](https://spec.commonmark.org/)

Only [ATX Heading](https://spec.commonmark.org/0.31.2/#atx-headings) are supported, not [Setext](https://spec.commonmark.org/0.31.2/#setext-headings), meaning that you can write :
```
## Feature
```
but not
```
Feature
-------
```
in the test file.

### References
https://github.com/briot/gnatbdd/tree/master
https://github.com/dcurtis/markdown-mark


[Specification as a ubiquitous language](https://en.wikipedia.org/wiki/Behavior-driven_development#Specification_as_a_ubiquitous_language)
*A ubiquitous language is a (semi-)formal language that is shared by all members of a software development team — both software developers and non-technical personnel.*

Interesting discussion on observability and TTD [here](https://www.youtube.com/watch?v=prLRI3VEVq4&t=2190s)


## Synonymes : ???

  Stderr = error output  
  Stdout = output

# Design decision
