Notes <!-- omit from toc -->
=====

- [Manipulation du code de retour avec bash](#manipulation-du-code-de-retour-avec-bash)
- [What I don’t want](#what-i-dont-want)
- [AST](#ast)
- [Markdown compliance and vocabulary](#markdown-compliance-and-vocabulary)
- [References](#references)
- [GNAT.Expect](#gnatexpect)


## Manipulation du code de retour avec bash

https://debian-facile.org/doc:programmation:shells:script-bash-etat-de-sorie-et-les-tests


## What I don’t want

  - Cram uses snippets of interactive shell sessions.
    This is not portable, unless providing a complete “bash like” environment in Windows, for example.
    And it’s using a “shell like” syntax, with meaningful indentation (horror), a $ sign at the beginning of the command, and a > sign for continuation line.  
    I want something clear, as close as English as possible, with no cryptic signs.  
    Same apply to output : when comparing the actual output with the expected one, I don’t care to have a classical [unified diff format](https://en.wikipedia.org/wiki/Diff#Unified_format) that could be used by some other tools.  
    What a want is directly a clear side by side diff that is understandable by humans.

## AST

Gerkhin file data model :  https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast

## Markdown compliance and vocabulary
The BBT Markdown subset try to comply with [CommonMark Spec](https://spec.commonmark.org/)


## References
https://github.com/briot/gnatbdd/tree/master
https://github.com/dcurtis/markdown-mark

[Specification as a ubiquitous language](https://en.wikipedia.org/wiki/Behavior-driven_development#Specification_as_a_ubiquitous_language)
*A ubiquitous language is a (semi-)formal language that is shared by all members of a software development team — both software developers and non-technical personnel.*

Interesting discussion on observability and TTD [here](https://www.youtube.com/watch?v=prLRI3VEVq4&t=2190s)

## GNAT.Expect

https://www.adacore.com/gems/gem-54
 
12.69. GNAT.Expect (g-expect.ads)

Provides a set of subprograms similar to what is available with the standard Tcl Expect tool. It allows you to easily spawn and communicate with an external process. You can send commands or inputs to the process, and compare the output with some expected regular expression. Currently GNAT.Expect is implemented on all native GNAT ports. It is not implemented for cross ports, and in particular is not implemented for VxWorks or LynxOS.

12.70. GNAT.Expect.TTY (g-exptty.ads)

As GNAT.Expect but using pseudo-terminal. Currently GNAT.Expect.TTY is implemented on all native GNAT ports. It is not implemented for cross ports, and in particular is not implemented for VxWorks or LynxOS.
