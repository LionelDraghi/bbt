[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg?style=flat-square)](https://opensource.org/licenses/Apache-2.0)

[![Awarded](https://img.shields.io/badge/Ada_Crate_of_the_Year-2024-black?style=flat-square)](https://blog.adacore.com/ada-spark-crate-of-the-year-2024-winners-announced)

[![image|61x20](https://img.shields.io/badge/inside-darkslateblue?style=flat-square&logoColor=white&logo=data:image/svg%2bxml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyBpZD0ic3ZnX3Jvb3QiIHdpZHRoPSIzMDAiIGhlaWdodD0iMTUwIiB2ZXJzaW9uPSIxLjEiIHZpZXdCb3g9IjAgMCAzMDAgMTUwIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciPgogPGcgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoOCAtMzEuMTk3KSI+CiAgPGcgdHJhbnNmb3JtPSJtYXRyaXgoMS4zMzMzIDAgMCAtMS4zMzMzIDAgMjAwKSI+CiAgIDxnIHRyYW5zZm9ybT0ic2NhbGUoLjA5NSkiPgogICAgPHBhdGggaWQ9InN2Z19sb2dvIiBkPSJtMTczNS45IDYwOC41YzAgMzguODk2IDQwLjk0MSA1My4yMTcgMTA3LjQ5IDYwLjM4NyA1NS4yNjUgNi4xMzkxIDk5LjI4NSAxNS4zNTUgMTI3Ljk1IDMzLjc3OXYtNDkuMTMxYzAtNTguNjQtNDkuMzcxLTg3LjE2NS0xMTUuMjgtOTMuNjc4LTI2Ljg4OSAyLjQ5OTMtNTUuMDI1IDQuODQyNy04NC4yNDEgNi45ODIxLTIyLjczIDguNjY5Ny0zNS45MTggMjMuNC0zNS45MTggNDEuNjYxem0tMTI3NC41LTE3LjQwMmgyODMuNTZsMjMuMjY2LTY4LjgwOWMtMTIyLjA1LTIyLjIxMi0yNDMuNTQtNTEuNDctMzYwLjc2LTg4LjM1MXptMTQxLjI3IDQxNy42NCA5OS4yODYtMjkxLjc0aC0xOTguNTl6bTYzMC41Ni0xNDYuMzhjNzQuNzMyIDAgMTM1LjEyLTY3LjU2NyAxMzUuMTItMTUxLjUgMC01OS4zNTYtMzEuNzkxLTExMC40OC03Ny42OTktMTM0LjIzLTM0LjU3NS0xLjE1NjUtNjkuNTk3LTIuNzQ5Ny0xMDQuOTYtNC44MTE1LTUxLjE3OCAyMC42ODItODcuNTc2IDczLjkzMy04Ny41NzYgMTM5LjA0IDAgODUuOTgyIDYwLjM5MiAxNTEuNSAxMzUuMTIgMTUxLjV6bTg2NC4xLTMzMy4yNGMtMC4wNzIgMS45MzcxLTAuMTQzOSAzLjg1OS0wLjE0MzkgNS42NzA1djI3NC4zNWMwIDQxLjk2My0xNi4zNzIgMTgwLjE2LTIzMC4zMSAxODAuMTYtMTM4LjE5IDAtMjM0LjQyLTc0LjcyNy0yNDcuNzMtMTc1LjA0aDEzNS4xM2MxMi4yODUgNDUuMDQ1IDY3LjU2NiA2Ni41MzkgMTE1LjY4IDY2LjUzOSA2MC4zODQgMCA5Ni4yMDYtMjIuNTI1IDk2LjIwNi01MC4xNTkgMC00Mi45ODgtNjIuNDM5LTUzLjIzNi0xNTAuNDctNjEuNDIxLTExNi42OS0xMC4yMzItMjE1Ljk5LTQwLjk0My0yMTUuOTktMTU5LjY5IDAtMTEuNzk0IDEuMTgzNy0yMi45IDMuMjU1Mi0zMy40Ni0zMi4zNTEgMS4xNzE3LTY1LjU1IDIuMDMwNy05OS40ODUgMi41MzA1djYzMS44MWgtMTM1LjEydi0yODQuNThjLTQwLjk0MSAzOC45MDItOTQuMTgzIDYxLjQyMy0xNTUuNiA2MS40MjMtMTM3LjE3IDAtMjUwLjgtMTE4Ljc0LTI1MC44LTI3NS4zNyAwLTU4LjM0IDE2LjAyOC0xMTEuNDggNDMuMjQ0LTE1NS4xNC0zMy44ODctMy43MTgyLTY3LjkxOC03Ljk2NjctMTAyLjAyLTEyLjc0N2wtMjE0LjU0IDYxNS4yMmgtMTcwLjk1bC0yNzIuNjMtNzgyLjQ5Yy03OS42MjUtMzEuMzA0LTE1Ni4yMS02Ni41NzYtMjI4LjQtMTA2LjAxIDM0OS40OCAxNTIuNDggMTIxMS4zIDM1OC44OSAyMTg2LjcgMjMyLjQ0LTQuNjU0NiA1LjE2OTgtNDIuNDc2IDE1LjAyNy0xMDYuMDMgMjUuOTYxIi8+CiAgIDwvZz4KICA8L2c+CiA8L2c+Cjwvc3ZnPgo=)](https://ada-lang.io/) ![](https://img.shields.io/badge/play-station-blue.svg?style=flat-square&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzMDAiIGhlaWdodD0iMjAwIj48cmVjdCBpZD0iYmFja2dyb3VuZHJlY3QiIHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIHg9IjAiIHk9IjAiIGZpbGw9Im5vbmUiIHN0cm9rZT0ibm9uZSIgc3R5bGU9IiIgY2xhc3M9IiIvPgogICAgICAKICAgICAgPCEtLSA8Y2lyY2xlIGlkPSJzdmdfY2lyY2xlX2ZyYW1lIiBjeD0iMTUwIiBjeT0iMTUwIiByPSIxNDgiIHN0eWxlPSJmaWxsOm5vbmU7c3Ryb2tlLXdpZHRoOjUiIHN0cm9rZT0iYmxhY2siLz4gLS0+CiAgICAgIAogICAgICAKICAgIDxnIGNsYXNzPSJjdXJyZW50TGF5ZXIiIHN0eWxlPSIiPjx0aXRsZT5MYXllciAxPC90aXRsZT48cmVjdCBpZD0ic3ZnX2ZyYW1lIiB3aWR0aD0iMzAwIiBoZWlnaHQ9IjE5My43ODUzMTU0NzA2MzYwMiIgc3R5bGU9InN0cm9rZS13aWR0aDogMTA7IiBzdHJva2U9Im5vbmUiIGNsYXNzPSJzZWxlY3RlZCIgeT0iLTQuMDQ5ODQ5NzM1NTY1NjU4ZS03IiB4PSIwIiBmaWxsPSIjNjdlN2U3IiBmaWxsLW9wYWNpdHk9IjEiLz48ZyBpZD0ic3ZnXzEiIGNsYXNzPSIiPgogICAgICAgIDxnIGlkPSJnMTAiPgogICAgICAgICAgPGcgc3Ryb2tlLXdpZHRoPSIwIiBpZD0ic3ZnXzIiPgogICAgICAgICAgICA8cGF0aCBpZD0ic3ZnX2xvZ28iIGZpbGw9IiNmZmZmZmYiIHN0eWxlPSJmaWxsLW9wYWNpdHk6MTtmaWxsLXJ1bGU6bm9uemVybztzdHJva2U6bm9uZTtzdHJva2Utd2lkdGg6MC43OTk3ODQ3MiIgZD0ibTIyNS42MjU4Mzc4OTY5MzEwMiwxMTkuOTg1ODc1OTg3MTQ2NzYgYzAsLTQuOTI2ODE3NjE1MDA2MTQxIDUuMTg1ODU5ODA1Mjc5NTM1LC02Ljc0MDgwMzI4MDIyNzMxIDEzLjYxNTUyNjE1NTQyNjE2MiwtNy42NDkwMTMzNzk0NTg4NTU1IGM3LjAwMDI1ODQwMzgxODUzMywtMC43Nzc2MjQzNzA4MDE0OSAxMi41NzYxMzc1Mjc3ODY4MjEsLTEuOTQ0OTc1NDYwMzAyNzE5NCAxNi4yMDY5NDg3MjQ3ODkxOTMsLTQuMjc4NjYwNTA2MDEwMDM2IHY2LjIyMzIzMDYzMjk5NDY0MSBjMCw3LjQyNzc2MDkyMTA5OTg1MjUgLTYuMjUzNjA5MDk4NTIwNjQ4LDExLjA0MDg0MjU4NTQzNDYwOSAtMTQuNjAyMjQ2Nzg1MDQzMSwxMS44NjU4NzgyODc3ODkyMTMgYy0zLjQwNTkxNDUzODc4MDIwNDYsLTAuMzE2NTgxNzg4MTEyODc3MzUgLTYuOTY5ODcxMDcxNjI2MTkzLC0wLjYxMzQwNzM3Njk2NzU2NjggLTEwLjY3MDU3NjkzMjY3MDgwNSwtMC44ODQ0MDMxMDAxMjU0NTg1IGMtMi44NzkxMjA1NTg1NjA0NDgsLTEuMDk4MTU2ODkyMDk5MzU3IC00LjU0OTY1MTE2MjUwMTQ1MSwtMi45NjQwMTEyODg3MDYyNzQ2IC00LjU0OTY1MTE2MjUwMTQ1MSwtNS4yNzcwMzE5MzUxODk1MDUgek02NC4xOTUxMzU2OTE3MjA0LDEyMi4xOTAwOTM3NzEwNDg2OCBoMzUuOTE3MzU1NDUxMzYyNjcgbDIuOTQ2OTkzNjIyNjc4NTkzLDguNzE1ODY3MTM5NDAwMjYgQzg3LjYwMDM4MjM0NjIyNTQsMTMzLjcxOTUzNDAwNDgwMzkzIDcyLjIxMDU3MjkyNDA4NzMsMTM3LjQyNTUxMjk5ODk4MzkgNTcuMzYzNjYzMTQ4MjMwOSwxNDIuMDk3MDM5MDIzNTc1OCB6TTgyLjA4OTgzNzg4NjQ3MDU4LDY5LjI4ODg3MDc1NzQwMjE4IEw5NC42NjYwNzQyMTQyNTM2OCwxMDYuMjQyNjk1MTY5ODQ3MTQgSDY5LjUxMTY3NzQ5MjA5MzA0IHpNMTYxLjk2MTA0NTYyMDc3MjA3LDg3LjgzMDYyMjk5NDUyMzM3IGM5LjQ2NjA1Mjk3Nzg5ODY4OCwwIDE3LjExNTY2ODAyNDAwMTYyLDguNTU4NDM2OTQ1MzExNTAyIDE3LjExNTY2ODAyNDAwMTYyLDE5LjE4OTU5NjQ3OTQ2MjM2NyBjMCw3LjUxODQyODkxNzY5NTQxOSAtNC4wMjY5MTA1MTU0NjI4MzE1LDEzLjk5NDUyMTY3NDUyODgzOSAtOS44NDE4ODU2MzA0NTM0NiwxNy4wMDE5OTM1NjE2MDMyNCBjLTQuMzc5NDQ5MTY4ODkyMjU0LDAuMTQ2NDg4NzI3ODMyOTIzODMgLTguODE1NjQ1MDAyMzIwNDM0LDAuMzQ4MjkxNTIwMjU1NTYyMzcgLTEzLjI5NTQwMTUwMDc3OTY0NywwLjYwOTQ1NzkxMDQ0OTE5NjUgYy02LjQ4MjU4NDQyMzI1NjMzNSwtMi42MTk2NzQzMDE2MzU1NiAtMTEuMDkzMDEwMjUwMTQyNDY2LC05LjM2NDgzNDkxNTAzMjU5NCAtMTEuMDkzMDEwMjUwMTQyNDY2LC0xNy42MTE0NTE0NzIwNTI0MzcgYzAsLTEwLjg5MTExMTE5MTA1Njc3NCA3LjY0OTYxNTA0NjEwMjkzLC0xOS4xODk1OTY0Nzk0NjIzNjcgMTcuMTE0NjI5MzU3MzczOTUzLC0xOS4xODk1OTY0Nzk0NjIzNjcgem0xMDkuNDUzMTA1ODkwMjE2NzgsNDIuMjEwNjg5NzQ4MzkxNjk0IGMtMC4wMDkxMTk5OTk2NTc1NTkwODUsLTAuMjQ1MzYzNDU3NDUzNjc0MzggLTAuMDE4MjI3MzMyNjQ4OTI3MTMzLC0wLjQ4ODgwMTU4MTY0NjMwODU1IC0wLjAxODIyNzMzMjY0ODkyNzEzMywtMC43MTgyNTk1MDYzNjM4NzY0IHYtMzQuNzUwNDEwOTYxODQ2MTcgYzAsLTUuMzE1MzI3MDY3MDg0OTIgLTIuMDczNzQ4NTg4ODAwODM0NSwtMjIuODIwMDAyMzQzMTQ2NjIzIC0yOS4xNzMxMTgyMzc5MzA5NTcsLTIyLjgyMDAwMjM0MzE0NjYyMyBjLTE3LjUwNDY3NDAwOTM5NTA4NSwwIC0yOS42OTM4MDY4ODUwNDY2MjcsOS40NjU0MzIzMTEyNTUzMjcgLTMxLjM3OTU1MDE1NTA4MzA4OCwyMi4xNzIyNTE4MzQxMzUyNTMgaDE3LjExNjY2ODY5MDYzMDcxIGMxLjU1NjA3NDYwODIzODYzNywtNS43MDU2NTkyNTI0Mjg1OTI1IDguNTU4MzQ3MDExOTgxNTQ4LC04LjQyODI2MDM1MDE5OTQyIDE0LjY1MjkxMzQ0OTgwNzMxNSwtOC40MjgyNjAzNTAxOTk0MiBjNy42NDg2MTQzNzk0NzM4Mzc1LDAgMTIuMTg2MTA1NTQyNDMxODc3LDIuODUzMTgzMDI2MjAxMDI2MyAxMi4xODYxMDU1NDI0MzE4NzcsNi4zNTM0MTEwMjgxMDY1ODIgYzAsNS40NDUyMDA5Mjg4NzUwMzggLTcuOTA4OTY1MDM2MzY0NzY3LDYuNzQzMjM1MjgwMTM1OTkzIC0xOS4wNTk3MjI2MTc2NzIyNDcsNy43ODAwMDE5MDc4NzM3ODYgYy0xNC43ODA1NTU0NDUwMTQ1NjksMS4yOTYwMDY0MTgwMDM3Njc0IC0yNy4zNTg3MTk2MzkzOTE5Niw1LjE4NjA1ODY3MTkzODczNSAtMjcuMzU4NzE5NjM5MzkxOTYsMjAuMjI3MTcyNTA3MTY5NzcgYzAsMS40OTM4NTk3NDM5MDgwMzU0IDAuMTQ5OTM1MzI3NzAzNTA5NTYsMi45MDA2OTY5NTc3NTAyODk3IDAuNDEyMzI1MzE3ODUxMTk5Miw0LjIzODI0MTE3NDE5NDM4MiBjLTQuMDk3ODMxMTc5NDY2NTQ1LC0wLjE0ODQxNDA2MTA5Mzk2NCAtOC4zMDMwNTAzNTQ5MDA4NDcsLTAuMjU3MjE2OTIzNjc1MjYzMDUgLTEyLjYwMTQ1ODE5MzUwMjczOCwtMC4zMjA1MzI1MjEyOTc4NjcxMyBWNDMuNzQ1NTYzMDQ5ODQ0NTIgSDE3OS4wNzY3MTM2NDQ3NzM3IFY3OS43OTIyOTA3NjMwMTYxMyBjLTUuMTg1ODU5ODA1Mjc5NTM1LC00LjkyNzUyODIxNDk3OTQ1OSAtMTEuOTI5Nzk1NTUyMDU1ODkzLC03Ljc4MDMwNTkwNzg2MjM3MSAtMTkuNzA5MDkxOTI2NjIyODM2LC03Ljc4MDMwNTkwNzg2MjM3MSBjLTE3LjM3NTAwNTM0NzU5NzI2MywwIC0zMS43Njc1NTA0MDcxODA5ODQsMTUuMDM5ODk3ODM1Mjc2NjkyIC0zMS43Njc1NTA0MDcxODA5ODQsMzQuODc5Njc2ODIzNjU5MTEgYzAsNy4zODk3NzEwNTU4NTk2NDMgMi4wMzAxNzUyNTcxMDM2MDc2LDE0LjEyMTA1MjgwMzExMTEzNyA1LjQ3NzYwNjA2MDk5MTYxMTUsMTkuNjUxMzUwOTI4NzkwOTE1IGMtNC4yOTIzMjI3NzIxNjM3MDg1LDAuNDcwOTcxOTgyMzE1NzgwNSAtOC42MDI5MDMyNzY5NzUyLDEuMDA5MTEwMjI4Nzc2MjQ1MiAtMTIuOTIyNTkxMTE0Nzc4MDYsMS42MTQ2MTYxMzkzNzM4MzI3IEw5Mi45ODAyNDEwMTA4ODcyNyw1MC4yMjkxNjA4MDYzOTYxNSBINzEuMzI2ODgwNDIzOTM1MTggTDM2Ljc5Mzc2OTkyMDU5NjQwNCwxNDkuMzQ0Nzc4NzUxNDM1MTQgQzI2LjcwNzk0MDc2NTk2OTI4NywxNTMuMzA5OTk4ODAyNTQ3NjggMTcuMDA2NjU4Mjk2OTAzMDc2LDE1Ny43Nzc3OTkyMzQ3ODkxNyA3Ljg2MzM5MDg2MDIxNzY1NiwxNjIuNzcyMTg4NDQ3MjU4MTIgQzUyLjEzMDU5NTk0NDcyMzkzLDE0My40NTgzODg5NzI0NTkzNSAxNjEuMjkxNDIyMzEyNTgyLDExNy4zMTI3MTI4ODc1MTk2NCAyODQuODQ0Mjg4MzQwMDQyNzQsMTMzLjMyOTcwODQ4NjEwNzkgYy0wLjU4OTU4MjY0NDUyODgxMjgsLTAuNjU0ODQyNTc1NDExNzQzNiAtNS4zODAzNTY0NjQ2NDMxNzQ1LC0xLjkwMzQ0MDE5NTE5NTYzMjggLTEzLjQzMDEzNjgyOTA1Mzg5MSwtMy4yODgzOTU3NDMxOTI4NDE0ICIgc3Ryb2tlLXdpZHRoPSIwIi8+CiAgICAgICAgICA8L2c+CiAgICAgICAgPC9nPgogICAgICA8L2c+PC9nPjwvc3ZnPg==)

![](docs/tests_results/Linux/badge.svg) ![](docs/tests_results/Windows/badge.svg) ![](docs/tests_results/Darwin/badge.svg)

# `bbt` README <!-- omit from toc -->

- [Overview](#overview)
  - [What does the behavior description look like?](#what-does-the-behavior-description-look-like)
  - [Partial parsing](#partial-parsing)
  - [Step arguments](#step-arguments)
  - [One more example](#one-more-example)
- [Installation](#installation)
  - [Stable version](#stable-version)
  - [Latest version](#latest-version)
- [First use](#first-use)
- [Why should I use bbt?](#why-should-i-use-bbt)
  - [Write once](#write-once)
  - [Write a real documentation, not just a ".feature" Gherkin file](#write-a-real-documentation-not-just-a-feature-gherkin-file)
  - [Be proficient in no time](#be-proficient-in-no-time)
  - [No-fuss no-wait run](#no-fuss-no-wait-run)
  - [Ready to publish output](#ready-to-publish-output)
- [Status of the project](#status-of-the-project)
- [Help and comments](#help-and-comments)
- [Further reading](#further-reading)

## Overview

bbt is a simple tool to black box check the behavior of an executable through [Command Line Interface (CLI)](https://en.wikipedia.org/wiki/Command-line_interface).  
Hence the name: bbt stands for *Black Box Tester*.  

bbt targets both *specification of the behavior* and *end-to-end test automation* for the very common case of apps taking some input and producing some output.  
It enable developers **to write and execute comprehensive test scenarios in just a few minutes**. 

The outstanding feature of btt is that **it directly uses your behavior documentation in plain english**.  
There is no script nor other file to write.

### What does the behavior description look like?

The behavior is described in almost natural English, in Markdown, using the [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) / [Gherkin](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language) usual pattern *Given / When / Then*.  
Here is a minimal example:  

```md
### Scenario: I want to know gcc version

- When I run `gcc --version`
- Then the output contains `14.2.0`
```  
resulting in:

### Scenario: I want to know gcc version  <!-- omit from toc -->

- When I run `gcc --version`
- Then the output contains `14.2.0`

bbt being about documentation and simplicity, Markdown[^1] is a perfect fit.  

Let's consider a slightly more complete example:
![simple example](docs/rpl_example.png)

(Markdown source [here](docs/examples/rpl_case_insensitivity.md))

We have:

1. An "Overview" header and a first text   
   All this is ignored, because: 
   - bbt processes **only** Gherkin headers *# Features*, *# Background*, and *# Scenario* or *# Example*.   
   - bbt considers all text lines as comment, except Step lines.  
   
   bbt staying out of the way, you're free to use markdown almost without constraints to draft nice documentations. 
   
1. A "Feature" and a "Scenario" header (followed by the feature/scenario name)  
   bbt is now awake, and waiting for step lines.  
   Note that header's level is ignored (*#### Scenario*, is equal to *# Scenario* for bbt), you're free to structure the file as you want. 

2. Steps  
   Steps are lines starting with *- Given*, *- When*, *- Then*, *- And*, *- But*, that contain the things to check or do.

### Partial parsing 

A distinctive feature of bbt is that it seems to directly understand those almost normal English sentences like:  
```
- When I run `sut --quiet input.txt`
- Then there is no output
```
This is achieved thanks to a [partial parser](https://devopedia.org/natural-language-parsing). It means that there is no rigid grammar, because bbt takes into account only some keywords to recognize the skeleton of the sentence.  

So when you write:  
> - Then I should get `version 15.0.0` (Fix #2398 and #2402)    

bbt actually reads:  
> - then get `version 15.0.0`     
  
And this is what gives the ability to write steps in almost natural language. 

### Step arguments

Step's argument may be strings or multiline text.

As per [MDG](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin), bbt uses :
- multiline text (expected output, file content, etc) : [fenced code blocks](https://spec.commonmark.org/0.31.2/#fenced-code-blocks), that is a text between two "```" lines
- strings (file name, command to run, etc.) : [code span](https://spec.commonmark.org/0.31.2/#code-spans), that is a string between backticks  

It's not only to nicely highlight inputs in the doc, but also because otherwise the analysis of the steps would be too complex.  
 
### One more example

[This example](docs/examples/gcc_hello_word.md) shows how simple it is to run a `gcc` sanity test, that compiles and runs the ubiquitous *Hello Word*.

## Installation

### Stable version

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbt.json)](https://alire.ada.dev/crates/bbt.html) is available on Windows, Linux and Darwin thanks to the Alire package manager:

1. Go to [Alire home](https://alire.ada.dev/) for a “one click” install.  
   
2. Run :
   > alr install bbt

   The exe will be moved in ~/.alire/bin on Linux and Darwin, or in xxxx on Windows.  
   Alternatively, you may choose another installation directory with:
   > alr install --prefix=/path/to/installation bbt  

   If needed, ensure that the installation directory is in your PATH.

### Latest version

For Linux user, an AppImage of the latest version is available [here](https://github.com/LionelDraghi/bbt/releases).  
(Thanks to @mgrojo and [Alr2AppImage](https://github.com/mgrojo/alr2appimage)).  
Download the AppImage, and:  
```sh
chmod +x bbt-0.1.0-x86_64.AppImage
ln -s bbt-0.1.0-x86_64.AppImage bbt
```

Or, to build the latest version on Windows, Darwin or Linux:
```sh
git clone https://github.com/LionelDraghi/bbt  
cd bbt  
alr build 
```

## First use

The easiest way to start is illustrated by Simon in it's [ada_caser](https://github.com/simonjwright/ada_caser/tree/main) project.  
He just created a scenario file called `tests.md`, and put in the README a reference to that file and the command line to run the tests.  
Thats'it, he didn't even need to create a "tests" directory.

## Why should I use bbt?

### Write once

Specification is the only source of truth. This is bbt most interesting feature, there is nothing else: no intermediate representation, no glue code, no scripting language, no duplication of the original source at all.  

With two main consequences: 
1. writing a test is a matter of minutes,
2. there is no more place for a discrepancy between documentation and tests.

Alternative tools exist, some are mentioned in [my quick overview of some comparable tools](docs/comparables.md).  
But as far as I know, **bbt is the only one to provide such a direct "run the doc" approach**.

### Write a real documentation, not just a ".feature" Gherkin file  

bbt effectiveness does not come at the cost of limiting documentation readability or expressiveness: 

- First, the vast majority of the file is just plain markdown : use it, structure it the way you like, give as much context as you want, and use all Markdown cool extensions (for example graphics with [Mermaid](https://mermaid.js.org/intro/));
- Second, even the part that is interpreted by bbt, the steps, is written in readable English thanks to the partial parsing.

Nice consequence, bbt scenarios may be written by non coders people.

### Be proficient in no time

bbt Steps uses a limited English subset, with a vocabulary dedicated to test with no-surprise keywords like *run*, *output*, *contains*, etc.  

Although simple, you don't have to learn this subset by heart, you may :
- ask for a template scenario by running `bbt create_template` (short form `bbt ct`), or
- ask for the complete grammar with `bbt list_grammar` (short form `bbt lg`).  

### No-fuss no-wait run

To run a scenario : `bbt my_scenario.md`  
To run all the md files in the *tests* tree `bbt -r tests`  
To run only a selection `bbt --select "Sanity check" tests`  

bbt has no dependencies on external lib or tools (diff, for example) and can be run as is on major native platforms.  

### Ready to publish output 

bbt output is in Markdown format. You can adjust the detail level with the usual "-q" and "-v" options.

The output cross-references the executed scenario files: if a test fails, just click on the link and you are in the scenario.  
You can push it on GitHub without further processing.  

To see what it looks like, consider [bbt own tests](docs/tests_results/Linux/features_results.md).  

Test results are generated when running `bbt`, by just using the `-o` option (`--output`).
  
## Status of the project

bbt is in an early stage, meaning that interface and behavior are subject to changes.  
Feel free to make features suggestions [in bbt discussions](https://github.com/LionelDraghi/bbt/discussions). 

The code has grown fast in 2024, and is far from being clean.  
Nevertheless, bbt is working, and has as a serious [test base](docs/tests_results/Linux/features_results.md).  

A very conclusive test on the effectiveness of bbt as being conducted on [the day 4 of Advent of Code 2024's challenges](https://github.com/LionelDraghi/Advent_of_code_2024/blob/main/day_04_tests.md).   
Tests where easy and fast to setup, allowing to stay most of the time focus on coding.

In real life, the [acc](https://github.com/LionelDraghi/ArchiCheck) project has largely migrated to BBT, resulting in a drastically reduced number of files and a significant gain in maintenability and readability of the tests.  
Other people are using it too.  

btt compile on Linux, Windows and Mac OS, and the test suite is run on the three platforms.  
On MacOS, it may be useful to set the environment variable GNAT_FILE_NAME_CASE_SENSITIVE to 1, cf. discussion [here](https://forum.ada-lang.io/t/name-file-casing-error-on-darwin/1795) to avoid small glitches on file names.  

## Help and comments
Comments are welcome [here](https://github.com/LionelDraghi/bbt/discussions)

## Further reading
- [User Guide](docs/UG.md): concepts, command, features...
- [Developer Guide](docs/developer_guide.md): design overview, issues, fixme...
- [References](docs/references.md): syntax, grammar, and more details on non obvious behavior
- [Project status](docs/project.md): changelog, tests, TDL...
- [Command line help](docs/bbt_help.md)

[^1]: More precisely, this is a subset of the existing [Markdown with Gherkin (MDG)](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md#markdown-with-gherkin) format.  

