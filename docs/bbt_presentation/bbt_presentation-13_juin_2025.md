---
marp: true
size: 16:9
theme: default
lang: en-US
title: Introduction to bbt
paginate: true
class: 
- noinvert
- lead
transition: fade

style: |
   section {padding: auto;}
   .small-text {font-size: 0.75rem;}
   .center {text-align: center;}
   h1 {text-align: center;
       font-size: 2rem;
       font-weight: medium;
       text-shadow: 3px 3px 2px rgba(49, 165, 159, 0.5);}
   h2 {font-size: 1.5rem;}   
   h3 {font-size: 1.2rem;
       font-weight: normal;}   
---

<!-- color: navy -->

# Using natural language for test specification, is that really wise?
# <div></div>
# An introduction to bbt
# <div></div>
# <!-- fit --> Lionel Draghi - Ada Developers Workshop - 13 June 2025

---

<!-- footer: "Lionel Draghi - AEiC 2025 / Ada Developers Workshop - 13 june 2025" -->

# Agenda
## Part 1 - Introduction to bbt
## Part 2 - Partial Parsing
## Part 3 - Surviving an ambiguous world
## Conclusion

---

<!-- header: "**[Introduction to bbt](#part-1---introduction-to-bbt-1)** | [Partial parsing](#part-2---partial-parsing-1) | [Surviving an ambiguous world](#part-3---surviving-an-ambiguous-world-1) | [Conclusion](#conclusion-1)" -->


# Part 1 - Introduction to bbt

---

## Please install bbt now! 

(We will use it during the presentation) 

- Stable version
  ~~~sh
  alr install bbt
  ~~~

- Latest version, AppImage
https://github.com/LionelDraghi/bbt#installation

<!-- 
-->

---

## Whoami

### Lionel Draghi

<small>

- 15 years as Ada software developper
- Retired from software dev in 2007
- Author of softwares that no one uses: Archicheck, smk
  (https://github.com/LionelDraghi)
- And author of bbt, which, by comparison, made a thunderous debut with 3 contributors and the Ada Crate of the Year award!

![bg right:40% 80%](portrait.jpg)
</small>

<!-- 
While you install the software, I'll continue with the least important part of the presentation
I used to be a senior software developer, I am now much more a senior than a software developper.
I am the author of those brilliant and useless software, whose merit will have been to give rise to BBT.
Will bbt be another tombstone in the middle of the giant software graveyard that is GitHub?
-->

---

## What is bbt?

* `bbt` is a dead-simple tool for _end-to-end_ testing of your command line apps

* It targets Apps reading some input and writing some output, like for example `grep` or `gcc` 
  :warning: Not suitable for UI testing, unit testing, etc.

* `bbt` inputs are markdown files, embedding descriptions of an expected behavior in Gherkin format, **with the steps in natural language** 

* From a practical point of view, you really run the doc : `bbt my_spec.md`

<!-- 
suitable for all cases, but who has never had to test an app of this type?
To get it more concrete, I propose to make a first demo.
-->

---

# Live demo - Getting started

<!-- 
1. check that bbt is in your PATH
1. creation d'un repertoire + fichier demo.md
1. lancer bbt avant qu'il y ait un scenario
1. lancer bbt avant qu'il y ait un step
1. creation test grep -i Rose flower.list
-->

---

## What I wanted to achieve  

<small>

* **A single source of truth**, minimizing overlaping definitions
  Initialy, behavior is defined in some docs, and further refined in tests description. 
  But quickly, the tests evolve independently because the developer is focused on them, and they end up contradicting the readme file or the user guide.
    
* **Put things in the right order**
  If the doc is the source of truth, then tests should come from it, not the other way around.
    
* **Whatever the documentation**, specifications, acceptance test, user guide, readme file... They are all valid source of truth. 

  :arrow_right: Even those slides! (written in Markdown, using Marp)

<!-- ![bg right:30% 80%](acc_test.png) -->

</small>

<!-- 
The inconsistencies are an epidemic in the software world, sometimes with benign effects (e.g., an error in an example), and sometimes with more serious consequences.
Why is it so? Because the truth of our software is in the User Guide, is in the sources comments, is in the tests definition, etc. 
Truth is a complex distributed system!
All systems end up with discrepancies, because we, developpers, are mostly focused on tests and code. But we have no easy way to check that the whole existing documentation is compliant with the latest state of the software. 
For example, examples in the user guide, are they up to date?

If it's true for litterate programming, it's also true for testing.
-->


---

# <!-- fit --> Live demo 2 - Let's create a runnable User Guide 

<!-- 
- Let's ask to some LLM :
> could you write a simple user guide for the rpl utility (string replace), with some use examples, in Markdown, with a toc, some table and a mermaid diagram?
- [backup file](./rpl_ug.md)
- paste dans vscode, vision en markdown
- run avec bbt, ne doit rien faire
- transformation des exemples en script bbt
- run du test !
-->

---

<!-- header: "[Introduction to bbt](#part-1---introduction-to-bbt-1) | **[Partial parsing](#part-2---partial-parsing-1)** | [Surviving an ambiguous world](#part-3---surviving-an-ambiguous-world-1) | [Conclusion](#conclusion-1)" -->

# Part 2 - Partial Parsing

---

## A word on Partial parsing

# <div></div>

In the NLP field, **partial parsing**, also known as **shallow parsing**, **light parsing** or **chunking**, occupies a position between simple tokenization and full syntactic parsing.
- Deep parsing and understanding can be complex and is not always necessary
- Shallow parsing is simpler and faster
- However, it may be ambiguous and lack precision

---

## <!-- fit --> Shallow parsing is based on word spotting

![bg right:40% w:500](44129332940.jpg)

<small> 

Example: *Eliza* (1966), the famous psychotherapist emulator

* The logic behind may be as simple as
  ~~~Ada
  if Answer.Contains ("you") then
     Ask ("You're not really talking about me, are you?");

  elsif Answer.Start_With ("no") then
     Ask ("Why not?");
  ...

  else
     Ask ("I see.");
  ~~~
* But nevetheless very wise
  ~~~Ada
  elsif Answer.Contains ("rust") then   
     Ask ("What's your problem with Ada?");
  ~~~

</small>

---

## Consider those bbt steps:

~~~
- Given there is no existing `.config` file
- When I run `my_app --init` 
- Then there is a `.config` file
- And the file `.config` contains `autosave = true`
~~~

# <div></div>

##  It may seem complex at first glance, but is it?

---

<small> 

**The same steps :**
~~~md
- Given   there                is no      existing `.config` file
- When    I                    run        `my_app --init` 
- Then    there                is         a `.config` file
- And     the file `.config`   contains   `autosave = true`
~~~

* All sentences have the same simple structure, in the same order
  **preposition + subject phrase + verb phrase + object phrase**

* The Markdown syntax is helping : parameters are between backticks, no possible confusion with keywords

* A small lexicon: about 20 keywords to build all the possible sentences

* And sentences are very repetitive! 

* ## In fact, bbt is a perfect fit for partial parsing

</small>

<!-- 
no need for NER (Named Entity Recognition)
-->

---

## bbt implementation (1/4) : Tokenization

<small> 

**Consider the step :**
~~~gherkin
- Given there is no existing `.config` file
~~~

# <div></div>

**Tokenization**

   Given  |  there  | is    | no    | existing | `` `.config` `` | file
   -------|---------|-------|-------|----------|-----------------|-----
   keyword|*ignored*|keyword|keyword|*ignored* |parameter      |keyword

</small>

<!-- 
1. the no need for Part of Speech Tagging. In the code, there are subtypes of the token enums named adjectives, preposition, etc. 
But actually The only phrase that need to be identify is the verb phrase
2. And that's because of the chunking
-->

---

## bbt implementation (2/4) : Chunking

<small> 

Before the verb, it's the subject chunk, after the verb it's the object chunk.
And if it's a Markdown code span (or a code block), it's a parameter.

# <div></div>

Chunk: | Preposition | Subject phrase | *Subject parameter* | Verb phrase | object phrase | *Object Parameter*
-------|-------------|----------------|---------------------|-------------|---------------|--------------------
Token: | *Given*     |                |                     | *Is_No*     | *File_Name*   | *.config*

</small>

<!-- 
Parameters, between backticks in Markdown, are easy to identify (`` `.config` ``)
For french people, beware of the false friend : verb phrase means groupe verbal 
-->

---
## bbt implementation (3/4) : Grammar

<small> 

The Grammar is a table of actions indexed by (preposition, Subject, Verb, Object...)
   
For example here : 
~~~Ada
Grammar (Preposition => Given, Verb => Is_No, Obj_Attrib => File, ...) := Setup_No_File; 
~~~

# <div></div>

Note : you can display the grammar with `bbt lg` (or `bbt list_grammar`)

</small>

<!-- 
-->

---
## bbt implementation (4/4) : Actions

<small> 

* The action and the parameters are stored in a Tree that represent a bbt document (that is a list of features containing a list of scenarios, containing etc.) 

* When all documents are parsed, a runner walk through and run actions in sequence.
  For that precise step:
  ~~~Ada
  Setup_No_File (Subject_Param => "", Object_Param => ".config");
  ~~~

# <div></div>

Note: there is a dry run mode `bbt ex` (or `bbt explain`)

</small>

<!-- 
-->

---
## Current State
- Token : less than 40
- Grammar definition : about 50 lines 
- 640 SLOC of code for lexing and parsing 

# <div></div>

Not at all a code I am proud of yet (could be easier to read and more robust)

But it is able to "understand" sentences like:
~~~
- Then the resulting `log.txt` file does not contain any `Error:`
~~~

---

<!-- header: "[Introduction to bbt](#part-1---introduction-to-bbt-1) | [Partial parsing](#part-2---partial-parsing-1) | **[Surviving an ambiguous world](#part-3---surviving-an-ambiguous-world-1)** | [Conclusion](#conclusion-1)" -->

# Part 3 - Surviving an ambiguous world

---

## Example of Ambiguity detected by bbt

# <div></div>

~~~md
- given there is no `config` file in the current directory
~~~
:bomb: In the Object chunk, there is both `file` and `directory` keywords...

# <div></div>

~~~md
- then the output contains what is in the file `simple.ads`. 
~~~
:bomb: two verbs, `contains` and `is`...

### May be detected because both word are in bbt's vocabulary

<!-- 
-->

---
## <!-- fit --> The worst case : bbt understand the opposite of what is said
# <div></div>

~~~md
- then the output never contains `Error`
~~~

# <div></div>
:bomb: `never` is not a keyword, this will indeed check that the output contains `error`

### Can't be detected because one of the word is ignored by bbt

<!-- 
-->

---

## In practice, it's very unlikely if you stick to the usual best practices for writing specifications: 
* Be clear and **concise**, avoid double negation, be consistent (don't innovate on formulations), etc.

Plus:
* Put complex comments on separate lines, not in the steps
* Uses `bbt explain` in case of doubt
* Check at least once the real output!!

<!-- 
-->

---

# But the Ambiguithon is still open: find a more serious error case and be awarded!

# <div></div>

<center>
https://github.com/LionelDraghi/bbt/issues

<!-- 
-->

---

<!-- header: "[Introduction to bbt](#part-1---introduction-to-bbt-1) | [Partial parsing](#part-2---partial-parsing-1) | [Surviving an ambiguous world](#part-3---surviving-an-ambiguous-world-1) | **[Conclusion](#conclusion-1)**" -->

# Conclusion

---

# Special thanks to

* ## AdaCore 

   - :heart: For awarding bbt __Ada Crate of the word 2024__ (https://blog.adacore.com/)

* ## The early adopters and contributors

   - :heart: Paul   (https://github.com/pyjarrett)
   - :heart: Manuel (https://github.com/mgrojo/coap_spark)
   - :heart: Simon  (https://github.com/simonjwright/ada_caser)

<!-- 
-->


---

# Let's make documentation great again!  

# <div></div>

# #runthedoc

<!-- 
The final word is not mine
-->

---

# <div></div>

# Q & A

# <div></div>
# <div></div>
# <div></div>

<small>

Ideas and features are welcome at https://github.com/LionelDraghi/bbt/discussions 

Slides made with [marp](https://marp.app/) available here https://github.com/LionelDraghi/bbt/blob/main/docs/bbt_presentation/

</small>

<!-- 
Everything is open to discussion!
-->



