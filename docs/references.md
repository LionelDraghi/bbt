
# References  <!-- omit from toc -->

- [Syntax](#syntax)
  - [Gherkin language subset](#gherkin-language-subset)
  - [bbt own DSL](#bbt-own-dsl)
  - [Scenario files format](#scenario-files-format)
    - [Steps](#steps)
    - [Headings](#headings)
    - [Fenced Code blocks](#fenced-code-blocks)

## Syntax

### Gherkin language subset

- *Feature*
- *Scenario* or *Example*
- *Background*
- *Given*
- *When*
- *Then*
- *And* or *But*

([Cf. Gherkin language reference](https://en.wikipedia.org/wiki/Cucumber_(software)#Gherkin_language))

### bbt own DSL 

[bbt keywords](https://github.com/LionelDraghi/bbt/blob/main/docs/keywords.md), including both the Gherkin subset and bbt specifics keywords may be obtained with `bbt -lk` (`--list_keywords`).

But more interesting, [the grammar](https://github.com/LionelDraghi/bbt/blob/main/docs/grammar.md) can be obtained through the `-lg` (`--list_grammar`) option. 

Each Step is a one line sentence, with a basic "subject verb object" structure, starting with the preposition/adverb/conjunction (*Given*, *When*, *And*, etc.). 
Add some attribute adjectives (e.g. *empty*), and here we are.

Here is an excerpt from the grammar :
```
| When  |     |        | run              | `text` | RUN_CMD                 |
```
- First, the keywords : here, `When` and `run`. 
- Then, some text or file name, between backticks.
- And, at the end, the resulting action.


### Scenario files format

The BBT Markdown subset tries to comply with [CommonMark Spec](https://spec.commonmark.org/), meaning that bbt generate Common Mark compliant Markdown.
On the other hand, restrictions apply when writing bbt scenario.


#### Steps

Because the lexer is not able to make a difference between bullet lines in steps definition or bullet lines in normal text, there are limitations on where you can use it.
- *bbt* will consider bullet line starting with `-` as comment before the first "scenario" header. 
- *bbt* will consider all lines starting with `-` as Step within a scenario. As a consequence, **Don't use `-` in comments within a Scenario.**
- *bbt* will also consider line starting with the other official bullet markers (`*`and`+`) as comment, and **not steps line marker**, so that you can use those markers where you want without interfering with the lexer.  
Our simple suggestion: use `-` for Steps and `*` for all bullet list in comments.

#### Headings

Only [ATX headings](https://spec.commonmark.org/0.31.2/#atx-headings) are supported, not [Setext headings](https://spec.commonmark.org/0.31.2/#setext-headings), meaning that you can write:
```
## Feature: My_Feature
```
but
```
Feature: My_Feature
-------------------
```
won't be recognized as a Feature.

####  Fenced Code blocks

Both ``` and ~~~ code fence mark are recognized by bbt, but only in steps, and only the first block.
Meaning that code blocks can be used within the documentation without interfering with bbt.

And, as per Markdown rules, the closing mark should the same as the opening one.
So that code blocks may include code block marks of the other type.  
For example, you can have:

~~~
> - Given the file `foo.md` 
> ```md
>
> This md file contains a code fenced block:
> ~~~
> here it is
> ~~~
> 
> ```
~~~