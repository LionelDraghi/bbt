# *bbt* FAQ (Frequently asked Questions) <!-- omit from toc -->

## What are the differences between Cucumber Gherkin and *bbt* Gherkin  

Some Gherkin features are not implemented in *bbt*:
* table input (`Scenario Outlines` / `Examples`). 
* the new "Rule" keyword (I don't know what it is for)

Some differences :
* the file structure of gherkin files is simple: one feature per file, optionally a background, and some scenarios. *bbt* is more flexible, zero, one or more features, one optional background at document level **and** one per feature;
* in Gherkin, tags are words starting with "@" put just below the scenario title. With *bbt* there is no special marker, whatever string may be a tag. You can use @windows if you want.
(in *bbt* tests, to specify different file path format output, I use Windows_Only and Unix_Only as tag)

Some features are specific to *bbt*:
* for example the ability to specify a common output on several commands, within a single scenario: 
  > - when I run `add "1+2"` or `add "2+1"` or `add "0+3"`
  > - then i get `3` 

But the most important difference is in the use: 
* gherkin is a doc format, it has no defined semantic within scenarios: to run stuff, you have to write code. 
* *bbt* is dedicated to cli black box testing, you do not have to write code, but you cannot use it for unit or integration or GUI testing.