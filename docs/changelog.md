<!-- omit from toc -->
# Changelog

All notable changes from a user perspective to this project will be documented in this file.  

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.1.0/), (guidelines at the bottom of the page).  
Version numbering adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

- **[Latest] - 2025-??-??**
  - [Changed] Removed option form of commands : "help" is a command, "-h" is removed (as announced in 0.1.0)
  - [Changed] The non essential explanation in the online help have bee moved to separate topics
  - [Added]   Close #21, first implementation of documents/features/scenario/steps selection through `--select` `--exclude` `--include` options.
  - [Added]   AppImage generation added by @mgrojo
  - [Fixed]   --cleanup now correctly removes directories tree (fixes #3)
  - [Added]   Processing of Asciidoc input (.adoc files) added

- **[0.1.0] - 2025-03-03**
  - [changed] option and command on command line now accept both '_' and '-' separator (you can use both `--keep_going` and `--keep-going`)
  - [Added]   *Human match* versus *Exact match* concept added, with `-em`, `-ic`, `-iw` and `-ibl` options
  - [Added]   keyword `executable` added to create scripts
  - [Added]   "Crate of the year" and tests results badges added!
  - [changed] Close #5 and #15 (more clear msg on spawn problems and auto find the exe in PATH)
  - [Added]   Tested on MacOS Ventura 13.6 Intel CPU
  - [changed] Close #10 (final counts formatted as MD table)
  - [changed] Close #11 (quote surrounding parameters now removed when spawning a command)
  - [changed] Close #9  (error code block fenced instead of line prefixed with "|")
  - [Changed] An empty code block (that is two consecutive ``` lines) is no more considered as an error, but just as a file intentionally empty.
  - [Fixed]   Fixed run summary printed even when nothing was run because of an early error occurs during scenario analysis.
  - [Added]   `Given the file containing` now accept code fenced block content .
  - [Changed] The template file (produce with -ct) is now more complete, so that a user could start with it without reading the doc.
  - [Added]   Added robustness tests on missing code block marks in scenario files.
  - [Changed] It's now possible to use both ``` and ~~~ for code block marks. As per Markdown rules, the closing mark has to be the same as the opening one.
  - [Added]   First implementation of a progress bar, `-sb` option
  - [Changed] On the command line, commands no more start with '-' or '--' (previous form still taken into account for now)
  - [Fixed]   Fixes #7 
  - [Added]   "not equal to file" form added

- **[0.0.6] - 2024-14-12**
  - [Fixed]   Ambiguity in all steps with a `string` object : if there is the `file` keyword in the object
              part of the step, then the expected output is in the file, otherwise, it's the string.
  - [Fixed]   `explain` output is now readable!
  - [Added]   `-k` | `--keep_going` implemented. Without, bbt stop when a test fails.
  - [Added]   *file is equal to file* form added (thanks to [Paul](https://forum.ada-lang.io/u/pyj)!)
  - [Added]   `unordered` keyword added to get the comparison of actual and expected output/file insensitive to line order
  - [Added]   `--strict` implemented to get warning on steps not in GWT order
  - [Added]   *output|file doesn't contain* form added
  - [Fixed]   .out files sometimes created in .md dir and not in Exec_Dir
  - [Fixed]   .out files not removed when using `--cleanup`
  - [Fixed]   Incoherencies between documentation removed regarding Markdown syntax and bbt step's syntax
  - [Fixed]   Normal output verbosity is now more balanced (it was pretty much identical to --verbose)  
  - [Changed] "uut" renamed "sut", because bbt is precisely not about Unit Under Test, but Software Under Test.
  - [Added]   "sut" augmented to be able to check future feature about Environment variable and user prompting.
  - [Changed] Files are now processed in alphabetic order, and displayed accordingly by --list_file
  - [Changed] Big Features renaming and reorg

- **[0.0.5] - 2024-10-17**
  - [Removed] `-pg` compilation option that prevented Alire integration. 
  - `bbt` version 0.0.5 is in Alire. First public announce on Reddit and ada-lang.io!
  
- **[0.0.4] - 2024-07-17**
  - [Changed] bbt now return an error status when one of the test fails
  - [Added]   first `--cleanup` implementation, that removes files created during the test by bbt
  - [Added]   in scenarios, `dir` is now a synonym of `directory`
  - [Changed] bbt bootstraps! bbt now runs bbt tests that return error code.
  - [Added]   `--exec_dir` option to run scenario in a dir different from current
  - [Added]   New `no output` syntax
  - [Added]   Interactive prompting added to delete file or dir in Given steps
  - [Added]   `--yes` option to avoid interactive prompting
  - [Removed] due to the new `--yes` option, `--auto_delete` is removed
  
- **[0.0.3] - 2024-06-30**
  - [Added] text file creation

- **[0.0.2] - 2024-06-04** 
  - [Added] automatically delete files and directories if needed in "Given" steps
    
- **[0.0.1] - 2024-05-13**
  - [Added] `background` feature
  - [Added] bbt `directory` keyword and related creation/check operations

- **[0.0.0] - 2024-05-04**
  - Initial release  
    A basic set of keywords operational, 11 tests OK, but not yet tested on my real test cases (acc and smk). 

---

[Keep a Changelog](http://keepachangelog.com/en/1.1.0/) Guiding Principles
  - Changelogs are for humans, not machines.
  - There should be an entry for every single version.
  - The same types of changes should be grouped.
  - Versions and sections should be linkable.
  - The latest version comes first.
  - The release date of each version is displayed.
  - Mention whether you follow Semantic Versioning.

Types of changes
  - Added for new features.
  - Changed for changes in existing functionality.
  - Deprecated for soon-to-be removed features.
  - Removed for now removed features.
  - Fixed for any bug fixes.
  - Security in case of vulnerabilities.

