<!-- omit from toc -->
# Changelog

All notable changes from a user perspective to this project will be documented in this file.  

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.1.0/), (guidelines at the bottom of the page).  
Version numbering adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).



- [Unreleased]
  - [Added]   `unordered` keyword added to get the comparison of actual and expected output/file insensitive to line order
  - [Fixed]   .out files sometimes created in .md dir and not in Exec_Dir

- [0.0.5] - 2024-10-17
  - [Removed] `-pg` compilation option that prevented Alire integration. 
    `bbt` version 0.0.5 is in Alire. First public announce on Reddit and ada-lang.io!
  
- [0.0.4] - 2024-07-17
  - [Changed] bbt now return an error status when one of the test fails
  - [Added]   first `--cleanup` implementation, that removes files created during the test by bbt
  - [Added]   in scenarios, `dir` is now a synonym of `directory`
  - [Changed] bbt bootstraps! bbt now runs bbt tests that return error code.
  - [Added]   `--exec_dir` option to run scenario in a dir different from current
  - [Added]   New `no output` syntax
  - [Added]   Interactive prompting added to delete file or dir in Given steps
  - [Added]   `--yes` option to avoid interactive prompting
  - [Removed] due to the new `--yes` option, `--auto_delete` is removed
  
- [0.0.3] - 2024-06-30
  - [Added] text file creation

- [0.0.2] - 2024-06-04 
  - [Added] automatically delete files and directories if needed in "Given" steps
    
- [0.0.1] - 2024-05-13
  - [Added] `background` feature
  - [Added] bbt `directory` keyword and related creation/check operations

- [0.0.0] - 2024-05-04
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

