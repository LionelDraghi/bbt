<!-- omit from toc -->
# Changelog

All notable changes from a user perspective to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.1.0/)
and version numbering adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

- [Head]
  - [Changed] bbt now return an error status when one of the test fails
  - [Added]   first `--cleanup` implementation, that removes files created during the test by bbt
  - [Added]   in scenarios, `dir` is now a synonym of `directory`
  - [Changed] bbt bootstraps! bbt now runs bbt tests that return error code.
  - [Added]   `--exec_dir` option to run scenario in a dir different from current
  - [Added]   New `no output` syntax
  
- [0.0.3]
  - [Added] text file creation

- [0.0.2] 
  - [Added] automatically delete files and directories if needed in "Given" steps
    
- [0.0.1]
  - [Added] `background` feature
  - [Added] bbt `directory` keyword and related creation/check operations

- [0.0.0] - 2024-05-04
  - Initial release  
    A basic set of keywords operational, 11 tests OK, but not yet tested on my real test cases (acc and smk). 
