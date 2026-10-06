<!-- omit from toc -->
# Changelog

All notable changes from a user perspective to this project will be documented in this file.  

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.1.0/), (guidelines at the bottom of the page).  
Version numbering adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

- **[0.4.2-dev] - 2026-??-??**
  - [Added]   `When I type Y` and `When I enter Y` steps, to interact with a command waiting for user input: `type` sends the text without a trailing newline, for programs reading a single key, and `enter` sends it followed by a newline, for programs reading a whole line. The command is started without waiting for its termination, and the output checks following an input step apply to the output produced after it: the prompt displayed before the input is ignored. Command execution is now based on the Spawn library (see docs/features/A290_When_I_Type_Or_Enter.md)
  - [Added]   a warning when a run command contains shell metacharacters (`|`, `$`, backtick, `<`, `>`, ...), as commands are not run through a shell
  - [Fixed]   this warning was raised on quoted metacharacters (e.g. `find . -name "*.ad[sb]"`), although quoting is exactly the way to pass a literal metacharacter as an argument
  - [Fixed]   an empty actual output compared to a non empty expected content raised a CONSTRAINT_ERROR and silently passed
  - [Fixed]   `bbt explain` crashed when a step with a missing code block was followed by another scenario header
  - [Fixed]   with `--keep_going`, an output checking step following a command that could not be spawned crashed with an unhandled NAME_ERROR instead of reporting a clean error on the missing output file
  - [Fixed]   CRs at line ends, as found in files or output produced on Windows, were significant in human match mode, although human match is supposed to ignore text presentation: they are now ignored like other whitespaces, and remain significant with `--exact_match`
  - [Fixed]   on Windows, command execution was broken since the move to the Spawn library: every command after the first failed with "Couldn't run", and bbt crashed at end of run or at scenario end, because the same Spawn process object was reused for all commands, which the Spawn Windows monitor does not support; a fresh process object is now created for each command, and disposed of once reaped
  - [Fixed]   on macOS, freeing the process objects corrupted the heap: the Spawn monitor never removes terminated processes from its pid map (cf. [spawn#36](https://github.com/AdaCore/spawn/issues/36)), and when the OS reuses a pid, the exit status was written into freed memory, crashing bbt at positions moving from run to run; the process objects are now kept alive for the whole run
  - [Added]   interactive scenarios testing the erasing confirmation of files and directory trees, including through the `new` keyword, the answer being sent to a nested bbt by a `When I type` step (cf. B030_File_creation_in_Given_steps.md)
  - [Fixed]   ``Given the new directory `dir1` `` now really starts from a white page: an existing `dir1` tree is erased, after user confirmation, or silently with `--yes`, and the directory is recreated empty; it used to leave an existing tree in place, and the step failed to detect it (cf. B030_File_creation_in_Given_steps.md)
  
- **[0.4.0] - 2026-10-03**
  - [Added]   An agent skill for AI coding agents to write, convert, run, and debug *bbt* scenarios, installable with `npx skills add LionelDraghi/bbt --skill bbt-skill` (see the new "For AI coding agents" section in the README)
  - [Added]   `Then the error output is | contains | does not contain ...` and `Then there is no error output`: when a scenario checks the error output, its commands' standard error is captured apart from the standard output
  - [Added]   `Then the exit code is n` checks the exact exit code of the last command
  - [Added]   `Given the environment variable NAME is value | is not set`
  - [Fixed]   `Then I get [no] error` read the exit code from an uninitialized variable
  - [Changed] error messages now display expected and actual side by side, in a
               more readable sdiff inspired format, with diff hunk headers (`@@`)
               to locate the problem, and a few lines of context around each difference
  - [Changed] help reorganization: `bbt explain` is now documented in the base
               help, the deprecated `lg | list_grammar` and `lk | list_keywords`
               commands are no more advertised (use `bbt help grammar` and
               `bbt help keywords`), and `bbt help on_all` no more dumps the
               grammar and keywords tables

- **[0.3.0] - 2026-04-26**
  - [Fixed]   `bbt list_files` no more return on error when no file found
  - [Added]   `bbt explain` command rewritten and tested, now usable
  - [Changed] `bbt create_template` deprecated and replaced with `bbt help tutorial` and `bbt help example` 
  - [Changed] `bbt lg | list_grammar` now produce a Markdown table **with an example** for each recognized syntax
  - [Added]   `Then I successfully run` syntax added
  - [Added]   ``` When I run `x` or `y` ``` syntax added
  - [Added]   ``` Given | Then `cmd` fails ``` syntax added

- **[0.2.1] - 2026-03-04**
  - [Added]   Feature #28: production of a junit.xml file through --junit option
  - [Added]   new `file matches regexp` syntax

- **[0.2.0] - 2025-07-02**
  - [Changed] `--output` is now deprecated and replaced with `--index` 
  - [Changed] Removed option form of commands : `help` is a command, `-h` is removed (as announced in 0.1.0)
  - [Changed] The non essential explanation in the online help have bee moved to separate topics
  - [Added]   Close #21, first implementation of documents/features/scenario/steps selection through `--select` `--exclude` `--include` options.
  - [Added]   AppImage generation added by @mgrojo
  - [Fixed]   `--cleanup` now correctly removes directories tree (fixes #3)
  - [Added]   Processing of Asciidoc input (.adoc files) added (proto)

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
  - [Changed] An empty code block (that is two consecutive `` ``` `` lines) is no more considered as an error, but just as a file intentionally empty.
  - [Added]   `Output matches regexp` syntax added.
  - [Fixed]   Fixed run summary printed even when nothing was run because of an early error occurs during scenario analysis.
  - [Added]   `-Given the file containing` now accept code fenced block content.
  - [Changed] The template file (produce with -ct) is now more complete, so that a user could start with it without reading the doc.
  - [Added]   Added robustness tests on missing code block marks in scenario files.
  - [Changed] It's now possible to use both `` ``` `` and ~~~ for code block marks. As per Markdown rules, the closing mark has to be the same as the opening one.
  - [Added]   First implementation of a progress bar, `-sb` option
  - [Changed] On the command line, commands no more start with '-' or '--' (previous form still taken into account for now)
  - [Fixed]   Fixes #7 
  - [Added]   "not equal to file" syntax added

- **[0.0.6] - 2024-14-12**
  - [Fixed]   Ambiguity in all steps with a `string` object : if there is the `file` keyword in the object
              part of the step, then the expected output is in the file, otherwise, it's the string.
  - [Fixed]   `explain` output is now readable!
  - [Added]   `-k` | `--keep_going` implemented. Without, bbt stop when a test fails.
  - [Added]   *file is equal to file* syntax added (thanks to [Paul](https://forum.ada-lang.io/u/pyj)!)
  - [Added]   `unordered` keyword added to get the comparison of actual and expected output/file insensitive to line order
  - [Added]   `--strict` implemented to get warning on steps not in GWT order
  - [Added]   `output|file doesn't contain` syntax added
  - [Fixed]   .out files sometimes created in .md dir and not in Exec_Dir
  - [Fixed]   .out files not removed when using `--cleanup`
  - [Fixed]   Incoherencies between documentation removed regarding Markdown syntax and bbt step's syntax
  - [Fixed]   Normal output verbosity is now more balanced (it was pretty much identical to `--verbose`)  
  - [Changed] "uut" renamed "sut", because bbt is precisely not about Unit Under Test, but Software Under Test.
  - [Added]   "sut" augmented to be able to check future feature about Environment variable and user prompting.
  - [Changed] Files are now processed in alphabetic order, and displayed accordingly by  `--list_file`
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
  - [Added]   text file creation

- **[0.0.2] - 2024-06-04** 
  - [Added]   automatically delete files and directories if needed in "Given" steps
    
- **[0.0.1] - 2024-05-13**
  - [Added]   `background` feature
  - [Added]   bbt `directory` keyword and related creation/check operations

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

