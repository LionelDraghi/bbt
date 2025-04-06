# Command line help
```

Usage : bbt [Options]* [Command] file*

  The default command is 'run'
  If no file is provided, reads *.md files

Basic options:
       --yes        : do not prompt if deletion is needed in
                      "Given" steps, silently answer yes
  -c | --cleanup    : after run, remove every file and dir
                      created by bbt in "Given" steps
  -r | --recursive  : search scenarios in subdirs
  -k | --keep_going : do as much work as possible
       --Werror     : treat warnings as errors
  -v | --verbose
  -q | --quiet      : no message unless error,
                      Warnings are also ignored

Basic commands:
       run               : the default command
  ls | list              : list selected items
  ct | create_template   : create a commented example of rules file
  he | help [topic]      : base help, or more on one of the topic listed below
  he | help on_all       : full online help

Help topics:
  filtering : --select --exclude --include
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines
  other     : list_files list_keywords list_grammar explain create_template
              --strict --output file.md --exec_dir --tmp_dir --generate_badge
  debug     : -lt -ls -t

bbt version 0.2.0-dev
https://github.com/LionelDraghi/bbt/

Filtering:
  Features, Scenarios and Steps may be selected or filtered.
  By default, every item is selected.
  -s | --select 'string'  : only items containing 'string' are selected
  -e | --exclude 'string' : remove from selection items containing 'string'
  -i | --include 'string' : include in selection items containing 'string'
  Multiple occurrences are processed in order, meaning that you can exclude
  a whole Feature and then re-include a Scenario belonging to this feature.

Human vs exact matching:
  bbt default behavior is "human match", that is ignoring differences
  in casing, ignoring consecutive spaces, and ignoring blank lines.
  The opposite behavior, to make strict compare, is set with:
  -em  | --exact_match
  exact_match may be altered if **followed** by one or more of:
  -iw  | --ignore_whitespaces (default)
  -ic  | --ignore_casing      (default)
  -ibl | --ignore_blank_lines (default)
  For example, "-em -iw" will take into account blank lines and
  casing but ignore whitespaces
  Note that -iw, -ic, and -ibl are useless if not preceded by -em, 
  because they are the default setting.
  There is also a
  -hm  | --human_match
  option, equivalent to defaults "-iw -ic -ibl", if you want to
  assert on the command line that this is the required behavior.

Other commands:
  lf | list_files      : list Scenario files found
  lk | list_keywords   : list Step keywords
  lg | list_grammar    : list rules for Step analysis
  ex | explain         : explain what bbt understands from Scenarios files
                         (do not run the scenarios)

Other options:
        --strict         : warn when not strictly following Gherkin common guidelines
  -o  | --output file.md : create an md file with test results
                           that indexes all scenarios run.
                           This file will contain the normal bbt output,
                           whatever are the verbosity settings (-q, -v, etc.)
                           for standard output.
  -ed | --exec_dir 'dir' : run command in dir instead of current dir
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir
  -gb | --generate_badge badge.url : create a text file containing
                           a shields.io URL to get a svg badge
                           with tests results summary.
```
