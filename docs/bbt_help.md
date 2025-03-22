# Command line help
```

Usage : bbt [Options]* [Command] [bbt_file]*

If no file name provided, read *.md (recursively if "-r")

Commands:
   run                   : the default command
   list                  : list selected items
   lf  | list_files      : list Scenario files found
   lk  | list_keywords   : list Step keywords
   lg  | list_grammar    : list rules for Step analysis
   ex  | explain         : explain what bbt understands from Scenarios files
                              (do not run the scenarios)
   ct  | create_template : create a commented example of rules file
   he  | help            : this message

Options:
          --yes            : do not prompt if deletion is needed in
                             "Given" steps, silently answer yes
          --strict         : warn when not strictly following Gherkin common guidelines
   -c   | --cleanup        : after run, remove every file and dir
                             created by bbt in "Given" steps
   -r   | --recursive      : search bbt files in subdir
   -k   | --keep_going     : Do as much work as possible
   -v   | --verbose
   -q   | --quiet          : no message unless error,
                             Warnings are also ignored
   -o   | --output file.md : create an md file with test results
                             that indexes all scenarios run.
                             This file will contain the normal bbt output,
                             whatever are the verbosity settings (-q, -v, etc.)
                             for standard output.
   -ed  | --exec_dir 'dir' : run command in dir instead of current dir
   -td  | --tmp_dir 'dir'  : create .out file in dir instead of current dir
   -gb  | --generate_badge badge.url : create a text file containing
                                       a shields.io URL to get a svg badge
                                       with tests results summary.

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

Debug command:
   -lt                    : list log topics
   -ls                    : list settings
Debug options:
   -t topic               : activate log related to the topic

bbt version 0.2.0-dev
https://github.com/LionelDraghi/bbt/

```
