# Command line help
```

Usage : bbt [Options]* [Command] [bbt_file]*

If no file name provided, read *.md (recursively if "-r")

Commands:
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

Debug command:
   -lt                    : list log topics
   -ls                    : list settings
Debug options:
   -t topic               : activate log related to the topic

bbt version 0.0.6
https://github.com/LionelDraghi/bbt/

```
