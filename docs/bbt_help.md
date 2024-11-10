# Command line help
```

Usage : bbt [Options]* [Command] [bbt_file]*

If no file name provided, read *.md (recursively if "-r")

Command :
   run                    : the default command
   -lf  | --list_files      : list Scenario files found
   -lk  | --list_keywords   : list Step keywords
   -lg  | --list_grammar    : list rules for Step analysis
   -e   | --explain         : explain what bbt understand from Scenarios files
                              (do not run the scenarios)
   -ct  | --create-template : create a commented example of rules file
   -h   | --help            : this message

Options :
          --yes            : do not prompt if deletion is needed in
                             "Given" steps, silently answer yes
   -c   | --cleanup        : after run, remove every file and dir
                             created by bbt in "Given" steps
   -r   | --recursive      : search bbt files in subdir
   -k   | --keep-going     : Do as much work as possible
   -v   | --verbose
   -q   | --quiet          : no message unless error,
                             Warning are also ignored
   -o   | --output file.md : create a Markdown file with tests results
                             This file will contains the normal bbt output,
                             whatever are the verbosity settings (-q, -v, etc.)
                             for standard output.
   -ed  | --exec_dir       : run command in exec_dir instead of current dir
          --strict         : warning on steps not following the Given/When/Then order.

Debug command:
   -lt                    : list log topics
   -ls                    : list settings
Debug options:
   -t topic               : activate log related to the topic

bbt version "0.0.6"
http://lionel.draghi.free.fr/bbt/

```
