
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
  he | help tutorial     : print a tutorial 
  he | help example      : print an example scenario file

Help topics:
  filtering : --select --exclude --include
  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines
  other     : list_files list_keywords list_grammar explain create_template
              --strict --output file.md --exec_dir --tmp_dir --generate_badge
  debug     : -d tt -ls -t
