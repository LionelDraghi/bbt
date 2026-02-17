
Other commands:
  lf | list_files      : list Scenario files found
  lk | list_keywords   : list Step keywords
  lg | list_grammar    : list rules for Step analysis
  ex | explain         : explain what bbt understands from Scenarios files
                         (do not run the scenarios)

Other options:
        --strict         : warn when not strictly following Gherkin common guidelines
        --index file.md  : create an md file with test results
                           that indexes all scenarios run.
                           This file will contain the normal bbt output,
                           whatever are the verbosity settings (-q, -v, etc.)
                           for standard output.
  -ed | --exec_dir 'dir' : run command in dir instead of current dir
  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir
  -j  | --junit file.xml : generate a JUnit XML report file
  -gb | --generate_badge badge.url : create a text file containing
                           a shields.io URL to get a svg badge
                           with tests results summary.

