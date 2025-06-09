-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Cmd_Line)

-- -----------------------------------------------------------------------------
procedure Put_Help (Topic : Settings.Help_Topic) is
begin
   case Topic is
      when Base =>
         New_Line;
         Put_Line ("Usage : bbt [Options]* [Command] file*");
         New_Line;
         Put_Line ("  The default command is 'run'");
         Put_Line ("  If no file is provided, reads *.md files");
         New_Line;
         Put_Line ("Basic options:");
         Put_Line ("       --yes        : do not prompt if deletion is needed in");
         Put_Line ("                      ""Given"" steps, silently answer yes");
         Put_Line ("  -c | --cleanup    : after run, remove every file and dir");
         Put_Line ("                      created by bbt in ""Given"" steps");
         Put_Line ("  -r | --recursive  : search scenarios in subdirs");
         Put_Line ("  -k | --keep_going : do as much work as possible");
         Put_Line ("       --Werror     : treat warnings as errors");
         Put_Line ("  -v | --verbose");
         Put_Line ("  -q | --quiet      : no message unless error,");
         Put_Line ("                      Warnings are also ignored");
         New_Line;
         Put_Line ("Basic commands:");
         Put_Line ("       run               : the default command");
         Put_Line ("  ls | list              : list selected items");
         Put_Line ("  ct | create_template   : create a commented example of rules file");
         Put_Line ("  he | help [topic]      : base help, or more on one of the topic listed below");
         Put_Line ("  he | help on_all       : full online help");
         Put_Line ("  he | help tutorial     : create a tutorial in " & Tutorial_Name);
         Put_Line ("  he | help example      : create a scenario example in " & Example_Name);
         New_Line;
         Put_Line ("Help topics:");
         Put_Line ("  filtering : --select --exclude --include");
         Put_Line ("  matching  : --exact_match --ignore_whitespaces --ignore_casing --ignore_blank_lines");
         Put_Line ("  other     : list_files list_keywords list_grammar explain create_template");
         Put_Line ("              --strict --output file.md --exec_dir --tmp_dir --generate_badge");
         Put_Line ("  debug     : -d tt -ls -t");
         New_Line;
         Put_Line ("bbt version " & Settings.BBT_Version);
         Put_Line ("https://github.com/LionelDraghi/bbt/");

      when Filtering =>
         New_Line;
         Put_Line ("Filtering:");
         Put_Line ("  Features, Scenarios and Steps may be selected or filtered.");
         Put_Line ("  By default, every item is selected.");
         Put_Line ("  -s | --select 'string'  : only items containing 'string' are selected");
         Put_Line ("  -e | --exclude 'string' : remove from selection items containing 'string'");
         Put_Line ("  -i | --include 'string' : include in selection items containing 'string'");
         Put_Line ("  Multiple occurrences are processed in order, meaning that you can exclude");
         Put_Line ("  a whole Feature and then re-include a Scenario belonging to this feature.");

      when Matching =>
         New_Line;
         Put_Line ("Human vs exact matching:");
         Put_Line ("  bbt default behavior is ""human match"", that is ignoring differences");
         Put_Line ("  in casing, ignoring consecutive spaces, and ignoring blank lines.");
         Put_Line ("  The opposite behavior, to make strict compare, is set with:");
         Put_Line ("  -em  | --exact_match");
         Put_Line ("  exact_match may be altered if **followed** by one or more of:");
         Put_Line ("  -iw  | --ignore_whitespaces (default)");
         Put_Line ("  -ic  | --ignore_casing      (default)");
         Put_Line ("  -ibl | --ignore_blank_lines (default)");
         Put_Line ("  For example, ""-em -iw"" will take into account blank lines and");
         Put_Line ("  casing but ignore whitespaces");
         Put_Line ("  Note that -iw, -ic, and -ibl are useless if not preceded by -em, ");
         Put_Line ("  because they are the default setting.");
         Put_Line ("  There is also a");
         Put_Line ("  -hm  | --human_match");
         Put_Line ("  option, equivalent to defaults ""-iw -ic -ibl"", if you want to");
         Put_Line ("  assert on the command line that this is the required behavior.");

      when Other =>
         New_Line;
         Put_Line ("Other commands:");
         Put_Line ("  lf | list_files      : list Scenario files found");
         Put_Line ("  lk | list_keywords   : list Step keywords");
         Put_Line ("  lg | list_grammar    : list rules for Step analysis");
         Put_Line ("  ex | explain         : explain what bbt understands from Scenarios files");
         Put_Line ("                         (do not run the scenarios)");
         New_Line;
         Put_Line ("Other options:");
         Put_Line ("        --strict         : warn when not strictly following Gherkin common guidelines");
         Put_Line ("  -o  | --output file.md : create an md file with test results");
         Put_Line ("                           that indexes all scenarios run.");
         Put_Line ("                           This file will contain the normal bbt output,");
         Put_Line ("                           whatever are the verbosity settings (-q, -v, etc.)");
         Put_Line ("                           for standard output.");
         -- Put_Line ("   -ot  | --output_tag 'tag' : include a specific tag in the results file");
         Put_Line ("  -ed | --exec_dir 'dir' : run command in dir instead of current dir");
         Put_Line ("  -td | --tmp_dir 'dir'  : create .out file in dir instead of current dir");
         Put_Line ("  -sb | --status_bar     : enable a progress bar in the terminal (WIP!!)");
         Put_Line ("  -gb | --generate_badge badge.url : create a text file containing");
         Put_Line ("                           a shields.io URL to get a svg badge");
         Put_Line ("                           with tests results summary.");

      when Debug =>
         New_Line;
         Put_Line ("Debug command:");
         Put_Line ("   tt             : list trace topics");
         Put_Line ("Debug options:");
         Put_Line ("   -ls            : list settings");
         Put_Line ("   -d             : very very verbose output");
         Put_Line ("   -d trace_topic : activate debug traces for the topic");
         New_Line;
         Put_Line ("Friends are here : https://github.com/LionelDraghi/bbt/Issues");
         Put_Line ("Good luck :-)");

      when Tutorial =>
         Put_Line ("");
         Put_Line ("## Introduction  ");
         Put_Line ("");
         Put_Line ("This is a template bbt file, generated with BBT 0.2.0-dev  ");
         Put_Line ("(It's also the shortest possible bbt tutorial!)");
         Put_Line ("");
         Put_Line ("A bbt file contains at least :  ");
         Put_Line ("1. a Scenario header, that is a line starting with ""# Scenario : ""  ");
         Put_Line ("2. some Steps, that is lines starting with ""- Given"", ""- when"" or ""- then""  ");
         Put_Line ("");
         Put_Line ("Minimal Example :  ");
         Put_Line ("");
         Put_Line ("    ## Scenario : getting gcc version  ");
         Put_Line ("    - When I run `gcc --version`  ");
         Put_Line ("    - Then I get `14.2.0`  ");
         Put_Line ("");
         Put_Line ("## Structure of the file  ");
         Put_Line ("");
         Put_Line ("Several scenarios may be organized in ""Feature"".  ");
         Put_Line ("Note that the only headers reserved for bbt uses are ""Feature"", ""Scenario"" or ""Example"", and ""Background""  ");
         Put_Line ("Other header will be ignored by bbt.  ");
         Put_Line ("");
         Put_Line ("## Steps  ");
         Put_Line ("");
         Put_Line ("Steps are the most important part of bbt files.  ");
         Put_Line ("- ""Given"" steps put the system in a known state  ");
         Put_Line ("- ""when""  steps run defined actions  ");
         Put_Line ("- ""then""  steps observes outcomes  ");
         Put_Line ("");
         Put_Line ("Examples of steps:  ");
         Put_Line ("");
         Put_Line ("    - Given there is no `.config` dir");
         Put_Line ("    - Given the `config.ini` file");
         Put_Line ("      ```");
         Put_Line ("      verbose=false");
         Put_Line ("      lang=am");
         Put_Line ("      ```");
         Put_Line ("    - Given the executable file `command.sh`");
         Put_Line ("      ```");
         Put_Line ("      #!/bin/bash");
         Put_Line ("      echo ""Bbt Rules!""");
         Put_Line ("      ```");
         Put_Line ("    - When I successfully run `xxx`");
         Put_Line ("      (Equivalent to both lines ""- when I Run `Xxx`"" and ""- then I Get No Error"")");
         Put_Line ("    - Then there is no output");
         Put_Line ("    - Then I get no error");
         Put_Line ("    - Then I get an error");
         Put_Line ("    - Then output is `sut v0.1.0` (Equivalent ""then I Get .. ."")");
         Put_Line ("");
         Put_Line ("You can continue a list of Given / When / Then with ""and"" or ""But"":  ");
         Put_Line ("");
         Put_Line ("    - Then output contains `234 processed data`");
         Put_Line ("    - And  output contains `result = 29580`");
         Put_Line ("    - But  output doesn't contain `Warning:`");
         Put_Line ("    - And  output does not contain `Error:`  ");
         Put_Line ("");
         Put_Line ("## Expected output  ");
         Put_Line ("");
         Put_Line ("Expected output is given in three possible ways :  ");
         Put_Line ("  1. as a string:");
         Put_Line ("");
         Put_Line ("    - Then I get `string`");
         Put_Line ("");
         Put_Line ("  2. as a code fenced block:");
         Put_Line ("");
         Put_Line ("    - Then I get");
         Put_Line ("    ```");
         Put_Line ("    This is my multi-line");
         Put_Line ("    file content");
         Put_Line ("    ```");
         Put_Line ("");
         Put_Line ("  3. in an external file:");
         Put_Line ("");
         Put_Line ("    - Then I get file `expected.txt`  ");
         Put_Line ("");
         Put_Line ("     Note in that case the mandatory ""File"" keyword  ");
         Put_Line ("");
         Put_Line ("Above forms test that the output is exactly what is given.  ");
         Put_Line ("If what you want is just test that the output contains something, then use the ""Contains"" keyword:  ");
         Put_Line ("");
         Put_Line ("    - Then output contains `sut version v0.1.0`  ");
         Put_Line ("");
         Put_Line ("If what you want is search for some pattern, then use the ""Matches"" keyword, followed by a regexp :  ");
         Put_Line ("");
         Put_Line ("    - Then output **matches** `sut version v[0-9]+\.[0-9]+\.[0-9]+`  ");
         Put_Line ("");
         Put_Line ("Note that the regexp must match the entire line,");
         Put_Line ("don't forget to put "". * "" at the beginning or at the end if necessary.  ");
         Put_Line ("");
         Put_Line ("## Background  ");
         Put_Line ("");
         Put_Line ("Preconditions common to several scenarios may be put in a Background section, before scenarios :  ");
         Put_Line ("");
         Put_Line ("    ### Background:  ");
         Put_Line ("    - Given there is no `input.txt` file  ");
         Put_Line ("    - Given there is a `tmp` dir  ");
         Put_Line ("");
         Put_Line ("## Help  ");
         Put_Line ("");
         Put_Line ("To get a complete (although less friendly) view on the grammar:  ");
         Put_Line ("");
         Put_Line ("    bbt list_grammar  ");
         Put_Line ("");
         Put_Line ("To check your scenario with a dry run:  ");
         Put_Line ("");
         Put_Line ("    bbt explain scenario.md  ");
         Put_Line ("");
         Put_Line ("More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-");
         Put_Line ("");

      when Example =>
         Put_Line ("# Feature : grep matching may be case insensitive");
         Put_Line ("");
         Put_Line ("## Background: ");
         Put_Line ("- Given the new file `flowers.lst`");
         Put_Line ("~~~");
         Put_Line ("Rose");
         Put_Line ("cactus");
         Put_Line ("rose");
         Put_Line ("Tulip");
         Put_Line ("~~~");
         Put_Line ("");
         Put_Line ("## Scenario : default case sensitive matching  ");
         Put_Line ("- When I run `grep rose flowers.lst`");
         Put_Line ("- Then I get ");
         Put_Line ("  ~~~");
         Put_Line ("  rose");
         Put_Line ("  ~~~");
         Put_Line ("");
         Put_Line ("## Scenario : case insensitive matching  ");
         Put_Line ("- When I run `grep -i rose flowers.lst`");
         Put_Line ("- Then I get ");
         Put_Line ("  ~~~");
         Put_Line ("  Rose");
         Put_Line ("  rose");
         Put_Line ("  ~~~");

      when On_All =>
         -- First the base, then topics
         Put_Help (Base);
         for Topic in Help_Topic when Topic not in On_All | Base loop
            -- Recursive call on all but this topic
            Put_Help (Topic);
         end loop;

   end case;

end Put_Help;
