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
         -- Put_Line ("   -sb  | --status_bar       : enable a progress bar in the terminal");
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

      when On_All =>
         -- First the base, then topics
         Put_Help (Base);
         for Topic in Help_Topic when Topic not in On_All | Base loop
            -- Recursive call on all but this topic
            Put_Help (Topic);
         end loop;

   end case;

end Put_Help;
