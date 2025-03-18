-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Main)

-- -----------------------------------------------------------------------------
procedure Put_Help is
begin
   New_Line;
   Put_Line ("Usage : bbt [Options]* [Command] [bbt_file]*");
   New_Line;
   Put_Line ("If no file name provided, read *.md (recursively if ""-r"")");
   New_Line;
   Put_Line ("Commands:");
   Put_Line ("   run                   : the default command");
   Put_Line ("   list                  : list selected items");
   Put_Line ("   lf  | list_files      : list Scenario files found");
   Put_Line ("   lk  | list_keywords   : list Step keywords");
   Put_Line ("   lg  | list_grammar    : list rules for Step analysis");
   Put_Line ("   ex  | explain         : explain what bbt understands from Scenarios files");
   Put_Line ("                              (do not run the scenarios)");
   Put_Line ("   ct  | create_template : create a commented example of rules file");
   Put_Line ("   he  | help            : this message");
   New_Line;
   Put_Line ("Options:");
   Put_Line ("          --yes            : do not prompt if deletion is needed in");
   Put_Line ("                             ""Given"" steps, silently answer yes");
   Put_Line ("          --strict         : warn when not strictly following Gherkin common guidelines");
   Put_Line ("   -c   | --cleanup        : after run, remove every file and dir");
   Put_Line ("                             created by bbt in ""Given"" steps");
   Put_Line ("   -r   | --recursive      : search bbt files in subdir");
   Put_Line ("   -k   | --keep_going     : Do as much work as possible");
   Put_Line ("   -v   | --verbose");
   Put_Line ("   -q   | --quiet          : no message unless error,");
   Put_Line ("                             Warnings are also ignored");
   Put_Line ("   -o   | --output file.md : create an md file with test results");
   Put_Line ("                             that indexes all scenarios run.");
   Put_Line ("                             This file will contain the normal bbt output,");
   Put_Line ("                             whatever are the verbosity settings (-q, -v, etc.)");
   Put_Line ("                             for standard output.");
   -- Put_Line ("   -ot  | --output_tag 'tag' : include a specific tag in the results file");
   Put_Line ("   -ed  | --exec_dir 'dir' : run command in dir instead of current dir");
   Put_Line ("   -td  | --tmp_dir 'dir'  : create .out file in dir instead of current dir");
   -- Put_Line ("   -sb  | --status_bar       : enable a progress bar in the terminal");
   Put_Line ("   -gb  | --generate_badge badge.url : create a text file containing");
   Put_Line ("                                       a shields.io URL to get a svg badge");
   Put_Line ("                                       with tests results summary.");
   New_Line;
   Put_Line ("Human vs exact matching:");
   Put_Line ("  bbt default behavior is ""human match"", that is ignoring differences");
   Put_Line ("  in casing, ignoring consecutive spaces, and ignoring blank lines.");
   Put_Line ("  The opposite behavior, to make strict compare, is set with:");
   Put_Line ("   -em  | --exact_match");
   Put_Line ("  exact_match may be altered if **followed** by one or more of:");
   Put_Line ("   -iw  | --ignore_whitespaces (default)");
   Put_Line ("   -ic  | --ignore_casing      (default)");
   Put_Line ("   -ibl | --ignore_blank_lines (default)");
   Put_Line ("  For example, ""-em -iw"" will take into account blank lines and");
   Put_Line ("  casing but ignore whitespaces");
   Put_Line ("  Note that -iw, -ic, and -ibl are useless if not preceded by -em, ");
   Put_Line ("  because they are the default setting.");
   Put_Line ("  There is also a");
   Put_Line ("   -hm  | --human_match");
   Put_Line ("  option, equivalent to defaults ""-iw -ic -ibl"", if you want to");
   Put_Line ("  assert on the command line that this is the required behavior.");
   New_Line;
   Put_Line ("Debug command:");
   Put_Line ("   -lt                    : list log topics");
   Put_Line ("   -ls                    : list settings");
   Put_Line ("Debug options:");
   Put_Line ("   -t topic               : activate log related to the topic");
   New_Line;
   Put_Line ("bbt version " & Settings.BBT_Version);
   Put_Line ("https://github.com/LionelDraghi/bbt/");
   New_Line;
end Put_Help;
