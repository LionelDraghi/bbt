-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Main)

-- -----------------------------------------------------------------------------
procedure Put_Help is
begin
   New_Line;
   Put_Line ("Usage : bbt [Options]* [Command] [bbt_file]*");
   New_Line;
   Put_Line ("If no file name provided, read *.bbt (recursively if ""-r"")");
   New_Line;
   Put_Line ("Command :");
   Put_Line ("   run                    : the default command");
   Put_Line ("   -lf  | --list_files      : list Scenario files found");
   Put_Line ("   -lk  | --list_keywords   : list bbt keywords");
   Put_Line ("   -lg  | --list_grammar    : list rules for Step analysis");
   Put_Line ("   -e   | --explain         : explain what bbt understand from Scenarios files");
   Put_Line ("   -ct  | --create-template : create a commented example of rules file");
   Put_Line ("   -d   | --dry-run         : print the commands that would be");
   Put_Line ("   -h   | --help            : this message");
   New_Line;
   Put_Line ("Options :");
   Put_Line ("   -r   | --recursive      : search bbt files in subdir");
   Put_Line ("   -k   | --keep-going     : Do as much work as possible");
   Put_Line ("   -v   | --verbose");
   Put_Line ("   -q   | --quiet          : no message unless error,");
   Put_Line ("                             Warning are also ignored");
   Put_Line ("   -o   | --output file.md : create a Markdown file with tests results");
   Put_Line ("                             This file will contains the normal bbt output,");
   Put_Line ("                             whatever are the verbosity settings (-q, -v, etc.)");
   Put_Line ("                             for standard output.");
   -- Put_Line ("   -p   | --prompt         : prompt before erasing file / directory");
   Put_Line ("   -y   | --assume_yes     : no prompt if deletion is needed, yes is assumed");
   New_Line;
   Put_Line ("Debug command:");
   Put_Line ("   -lt                    : list log topics");
   Put_Line ("   -ls                    : list settings");
   Put_Line ("Debug options:");
   Put_Line ("   -t topic               : activate log related to the topic");
   New_Line;
   Put_Line ("http://lionel.draghi.free.fr/bbt/");
   New_Line;
end Put_Help;
