-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (http://lionel.draghi.free.fr/bbt/)
-- © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- http://www.apache.org/licenses/LICENSE-2.0
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
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
   Put_Line ("   -lf  | list_files      : list scenarii files found");
   Put_Line ("   -lk  | list_keywords   : list bbt keywords");
   Put_Line ("   -e   | explain         : explain what bbt understand from scenarii files");
   Put_Line ("   -ct  | create-template : create a commented example of rules file");
   Put_Line ("   -d   | dry-run         : print the commands that would be");
   Put_Line ("   -h   | help            : this message");
   New_Line;
   Put_Line ("Options :");
   Put_Line ("   -r   | --recursive     : search bbt files in subdir");
   Put_Line ("   -k   | --keep-going    : Do as much work as possible");
   Put_Line ("   -v   | --verbose");
   Put_Line ("   -q   | --quiet         : no message unless error,");
   Put_Line ("                            Warning are also ignored");
   --  Put_Line ("   -wc  | --with-comments : when used with -e, list the full contents of");
   --  Put_Line ("                            scenarii files");
   --  Put_Line ("   -bk  | --bold-keywords : when used with -e, keywords are surrouded by ""**""");
   --  Put_Line ("                            for a better markdown rendering");
   Put_Line ("   -o   | --output file.md : create a Markdown file with tests results");
   Put_Line ("                             This file will contains the normal bbt output,");
   Put_Line ("                             whatever are the verbosity settings (-q, -v, etc.)");
   Put_Line ("                             for standard output.");
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
