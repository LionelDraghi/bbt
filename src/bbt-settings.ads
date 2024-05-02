-- -----------------------------------------------------------------------------
-- bbt, the smart make (http://lionel.draghi.free.fr/bbt/)
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

-- -----------------------------------------------------------------------------
-- Purpose:
--   This package manages global settings, hard coded or from cmd line.
-- -----------------------------------------------------------------------------

with Bbt_Config;

private package BBT.Settings is

   BBT_Version : constant String := Bbt_Config.Crate_Version;

   -- --------------------------------------------------------------------------
   Explain               : Boolean := False;
   Dry_Run               : Boolean := False;
   Keep_Going            : Boolean := False;
   Ignore_Errors         : Boolean := False;
   Warnings_As_Errors    : Boolean := False;
   No_File_Given         : Boolean := True;
   Recursive             : Boolean := False;
   Help_Needed           : Boolean := False;
   List_Files            : Boolean := False;
   With_Comments         : Boolean := False;
   With_Bold_Keywords    : Boolean := False;
   Create_Template       : Boolean := False;
   Template_Name         : constant String := "./bbt_template.md";
   List_Settings         : Boolean := False;
   List_Topics           : Boolean := False;
   List_Keywords         : Boolean := False;

   -- --------------------------------------------------------------------------
   -- Observability!
   type Extended_Topics is (None,
                            Spawn,
                            Lexer,
                            BBT_Files,
                            Builder,
                            Step_Lexer);
   subtype Topics is Extended_Topics range Extended_Topics'Succ (None) .. Extended_Topics'Last;
   --  None is the default parameter for IO operation, but is not in Topics
   --  range, used when setting what should be printed.
   procedure Enable_Topic (Topic : Topics);
   function Is_Enabled (Topic : Extended_Topics) return Boolean;
   -- return always false for None

   -- --------------------------------------------------------------------------
   function Initial_Directory return String;
   -- returns Ada.Directories.Current_Directory at bbt launch.
   function Run_Dir_Name return String;

   -- --------------------------------------------------------------------------
   type Print_Out_Level is (Debug, Verbose, Normal, Quiet);
   -- default: Normal messages are displayed, verbose messages are not
   --          displayed.
   -- quiet:   Neither normal messages nor verbose messages are displayed.
   --          This mode can be achieved using option --quiet.
   -- verbose: Both normal messages and verbose messages are displayed.
   --          This mode can be achieved using option --verbose.
   Verbosity : Print_Out_Level := Normal;
   function Is_Authorised (Level : Print_Out_Level) return Boolean;

   -- --------------------------------------------------------------------------
   function Debug_Mode return Boolean is (Verbosity = Debug);

end BBT.Settings;
