-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Bbt_Config;

private package BBT.Settings is
-- Purpose:
--   This package manages global settings, hard coded or from cmd line.

   -- --------------------------------------------------------------------------
   BBT_Version : constant String := Bbt_Config.Crate_Version;

   type Command is (Run,
                    List,
                    Explain,
                    List_Trace_Topics,
                    List_Keywords,
                    List_Grammar,
                    List_Files,
                    Create_Template,
                    Help,
                    Version,
                    None);

   type Help_Topic is (Base,
                       Filtering,
                       Matching,
                       Other,
                       Debug,
                       Tutorial,
                       Example,
                       On_All);
   -- Topics are given in importance order, because this order is used when
   -- printing full help with command 'help on_all'

   -- --------------------------------------------------------------------------
   -- Explain            : Boolean := False;
   Keep_Going         : Boolean    := False; -- if set, do not exit on assertion failure
   Ignore_Errors      : Boolean    := False; -- if set, return no error code when a test fails
   Warnings_As_Errors : Boolean    := False;
   No_File_Given      : Boolean    := True;
   Recursive          : Boolean    := False;
   List_Settings      : Boolean    := False;
   Yes                : Boolean    := False;
   Cleanup            : Boolean    := False;
   Strict_Gherkin     : Boolean    := False;
   Status_Bar         : Boolean    := False;
   Generate_Badge     : Boolean    := False;
   Ignore_Whitespaces : Boolean    := True;
   Ignore_Casing      : Boolean    := True;
   Ignore_Blank_Lines : Boolean    := True;
   Current_Command    : Command    := None;
   Current_Topic      : Help_Topic := Base;
   Selection_Mode     : Boolean    := False;
   -- In the default mode no File/Feature/Scenario etc. is filtered except those
   -- provided with --exclude on command line.
   -- But if the user wants to select specific items by using --select, the
   -- Selection_Mode will be set, and all items will be filtered
   -- except those provided with --select or --include.

   Template_Name : constant String := "bbt_template.md";
   Tutorial_Name : constant String := "bbt_tutorial.md";
   Example_Name  : constant String := "bbt_example.md";

   -- --------------------------------------------------------------------------
   function Stop_On_Error return Boolean is (not Keep_Going);

   -- --------------------------------------------------------------------------
   function Launch_Directory return String;
   -- Returns Ada.Directories.Current_Directory at bbt launch.

   -- --------------------------------------------------------------------------
   procedure Set_Exec_Dir (Dir_Name : String);
   function Exec_Dir return String;
   -- Dir where the tests will be run, the temp file created, etc.

   procedure Set_Tmp_Dir (Dir_Name : String);
   function Tmp_Dir return String;
   -- Dir where bbt created tmp files are stored.

   -- --------------------------------------------------------------------------
   function Output_File_Dir return String;
   -- Each spawned command outputs will be output here

   -- --------------------------------------------------------------------------
   -- Operation related to the index file of all run scenarios
   procedure Set_Index_File (File_Name : String);
   function Index_File_Name return String;
   function Index_Dir       return String;

   -- --------------------------------------------------------------------------
   procedure Set_Badge_File_Name (File_Name : String);
   function Badge_File_Name return String;

end BBT.Settings;
