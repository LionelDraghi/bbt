-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Bbt_Config;

private package BBT.Settings is
-- Purpose:
--   This package manages global settings, hard coded or from cmd line.

   BBT_Version : constant String := Bbt_Config.Crate_Version;

   -- --------------------------------------------------------------------------
   Explain            : Boolean := False;
   Keep_Going         : Boolean := False; -- if set, do not exit on assertion failure
   Ignore_Errors      : Boolean := False; -- if set, return no error code when a test fails
   Warnings_As_Errors : Boolean := False;
   No_File_Given      : Boolean := True;
   Recursive          : Boolean := False;
   Help_Needed        : Boolean := False;
   List_Files         : Boolean := False;
   Create_Template    : Boolean := False;
   List_Settings      : Boolean := False;
   List_Topics        : Boolean := False;
   List_Keywords      : Boolean := False;
   List_Grammar       : Boolean := False;
   Yes                : Boolean := False;
   Cleanup            : Boolean := False;
   Strict_Gherkin     : Boolean := False;
   Template_Name      : constant String := "bbt_template.md";

   -- --------------------------------------------------------------------------
   function Launch_Directory return String;
   -- Returns Ada.Directories.Current_Directory at bbt launch.

   -- --------------------------------------------------------------------------
   procedure Set_Exec_Dir (Dir_Name : String);
   function Exec_Dir return String;
   -- Dir where the tests will be run, the temp file created, etc.

   -- --------------------------------------------------------------------------
   function Output_File_Dir return String;

   -- --------------------------------------------------------------------------
   procedure Set_Result_File (File_Name : String);
   function Result_File_Name return String;
   function Result_Dir       return String;
   -- returns the containing directory of the file_name

end BBT.Settings;
