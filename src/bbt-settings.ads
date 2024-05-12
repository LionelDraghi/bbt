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
   function Initial_Directory return String;
   -- returns Ada.Directories.Current_Directory at bbt launch.
   -- function Run_Dir_Name return String;

   -- --------------------------------------------------------------------------
   procedure Set_Output_File (File_Name : String);
   function Get_Output_File_Name return String;

end BBT.Settings;
