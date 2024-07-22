-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

package BBT.Scenarios.Files is
-- Provide services related to the Scenario files list, that is mainly to :
-- - identify (possibly recursively) Scenario files in a directory;
-- - manually add files to the list.

   -- --------------------------------------------------------------------------
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String)    return File_Name;
   No_File : constant File_Name
     := File_Name (Ada.Strings.Unbounded.Null_Unbounded_String);

   -- --------------------------------------------------------------------------
   package File_List is new Ada.Containers.Indefinite_Vectors (Positive,
                                                               String);
   procedure Append_File (File_Name : String);

   -- --------------------------------------------------------------------------
   procedure Find_BBT_Files
     (Recursive   : Boolean;
      Start_In    : String := "./";
      Remove_Root : String := Settings.Initial_Directory);
   function No_BBT_File return Boolean;
   function BBT_Files return File_List.Vector;

   -- --------------------------------------------------------------------------
   procedure Analyze_MDG_File (File_Name : String);

end BBT.Scenarios.Files;
