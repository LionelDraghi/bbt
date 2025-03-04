-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with Ada.Containers.Indefinite_Vectors;

package BBT.Scenarios.Files is
-- Provide services related to the Scenario files list, that is mainly to :
-- - identify (possibly recursively) Scenario files in a directory;
-- - manually add files to the list.

   -- --------------------------------------------------------------------------
   package File_List is new Ada.Containers.Indefinite_Vectors (Positive,
                                                               String);
   procedure Append_File (File_Name : String);

   -- --------------------------------------------------------------------------
   function No_BBT_File return Boolean;
   function BBT_Files return File_List.Vector;
   function One_Line_Image (Files : File_List.Vector) return String;
   -- returns something like "[name1, name2, name3]"

   -- --------------------------------------------------------------------------
   procedure Get_Document_List
     (Start_In    : String;
      Recursive   : Boolean;
      Remove_Root : String := BBT.Settings.Launch_Directory);
   -- Returns all sources in Start_In and surbdirs that is recognized
   -- by one of the Formater.

   -- --------------------------------------------------------------------------
   procedure Analyze_MDG_File (File_Name : String);

end BBT.Scenarios.Files;
