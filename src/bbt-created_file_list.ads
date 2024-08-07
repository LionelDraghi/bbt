-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Created_File_List is

   procedure Open (File_Name : String);
   -- Create and open the file containing created files

   procedure Add (Name : String);
   -- Add a file name in the list

   procedure Put;

   procedure Delete_All;
   -- Delete all files in the list, and also the file containing the list

end BBT.Created_File_List;
