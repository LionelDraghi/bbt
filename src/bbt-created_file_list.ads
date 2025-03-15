-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Created_File_List is
-- This package manage the list of files created by bbt when executing
-- the scenarios in a document (that is, a file).
-- This is done by calling Delete_All after running the document.
--
-- When using the -c | --cleanup option, each file and dir in the list is
-- removed by calling Delete_All.
--
-- When NOT Using The -c | --cleanup option, this package has not effect.

   -- --------------------------------------------------------------------------
   procedure Add (Name : String);
   -- Add a file name in the list
   -- Add non existing dir in the Path, meaning that if dir1 exists
   -- and dir2 doesn't, and Name = "dir1/dir2/file1", only "dir2" and "file1"
   -- will be added.

   procedure Put;
   -- Just list file currently recorded for deletion

   procedure Delete_All;
   -- Delete all files in the list, and also the file containing the list

end BBT.Created_File_List;
