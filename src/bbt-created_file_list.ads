-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package BBT.Created_File_List is
-- This package manage the list of files created by bbt when executing
-- the scenarios in a document (that is, a file), and offer a service to remove
-- them all at once, if the cleanup has been activated

   -- --------------------------------------------------------------------------
   procedure Add (Name : String);
   -- Add a file name in the list
   -- Add non existing dir in the Path, meaning that if dir1 exists
   -- and dir2 doesn't, and Name = "dir1/dir2/file1", only "dir2" and "file1"
   -- will be added.

   procedure Put;
   -- Just list file currently recorded for deletion

   procedure Delete_All;
   -- Delete all files in the list, and also the file containing the list,
   -- if the cleanup has been activated on the command line;
   -- otherwise, Delete_All has no effect.

   procedure Take_Snapshot;
   -- Take a recursive snapshot of the current dir (the exec dir),
   -- storing the full name of every file and dir, whatever their
   -- creation date: this snapshot is the reference state for
   -- Delete_New_Entries.

   procedure Delete_New_Entries
     (Confirm : not null access function (Prompt : String) return Boolean);
   -- Remove from the current dir every file and dir that is not in
   -- the snapshot, whatever created it. Contrary to Delete_All, this
   -- removes entries that bbt did not create itself: the Confirm
   -- function is called once before removing, and may refuse.
   -- Symbolic links are ignored: never traversed, never removed.

end BBT.Created_File_List;
