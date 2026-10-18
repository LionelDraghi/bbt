-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
-- with Ada.Strings.Unbounded;
with Ada.Text_IO;

package BBT.Tests.Actions.File_Operations is

   -- --------------------------------------------------------------------------
   subtype File_Kind is Ada.Directories.File_Kind;
   Directory     : File_Kind renames Ada.Directories.Directory;
   Ordinary_File : File_Kind renames Ada.Directories.Ordinary_File;
   Special_File  : File_Kind renames Ada.Directories.Special_File;

   -- --------------------------------------------------------------------------
   function Exists (Name : String) return Boolean;
   function Kind (Name : String) return File_Kind;
   procedure Delete_File (Name : String);
   procedure Delete_Tree (Dir_Name : String);
   function Confirm (Prompt : String) return Boolean;
   --  Prompts the user for a confirmation, unless Settings.Yes is set,
   --  and refuses on a non interactive stdin. All bbt confirmations
   --  go through this function, so that --yes applies uniformly.

   function Confirm_Overwrite (File_Name : String) return Boolean;
   --  Prompts the user for the confirmation of the replacing of an
   --  existing file, unless Settings.Yes is set.

   -- --------------------------------------------------------------------------
   subtype File_Type is Ada.Text_IO.File_Type;
   subtype File_Mode is Ada.Text_IO.File_Mode;
   Out_File : File_Mode renames Ada.Text_IO.Out_File;

   -- --------------------------------------------------------------------------
   procedure Close  (File : in out File_Type);

end BBT.Tests.Actions.File_Operations;
