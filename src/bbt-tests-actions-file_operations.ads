-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded;
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
   procedure Create_Path (New_Directory : String;
                          Form          : String := "");
   procedure Delete_File (Name : String);
   procedure Delete_Tree (Directory : String);

   -- --------------------------------------------------------------------------
   subtype File_Type is Ada.Text_IO.File_Type;
   subtype File_Mode is Ada.Text_IO.File_Mode;
   Out_File : File_Mode renames Ada.Text_IO.Out_File;

   -- --------------------------------------------------------------------------
   procedure Create (File : in out File_Type;
                     Mode : File_Mode := Out_File;
                     Name : String := "";
                     Form : String := "");
   procedure Close  (File : in out File_Type);

   -- --------------------------------------------------------------------------
   function Create_File (File_Name    : Ada.Strings.Unbounded.Unbounded_String;
                         With_Content : Text) return Boolean;

end BBT.Tests.Actions.File_Operations;
