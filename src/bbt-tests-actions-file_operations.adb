with Text_Utilities;
-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

package body BBT.Tests.Actions.File_Operations is

   function Exists (Name : String) return Boolean
                    renames Ada.Directories.Exists;
   function Kind (Name : String) return File_Kind
                  renames Ada.Directories.Kind;
   procedure Create_Path (New_Directory : String;
                          Form          : String := "")
                          renames Ada.Directories.Create_Path;
   procedure Delete_File (Name : String) renames Ada.Directories.Delete_File;
   procedure Delete_Tree (Directory : String)
                          renames Ada.Directories.Delete_Tree;

   procedure Create (File : in out File_Type;
                     Mode : File_Mode := Out_File;
                     Name : String := "";
                     Form : String := "") renames Ada.Text_IO.Create;
   procedure Close  (File : in out File_Type) renames Ada.Text_IO.Close;

   function Create_File (File_Name    : Ada.Strings.Unbounded.Unbounded_String;
                         With_Content : Text) return Boolean
                         renames Text_Utilities.Create_File;

end BBT.Tests.Actions.File_Operations;
