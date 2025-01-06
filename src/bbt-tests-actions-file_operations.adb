-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with Text_Utilities;

package body BBT.Tests.Actions.File_Operations is

   -- --------------------------------------------------------------------------
   function Confirm_Delete (Prompt : String) return Boolean is
      C : Character;
   begin
      if Settings.Yes then return True;
      end if;

      loop
         Ada.Text_IO.Put (Prompt);
         Ada.Text_IO.Put_Line ("   [Y]es/[N]o/[A]ll");
         Ada.Text_IO.Get_Immediate (C);
         case C is
            when 'N' | 'n' => return False;
            when 'Y' | 'y' => return True;
            when 'A' | 'a' =>
               Settings.Yes := True;
               return True;
            when others => null;
         end case;
      end loop;
   end Confirm_Delete;

   -- --------------------------------------------------------------------------
   function Exists (Name : String) return Boolean is
   begin
      return Ada.Directories.Exists (Name);
   exception
      when Ada.Directories.Name_Error =>
         Put_Error ("Illegal file name : """ & Name & """");
         raise;
         -- return False;
   end Exists;

   function Kind (Name : String) return File_Kind
                  renames Ada.Directories.Kind;
   procedure Create_Path (New_Directory : String;
                          Form          : String := "")
                          renames Ada.Directories.Create_Path;

   -- --------------------------------------------------------------------------
   procedure Delete_File (Name : String) is
   begin
      if Exists (Name)
        and then Confirm_Delete ("Delete file " & Name & "?")
      then
         Ada.Directories.Delete_File (Name);
      end if;
   end Delete_File;

   -- --------------------------------------------------------------------------
   procedure Delete_Tree (Dir_Name : String)  is
   begin
      if Exists (Dir_Name)
        and then Confirm_Delete ("Delete tree " & Dir_Name & "?")
      then
         Ada.Directories.Delete_Tree (Dir_Name);
      end if;
   end Delete_Tree;

   -- --------------------------------------------------------------------------
   procedure Create (File : in out File_Type;
                     Mode : File_Mode := Out_File;
                     Name : String := "";
                     Form : String := "") renames Ada.Text_IO.Create;
   procedure Close  (File : in out File_Type) renames Ada.Text_IO.Close;

   function Create_File (File_Name    : Ada.Strings.Unbounded.Unbounded_String;
                         With_Content : Text) return Boolean
                         renames Text_Utilities.Create_File;

end BBT.Tests.Actions.File_Operations;
