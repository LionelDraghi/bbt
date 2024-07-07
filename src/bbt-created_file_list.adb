-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package body BBT.Created_File_List  is

   Is_Open : Boolean := False;
   File : Ada.Text_IO.File_Type;
   List_File_Name : Unbounded_String := Null_Unbounded_String;

   -- --------------------------------------------------------------------------
   procedure Initialize (File_Name : String) is
   begin
      List_File_Name := To_Unbounded_String (File_Name);
   end Initialize;

   -- --------------------------------------------------------------------------
   procedure Add (Name : String) is
   begin
      if not Is_Open then
         Create (File, Mode => Out_File, Name => To_String (List_File_Name));
         Is_Open := True;
      end if;
      Put_Line (File, Name);
   end Add;

   -- --------------------------------------------------------------------------
   procedure Delete_All is
   begin
      -- First delete all files
      Reset (File, Mode => In_File);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Exists (Line) and then Kind (Line) = Ordinary_File then
               -- Put_Line ("deleting " & Line);
               Delete_File (Line);
            end if;
         end;
      end loop;

      -- Then delete all dir
      Reset (File);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Exists (Line) and then Kind (Line) = Directory then
               -- Put_Line ("deleting " & Line);
               Delete_Directory (Line);
            end if;
         end;
      end loop;

      -- Put_Line ("deleting " & Name (File));
      Delete (File);
   end Delete_All;

end BBT.Created_File_List;
