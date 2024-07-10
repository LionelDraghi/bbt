-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with BBT.IO;
with BBT.Settings;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

package body BBT.Created_File_List  is

   Is_Open : Boolean := False;
   File    : Ada.Text_IO.File_Type;

   -- --------------------------------------------------------------------------
   procedure Open (File_Name : String) is
   begin
      if BBT.Settings.Cleanup then
         -- Put_Line ("== Creating " & File_Name);
         Create (File, Mode => Out_File, Name => File_Name);
         Is_Open := True;
      end if;
   end Open;

   -- --------------------------------------------------------------------------
   procedure Add (Name : String) is
   begin
      if BBT.Settings.Cleanup then
         -- Put_Line ("== Adding " & Name);
         Put_Line (File, Name);
      end if;
   end Add;

   -- --------------------------------------------------------------------------
   procedure Put is
   begin
      if BBT.Settings.Cleanup then
         if Is_Open then
            Reset (File, Mode => In_File);
            while not End_Of_File (File) loop
               Put_Line (Get_Line (File));
            end loop;
         end if;
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   procedure Delete_All is
      package File_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
        (String);
      FL : File_Lists.List;
   begin
      Is_Open := False;
      if BBT.Settings.Cleanup then
         -- Put_Line ("== Delete_All ");
         -- First delete all files
         Reset (File, Mode => In_File);
         while not End_Of_File (File) loop
            FL.Append (Get_Line (File));
         end loop;

         for F of reverse FL loop
            -- We delete in the opposite order of creation, so that
            -- dir1/f1 is deleted before dir1
            begin
               if Exists (F) then
                  if Kind (F) = Ordinary_File then
                     -- Put_Line ("deleting file " & F);
                     Delete_File (F);
                  elsif Kind (F) = Directory then
                     -- Put_Line ("deleting dir " & F);
                     Delete_Directory (F);
                  end if;
               end if;
            exception
               when others =>
                  BBT.IO.Put_Exception ("Unable to erase " & F'Image);
            end;
         end loop;

         -- Put_Line ("== Deleting " & Name (File));
         Delete (File);
      end if;
   end Delete_All;

end BBT.Created_File_List;
