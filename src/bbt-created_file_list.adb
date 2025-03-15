-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO;
with BBT.Settings;

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Text_IO;

use BBT.IO,
    Ada,
    Ada.Directories,
    Ada.Text_IO;

package body BBT.Created_File_List  is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Created_Files)
                             renames BBT.IO.Put_Line;
   pragma Warnings (Off, Put_Debug_Line);

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   File_Set : String_Sets.Set;

   -- --------------------------------------------------------------------------
   procedure Register (File_Name : String) is
      use String_Sets;
   begin
      if File_Set.Find (File_Name) = No_Element then
         File_Set.Insert (File_Name);
      end if;
   end Register;

   -- --------------------------------------------------------------------------
   procedure Add_Path (Path : String) is
   begin
      if BBT.Settings.Cleanup then
         Put_Debug_Line ("== Add_Path (" & Path & ");");
         if Exists (Path) then
            -- The file exists, let's exit
            Put_Debug_Line ("   Path " & Path & " exists, exiting... ");
         else
            Put_Debug_Line ("   " & Path & " doesnt exists, adding it");
            Register (Path);
            declare
               Parent : constant String :=
                          Directories.Containing_Directory (Path);
            begin
               Add_Path (Parent);
            end;
         end if;
      end if;
   end Add_Path;

   -- --------------------------------------------------------------------------
   procedure Add (Name : String) is
   begin
      if BBT.Settings.Cleanup then
         Put_Debug_Line ("== Add (" & Name & ");");
         Register (Name);
         Add_Path (Directories.Containing_Directory (Name));
      end if;
   end Add;

   -- --------------------------------------------------------------------------
   procedure Put is
   begin
      if BBT.Settings.Cleanup then
         for F of File_Set loop
            Text_IO.Put_Line (F);
         end loop;
      end if;
   end Put;

   -- --------------------------------------------------------------------------
   procedure Delete_All is
   begin
      if Settings.Cleanup then
         Put_Debug_Line ("+++ Delete_All ");

         for F of File_Set loop
            -- We could delete in the opposite order of creation, so that
            -- dir1/f1 is deleted before dir1.
            -- But if bbt did create dir1, then what's in is supposed to be
            -- cleanup also. So there is no "reverse" in the loop, and we
            -- use Delete_Tree and not Delete_Directory.
            begin
               if Exists (F) then
                  if Kind (F) = Ordinary_File then
                     Put_Debug_Line ("Cleanup, deleting file """ & F & """");
                     Delete_File (F);
                  elsif Kind (F) = Directory then
                     Put_Debug_Line
                       ("Cleanup, deleting tree rooted at """ & F & """");
                     Delete_Tree (F);
                  end if;
               end if;
            exception
               when others =>
                  IO.Put_Exception ("Unable to cleanup " & F'Image);
                  raise;
            end;
         end loop;

         File_Set.Clear;
      end if;
   end Delete_All;

end BBT.Created_File_List;
