-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;

with Ada.Strings.Unbounded;

package body TodoMVC_Store is

   use Ada.Text_IO;
   use TodoMVC_Model;

   function Filter_Label (Kind : Filter_Kind) return String is
     (case Kind is
        when All_Filter       => "All",
        when Active_Filter    => "Active",
        when Completed_Filter => "Completed");

   procedure Load (List : in out Todo_List) is
      File : File_Type;
      Line : String (1 .. 1024);
      Len  : Natural;
   begin
      if not Ada.Directories.Exists (State_File_Name) then
         return;
      end if;

      Open (File, In_File, State_File_Name);
      while not End_Of_File (File) loop
         Get_Line (File, Line, Len);
         if Len > 2 then
            if Line (1 .. 2) = "F " then
               Set_Filter
                 (List,
                  (if    Line (3 .. Len) = "Active"    then Active_Filter
                   elsif Line (3 .. Len) = "Completed" then Completed_Filter
                   else All_Filter));
            elsif Line (1 .. 2) = "U " or else Line (1 .. 2) = "D " then
               Add (List, Line (3 .. Len));
               Set_Done (List, Length (List), Line (1) = 'D');
            end if;
         end if;
      end loop;
      Close (File);
   exception
      when Name_Error | Use_Error =>
         --  unreadable state file: a fresh app
         if Is_Open (File) then
            Close (File);
         end if;
   end Load;

   procedure Save (List : Todo_List) is
      File : File_Type;
   begin
      if Length (List) = 0 then
         if Ada.Directories.Exists (State_File_Name) then
            Ada.Directories.Delete_File (State_File_Name);
         end if;
         return;
      end if;

      Create (File, Out_File, State_File_Name);
      Put_Line (File, "F " & Filter_Label (Filter (List)));
      for I in 1 .. Length (List) loop
         declare
            Item : constant Todo := Todo_At (List, I);
         begin
            Put_Line
              (File,
               (if Item.Done then "D " else "U ")
               & Ada.Strings.Unbounded.To_String (Item.Title));
         end;
      end loop;
      Close (File);
   end Save;

end TodoMVC_Store;
