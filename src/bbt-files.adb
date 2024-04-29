-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (http://lionel.draghi.free.fr/bbt/)
-- © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- http://www.apache.org/licenses/LICENSE-2.0
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------

with BBT.IO;
with BBT.Settings;       use BBT.Settings;

with Ada.Directories;

package body BBT.Files is

   -- --------------------------------------------------------------------------
   -- IO renamed with "Spawn" as Topic
   procedure Put_Line
     (Item  : String;
      File  : String  := "";
      Line  : Integer := 0;
      Level : Print_Out_Level := Normal;
      Topic : Extended_Topics := BBT_Files) renames IO.Put_Line;
   --  procedure Put (Item  : String;
   --                 File  : String  := "";
   --                 Line  : Integer := 0;
   --                 Level : Print_Out_Level := Normal;
   --                 Topic : Extended_Topics := Spawn) renames IO.Put;
   --  procedure New_Line (Level : Print_Out_Level := Normal;
   --                      Topic : Extended_Topics := Spawn) renames IO.New_Line;


   -- --------------------------------------------------------------------------
   function "+" (Name : File_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));

   -- --------------------------------------------------------------------------
   --  function Is_Dir (File_Name : String) return Boolean is
   --     use Ada.Directories;
   --  begin
   --     return Exists (File_Name) and then Kind (File_Name) = Directory;
   --  end Is_Dir;

   The_List : File_List.Vector;

   -- --------------------------------------------------------------------------
   procedure Append (File_Name : String) is
   begin
      --  if File_Name /= Settings.Template_Name then
      --     --  The md file created with --create-template is not supposed to
      --     --  be executed.
         The_List.Append (File_Name);
      --  end if;
   end Append;

   -- --------------------------------------------------------------------------
   procedure Find_BBT_Files (Recursive : Boolean) is

      use Ada.Directories;

      Current : constant String := Current_Directory;

      -- -----------------------------------------------------------------------
      procedure Walk (Name : String) is
         -- code mostly from :
         -- https://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada

         -- --------------------------------------------------------------------
         procedure Print (Item : Directory_Entry_Type) is
            -- Fixme: rename Print
            Name : constant String := Full_Name (Item);
         begin
            if Name'Length > Current'Length and then
              Name (Name'First .. Name'First + Current'Length - 1) = Current
            -- Simple optimization : if the long path is a subdir of the
            -- current one, we only print the subdir
            then
               Files.The_List.Append
                 ((Name (Name'First + Current'Length + 1 .. Name'Last)));
            else
               Files.The_List.Append ((Name));
            end if;
         end Print;

         -- --------------------------------------------------------------------
         procedure Walk (Item : Directory_Entry_Type) is
         begin
            if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".."
            then
               -- This is OK with Unix and Windows dir, so I consider
               -- it as portable.
               Walk (Full_Name (Item));
            end if;
         exception when Name_Error => null;
         end Walk;

         Extension : constant String := "*.md";

      begin
         Put_Line (Item  => "Walking in " & Name, Level => IO.Debug);
         Search (Directory => Name,
                 Pattern   => Extension,
                 Filter    => [Directory => False, others => True],
                 Process   => Print'Access);
         if Recursive then
            Search (Directory => Name,
                    Pattern   => "",
                    Filter    => [Directory => True, others => False],
                    Process   => Walk'Access);
         end if;
      end Walk;

   begin
      Walk ("./");
      Put_Line (Item  => "Found " & The_List'Image, Level => IO.Debug);

   end Find_BBT_Files;

   -- --------------------------------------------------------------------------
   function No_bbt_File return Boolean is (The_List.Is_Empty);
   function bbt_Files return File_List.Vector is (The_List);

end BBT.Files;
