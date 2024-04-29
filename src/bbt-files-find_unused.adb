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

with bbt.Files.File_Lists;
with bbt.IO;
with bbt.Settings;

with Ada.Directories;

procedure bbt.Files.Find_Unused (From   : in     File_Name; -- must be a dir
                                 Not_In : in     File_Lists.Map;
                                 Put_In : in out File_Lists.Map) is
   use bbt.Files.File_Lists;

   -- --------------------------------------------------------------------------
   procedure Walk_Dir (Dir : in String) is
      use Ada.Directories;
      Search : Search_Type;
      File   : Directory_Entry_Type;

   begin
      Put_Line ("Start searching new files in " & Dir,
                   Level => IO.Debug);
      Start_Search (Search,
                    Directory => Dir,
                    Pattern   => "*",
                    Filter    => [Ordinary_File => True,
                                  Directory     => True,
                                  others        => False]);

      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);
         declare
            Full_Name   : constant String := Ada.Directories.Full_Name   (File);
            Simple_Name : constant String := Ada.Directories.Simple_Name (File);

         begin
            if Simple_Name = ".." or Simple_Name = "." then
               Put_Line ("Ignoring " & Simple_Name, Level => IO.Debug);

            elsif Not_In.Contains (+Full_Name) then
               Put_Line (Simple_Name & " already known", Level => IO.Debug);

            else
               -- A genuine new file:
               Put_Line ("New file " & Simple_Name, Level => IO.Debug);
               if not Settings.In_Ignore_List (Simple_Name) then
                  Put_In.Insert (+Full_Name,
                                 Create (File => +Full_Name,
                                         Role => Unused));
               end if;

               if Kind (File) = Directory then
                  Walk_Dir (Full_Name); -- recursive call

               end if;

            end if;
         end;

      end loop;

   end Walk_Dir;

begin
   Walk_Dir (+From);

end bbt.Files.Find_Unused;
