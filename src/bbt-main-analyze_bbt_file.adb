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

with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with BBT.Documents; use BBT.Documents;
with BBT.IO;
with BBT.MDG_Lexer;
with BBT.Step_Lexer;
with BBT.Tests_Builder; use BBT.Tests_Builder;

separate (BBT.Main)

procedure Analyze_BBT_File (File_Name : String) is

   --  Output : Text_IO.File_Type;
   --  Base_Name : String := Ada.Directories.Base_Name (File_Name);
   --  Dir_Name  : String := Ada.Directories.Containing_Directory (File_Name);
   Input : Ada.Text_IO.File_Type;
   Context : Extended_Step_Categories := Unknown;

   -- --------------------------------------------------------------------------
   -- IO renamed with "Spawn" as Topic
   procedure Put_Line
     (Item  : String;
      File  : String  := "";
      Line  : Integer := 0;
      Level : IO.Print_Out_Level := Normal;
      Topic : Settings.Extended_Topics := Settings.Lexer) renames IO.Put_Line;

   use Ada.Text_IO;

begin
   Put_Line ("Analysing file " & File_Name, Level => Debug);

   Open (Input,
         Mode => In_File,
         Name => File_Name);

   Tests_Builder.Add_Document (File_Name);
   -- The doc name is not in the file content

   while not End_Of_File (Input) loop
      Line_Processing : declare
         Line : aliased constant String := Get_Line (Input);
         use BBT.MDG_Lexer;
         Attrib : constant Line_Attributes := Parse_Line (Line'Access);

      begin
         -- Put_Line ("Processing Line = " & Line,         Level => Verbose);
         -- Put_Line ("State           = " & State'Image,  Level => Verbose);
         -- Put_Line ("Attrib          = " & Attrib'Image, Level => Debug);
         -- New_Line (Level => Verbose);
         Put_Line ("Lexer returns :" & Attrib'Image, Level => Debug);

         case Attrib.Kind is
            when Feature_Line =>
               Tests_Builder.Add_Feature (To_String (Attrib.Name));

            when Scenario_Line =>
               Tests_Builder.Add_Scenario (To_String (Attrib.Name));

            when Step_Line =>
               declare
                  S : constant Step_Details :=
                        Step_Lexer.Parse (Attrib.Step_Ln, Context);
                  -- We give context to the parser, so that it can understand
                  -- that a "And" line is in fact a "When" line thanks to the
                  -- previous.
               begin
                  Context := S.Cat;
                  Tests_Builder.Add_Step (S);
                  -- Put_Line (S'Image, Level => Quiet);
                  -- Tests_Builder.Add_Step (S);
               end;

            when Code_Fence =>
               Tests_Builder.Add_Code_Block;

            when Text_Line =>
               Tests_Builder.Add_Line (To_String (Attrib.Line));

            when Empty_Line => null;

         end case;

      end Line_Processing;

   end loop;

   -- and finally, let's record the document
   Close (Input);
   Put_Line ("Doc_List = " & Tests_Builder.The_Document_List.all'Image,
             Level => Debug);

exception
   when E : Missing_Scenario =>
      Put_Error (Ada.Exceptions.Exception_Message (E));
      Put_Error ("Interrupting " & File_Name & " processing");

   when E : others =>
      Put_Error (Ada.Exceptions.Exception_Message (E));
      Put_Error ("Unknown exception while processing " & File_Name);

end Analyze_BBT_File;
