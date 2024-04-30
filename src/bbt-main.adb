with Ada.Text_IO;
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

with BBT.Documents; use BBT.Documents;
with BBT.IO;
with BBT.Files;
with BBT.Settings;
with BBT.Tests_Builder;
with BBT.Tests_Runner;

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure BBT.Main is

   use all type BBT.Settings.Print_Out_Level;

   -- --------------------------------------------------------------------------
   -- Put_Line Utilities:
   procedure Put_Help     is separate;
   procedure Put_Settings is separate;
   procedure Put_Topics   is separate;
   procedure Put_Error (Msg       : String  := "";
                        With_Help : Boolean := False) is separate;
   procedure Create_Template  is separate;
   procedure Analyze_BBT_File (File_Name : String) is separate;
   procedure Analyze_Cmd_Line is separate; -- Cmd line options are then
                                           -- available in the Settings package.

begin
   -- --------------------------------------------------------------------------
   Analyze_Cmd_Line;

   if IO.Some_Error then
      -- If some error occurs during command line analysis, stop here,
      -- even if Ignore_Errors or Keep_Going is set
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   -- Command processing : "There can be only one" -----------------------------
   -- if one is found, exit
   -- (options are already processed in the analyze Cmd_Line procedure)

   if Settings.No_File_Given then
      Files.Find_BBT_Files (Settings.Recursive);
   end if;

   if Settings.List_Files then
      for File of Files.BBT_Files loop
         Ada.Text_IO.Put_Line (File);
      end loop;
      return;
   end if;

   if Settings.Create_Template then
      Create_Template;
      return;
   end if;

   if Settings.Help_Needed then
      Put_Help;
      return;
   end if;

   if Settings.List_Topics then
      Put_Topics;
      return;
   end if;

   if Settings.List_Settings then
      Put_Settings;
      return;
   end if;

   if Files.No_BBT_File then
      -- No file given on cmd line, and no bbt file found
      Put_Error ("No bbt file found");
   else
      for File of Files.BBT_Files loop
         IO.Put_Line ("Loading " & File'Image, Level => IO.Verbose);
         Analyze_BBT_File (File);
      end loop;
   end if;

   if Settings.Explain then
      -- let's display our rebuild of the original test definition file
      -- comment lines are filtered out
      Put_Document_List (BBT.Tests_Builder.The_Document_List.all,
                         With_Comments      => Settings.With_Comments,
                         With_Bold_Keywords => True);
   end if;

   Tests_Runner.Run_All;
   Put_Run_Summary;
   -- "run" is the default action, so they should'nt be any other action
   --  processed after that point. "run" apply

   if IO.Some_Error and not Settings.Ignore_Errors then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end BBT.Main;
