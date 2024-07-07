-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with BBT.Documents; use BBT.Documents;
with BBT.IO;
with BBT.Scenarios.Files;
with BBT.Scenarios.Step_Parser;
with BBT.Settings;
with BBT.Tests.Builder;
with BBT.Tests.Runner;

with Ada.Command_Line;
-- with Ada.Calendar;
-- with Ada.Calendar.Formatting;
with Ada.Text_IO;

procedure BBT.Main is

   -- use all type BBT.IO.Print_Out_Level;

   -- --------------------------------------------------------------------------
   -- Put_Line Utilities:
   procedure Put_Help     is separate;
   procedure Put_Settings is separate;
   procedure Put_Topics   is separate;
   --  procedure Put_Error (Msg       : String  := "";
   --                       With_Help : Boolean := False) is separate;
   procedure Create_Template  is separate;
   procedure Analyze_Cmd_Line is separate;
   -- Cmd line options are then available in the Settings package.

begin
   -- --------------------------------------------------------------------------
   Analyze_Cmd_Line;
   IO.Set_Reference_Directory (Settings.Initial_Directory);
   --  To get the file name relative to ths start dir, and not
   --  absolute Path.

   if IO.Some_Error then
      -- If some error occurs during command line analysis, stop here,
      -- even if Ignore_Errors or Keep_Going is set
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if Settings.No_File_Given then
      Scenarios.Files.Find_BBT_Files (Settings.Recursive);
   end if;

   -- Command processing :
   --   Principle : "There can be only one". If one action is found, then
   --               do the job and exit.
   --   (options are already processed in the analyze Cmd_Line procedure)

   if Settings.List_Files then
      for File of Scenarios.Files.BBT_Files loop
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

   if Settings.List_Keywords then
      BBT.Scenarios.Step_Parser.Put_Keywords;
      return;
   end if;

   if Settings.List_Grammar then
      BBT.Scenarios.Step_Parser.Put_Grammar;
      return;
   end if;

   if Scenarios.Files.No_BBT_File then
      -- No file given on cmd line, and no bbt file found
      IO.Put_Error ("No md file found", IO.No_Location);
   else
      for File of Scenarios.Files.BBT_Files loop
         IO.Put_Line ("Loading " & File'Image, IO.No_Location, IO.Debug);
         Scenarios.Files.Analyze_MDG_File (File);
      end loop;
      Tests.Builder.Duplicate_Multiple_Run;
      -- Process in all recorded scenario the
      -- duplication of the "run X or Y" steps.
   end if;

   if Settings.Explain then
      -- let's display our rebuild of the original test definition file
      -- comment lines are filtered out
      Put_Document_List (BBT.Tests.Builder.The_Tests_List.all);

   else
      -- Finally, the "normal" run situation:

      --  declare
      --     use Ada.Calendar;
      --     Start_Time : constant Time := Clock;
      --     End_Time   : Time;
      --  begin
      Tests.Runner.Run_All;
      Documents.Compute_Overall_Tests_Results;
      Documents.Put_Overall_Results;
      --     End_Time := Clock;
      --     IO.New_Line;
      --     IO.Put_Line ("- Start Time = " & IO.Image (Start_Time));
      --     IO.Put_Line ("- End Time   = " & IO.Image (End_Time));
      --  end;

      -- "run" is the default action, so they shouldn't be any other action
      --  processed after that point.
   end if;

   if (IO.Some_Error and then not Settings.Ignore_Errors)
   or else Overall_Results (Failed) /= 0
   then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end BBT.Main;
