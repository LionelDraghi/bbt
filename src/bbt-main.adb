-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Writers.Markdown_Writer,
     BBT.Writers.Asciidoc_Writer,
     BBT.Tests.Results,
     BBT.Scenarios.Files,
     BBT.Scenarios.Readers.MDG_Reader,
     BBT.Scenarios.Step_Parser,
     BBT.Settings,
     BBT.Status_Bar,
     BBT.Tests.Builder,
     BBT.Tests.Runner,
     BBT.Writers;

with Ada.Command_Line,
--  Ada.Calendar,
--  Ada.Calendar.Formatting,
     Ada.Text_IO;

use BBT.Scenarios.Readers,
    BBT.Writers,
    BBT.Tests.Results;

procedure BBT.Main is

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
   MDG_Reader.Initialize;
   Markdown_Writer.Initialize;
   Asciidoc_Writer.Initialize;

   Analyze_Cmd_Line;

   if No_Output_Format_Enabled then
      Writers.Enable_Output (For_Format => Markdown);
      -- Default, for when there is no explicit --output
   end if;

   if Settings.Status_Bar then
      Status_Bar.Enable;
   end if;

   IO.Set_Reference_Directory (Settings.Launch_Directory);
   --  To get the file name relative to the start dir, and not
   --  absolute Path.

   if IO.Some_Error then
      -- If some error occurs during command line analysis, stop here,
      -- even if Ignore_Errors or Keep_Going is set
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if Settings.Help_Needed then
      Put_Help;
      return;
   end if;

   --  if Settings.No_File_Given then
   --     Scenarios.Files.Find_BBT_Files (Settings.Recursive);
   --  end if;

   -- Command processing :
   --   Principle : "There can be only one". If one action is found, then
   --               do the job and exit.
   --   (options are already processed in the analyze Cmd_Line procedure)

   if Settings.List_Files then
      Status_Bar.Put_Activity ("Listing files");
      for File of Scenarios.Files.Document_List loop
         Ada.Text_IO.Put_Line (File);
      end loop;
      return;
   end if;

   if Settings.Create_Template then
      Status_Bar.Put_Activity ("Creating template");
      Create_Template;
      return;
   end if;

   if Settings.List_Topics then
      Status_Bar.Put_Activity ("Listing topics");
      Put_Topics;
      return;
   end if;

   if Settings.List_Settings then
      Status_Bar.Put_Activity ("Listing settings");
      Put_Settings;
      return;
   end if;

   if Settings.List_Keywords then
      Status_Bar.Put_Activity ("Listing keywords");
      Scenarios.Step_Parser.Put_Keywords;
      return;
   end if;

   if Settings.List_Grammar then
      Status_Bar.Put_Activity ("Listing grammar");
      BBT.Scenarios.Step_Parser.Put_Grammar;
      return;
   end if;

   if Scenarios.Files.No_Document_Found then
      -- No file given on cmd line, and no bbt file found
      IO.Put_Error ("No scenario file found", IO.No_Location);

   else
      -- HERE we hare in the normal execution flow
      Status_Bar.Put_Activity ("Loading files");

      for File of Scenarios.Files.Document_List loop
         Scenarios.Files.Analyze_Document (File);
         exit when IO.Some_Error and not Settings.Keep_Going;
      end loop;

      if not IO.Some_Error or Settings.Keep_Going then
      -- If there is some error during the file analysis, we don't go further
      -- except if the Keep_Going option is set.
         Tests.Builder.Duplicate_Multiple_Run;
         -- Process in all recorded scenario the
         -- duplication of the "run X or Y" steps.

         if Settings.Explain then
            -- Dry run --
            -- Let's display our rebuild of the original test definition file
            -- comment lines are filtered out
            Status_Bar.Put_Activity ("Display loaded scenarios");
            Writers.Put_Document_List (Tests.Builder.The_Tests_List.all);

         else
            if IO.No_Error or Settings.Keep_Going then
               -- Real run --
               Status_Bar.Put_Activity ("Running scenarios");
               Tests.Runner.Run_All;
            end if;
            Tests.Results.Sum_Results (Tests.Builder.The_Tests_List);
            Writers.Put_Overall_Results (Overall_Results);

            if Settings.Generate_Badge then
               Tests.Results.Generate_Badge;
            end if;

         end if;
      end if;
   end if;

   -- --------------------------------------------------------------------
   -- "run" is the default action, so they shouldn't be any other action
   --  processed after that point.
   -- --------------------------------------------------------------------

   if (IO.Some_Error and then not Settings.Ignore_Errors)
   or else Tests.Results.Overall_Results (Failed) /= 0
   then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end BBT.Main;
