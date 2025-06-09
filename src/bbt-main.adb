-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Model.Documents,
     BBT.Cmd_Line,
     BBT.Scenarios.Files,
     BBT.Scenarios.Readers.Adoc_Reader,
     BBT.Scenarios.Readers.MDG_Reader,
     BBT.Scenarios.Step_Parser,
     BBT.Settings,
     BBT.Status_Bar,
     BBT.Tests.Builder,
     BBT.Tests.Results,
     BBT.Tests.Runner,
     BBT.Writers,
     BBT.Writers.Text_Writer,
     BBT.Writers.Markdown_Writers,
     BBT.Writers.Asciidoc_Writer;

with Ada.Command_Line,
--  Ada.Calendar,
--  Ada.Calendar.Formatting,
     Ada.Text_IO;

use BBT,
    BBT.Scenarios.Readers,
    BBT.Settings,
    BBT.Tests.Results,
    BBT.Writers;

procedure BBT.Main is

   -- --------------------------------------------------------------------------
   procedure Analyze_Documents is
   begin
      if Scenarios.Files.No_Document_Found then
         -- No file given on cmd line, and no bbt file found
         IO.Put_Error ("No scenario file found", IO.No_Location);

      else
         -- Here we are in the normal execution flow
         Status_Bar.Put_Activity ("Loading files");

         for File of Scenarios.Files.Document_List loop
            Scenarios.Files.Analyze_Document (File);
            exit when IO.Some_Error and not Settings.Keep_Going;
            -- If there is some error during the file analysis, we don't go
            -- further except if the Keep_Going option is set.
            Tests.Builder.Duplicate_Multiple_Run;
            -- Process in all recorded scenario the
            -- duplication of the "run X or Y" steps.
         end loop;
      end if;

      Model.Documents.Apply_Filters;

   end Analyze_Documents;

begin
   -- --------------------------------------------------------------------------
   MDG_Reader.Initialize;
   Adoc_Reader.Initialize;

   Text_Writer.Initialize;
   Markdown_Writers.Initialize;
   Asciidoc_Writer.Initialize;

   Cmd_Line.Analyze;

   if No_Output_Format_Enabled then
      Writers.Enable_Output (For_Format => Txt);
      -- Default, for when there is no explicit --output
   end if;

   if Settings.Status_Bar then
      Status_Bar.Enable;
   end if;

   if Settings.List_Settings then
      Status_Bar.Put_Activity ("Listing settings");
      Cmd_Line.Put_Settings;
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

   --  if Settings.No_File_Given then
   --     Scenarios.Files.Find_BBT_Files (Settings.Recursive);
   --  end if;

   -- Command processing :
   --   Principle : "There can be only one". If one action is found, then
   --               do the job and exit.
   --   (options are already processed in the analyze Cmd_Line procedure)
   case Settings.Current_Command is

   when List_Trace_Topics =>
      Status_Bar.Put_Activity ("Listing trace topics");
      Cmd_Line.Put_Trace_Topics;

   when List_Keywords =>
      Status_Bar.Put_Activity ("Listing keywords");
      Scenarios.Step_Parser.Put_Keywords;
      return;

   when List_Grammar =>
      Status_Bar.Put_Activity ("Listing grammar");
      BBT.Scenarios.Step_Parser.Put_Grammar;

   when List_Files =>
      Status_Bar.Put_Activity ("Listing files");
      for File of Scenarios.Files.Document_List loop
         Ada.Text_IO.Put_Line (File);
      end loop;
      -- return;

   when Explain =>
      Status_Bar.Put_Activity ("Analyzing documents");
      Analyze_Documents;
      -- Dry run --
      -- Let's display our rebuild of the original test definition
      -- file comment lines are filtered out.
      Status_Bar.Put_Activity ("Display loaded scenarios");
      Writers.Explain (Model.Documents.The_Tests_List.all);

   when List =>
      Status_Bar.Put_Activity ("Analyzing documents");
      Analyze_Documents;
      Status_Bar.Put_Activity ("Display non filtered items");
      Writers.Put_Document_List (Model.Documents.The_Tests_List.all);

   when Run | Settings.None =>
      -- Run is the default command, so None => Run
      Status_Bar.Put_Activity ("Analyzing documents");
      Analyze_Documents;

      if IO.No_Error or Settings.Keep_Going then
         -- Real run --
         Status_Bar.Put_Activity ("Running non filtered items");
         Tests.Runner.Run_All;
      end if;
      Tests.Results.Sum_Results (Model.Documents.The_Tests_List.all);
      Writers.Put_Overall_Results;

      if Settings.Generate_Badge then
         Tests.Results.Generate_Badge;
      end if;

   when Create_Template =>
      Status_Bar.Put_Activity ("Creating template");
      Cmd_Line.Create_Template;

   when Version =>
      Ada.Text_IO.Put_Line ("bbt version " & Settings.BBT_Version);

   when Help =>
      --  if Settings.Current_Topic = Tutorial then
      --     Status_Bar.Put_Activity ("Creating tutorial");
      --     Cmd_Line.Create_Template;
      --
      --  elsif Settings.Current_Topic = Example then
      --     Status_Bar.Put_Activity ("Creating example");
      --     Cmd_Line.Create_Example;
      --
      --  else
         Cmd_Line.Put_Help (Settings.Current_Topic);

      --  end if;
   end case;

   -- --------------------------------------------------------------------
   -- "run" is the default action, so they shouldn't be any other action
   --  processed after that point.
   -- --------------------------------------------------------------------

   if (IO.Some_Error and then not Settings.Ignore_Errors)
     or else not Tests.Results.No_Fail
   then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end BBT.Main;
