-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Model.Documents,
     BBT.Model.Steps,
     BBT.Cmd_Line,
     BBT.GUI,
     BBT.Scenarios.Files,
     BBT.Scenarios.Readers.Adoc_Reader,
     BBT.Scenarios.Readers.MDG_Reader,
     BBT.Scenarios.Steps,
     BBT.Settings,
     BBT.Status_Bar,
     BBT.Tests.Builder,
     BBT.Backends,
     BBT.Tests.Runner,
     BBT.Writers,
     BBT.Writers.Text_Writer,
     BBT.Writers.Markdown_Writers,
     BBT.Writers.Asciidoc_Writer,
     BBT.JUnit_Writer;

with Ada.Command_Line,
     Ada.Text_IO;

use BBT,
    BBT.Scenarios.Readers,
    BBT.Settings,
    BBT.Writers;

package body BBT.Driver is

   -- --------------------------------------------------------------------------
   procedure Analyze_Documents is
   begin
      if Scenarios.Files.No_Document_Found then
         -- No file given on cmd line, and no bbt file found
         IO.Put_Warning ("No scenario file found", IO.No_Location);

      else
         -- Here we are in the normal execution flow
         Status_Bar.Put_Activity ("Loading files");

         for File of Scenarios.Files.Document_List loop
            Scenarios.Files.Analyze_Document (File);
            return when IO.Some_Error and Settings.Stop_On_Error;
            -- If there is some error during the file analysis, we don't go
            -- further except if the Keep_Going option is set.
         end loop;

         Tests.Builder.Duplicate_Multiple_Run;
         -- Process in all recorded scenario the
         -- duplication of the "run X or Y" steps.

         Model.Documents.Apply_Filters;

         if Model.Steps.Some_Gui_Steps
           and then BBT.Backends.Selected = null
         then
            --  Eager backend check (cf.
            --  docs/dev/design_discussions.md (D16)): documents with
            --  GUI steps are rejected before any scenario runs, with
            --  an actionable backend selection hint. In debugger
            --  mode, the backend lives in the backend executable
            --  (the debugger itself is in every bbt executable): the
            --  working combination is --gui --backend=<kind>.
            if Settings.Gui then
               IO.Put_Error
                 ("no GUI backend available in the interactive debugger;"
                  & " run the debugger in the backend executable instead,"
                  & " e.g. bbt --gui --backend=<kind> <scenarios>");
            else
               IO.Put_Error
                 ("no GUI backend available: select one with --backend=<kind>");
            end if;
         end if;

      end if;
   end Analyze_Documents;

   -- --------------------------------------------------------------------------

   procedure Run is
   begin
      MDG_Reader.Initialize;
      Adoc_Reader.Initialize;

      Text_Writer.Initialize;
      Markdown_Writers.Initialize;
      Asciidoc_Writer.Initialize;

      Cmd_Line.Analyze;

      if Settings.Gui_Backend_Ran then
         --  The --backend=<kind> launcher replaced this process by the
         --  backend executable, whose exit status is already propagated:
         --  nothing more to run in this process
         return;
      end if;

      if IO.Some_Error then
         -- If some error occurs during command line analysis, stop here,
         -- even if Ignore_Errors or Keep_Going is set
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      if No_Output_Format_Enabled then
         Writers.Enable_Output (For_Format => Markdown);
         -- Default, for when there is no explicit --output
      end if;

      if not (Settings.No_TTY or else Settings.Gui) then
         --  The status bar is the default, when the standard output is
         --  a terminal; --no_tty disables it, and wins over
         --  --force_status_bar.
         Status_Bar.Enable (Force => Settings.Force_Status_Bar);
      end if;

      --  The one shot commands (listing, help, version) do not use the
      --  status bar: they are instantaneous, and most of their output
      --  does not go through BBT.IO.

      if Settings.List_Settings then
         Cmd_Line.Put_Settings;
      end if;

      IO.Set_Reference_Directory (Settings.Launch_Directory);
      --  To get the file name relative to the start dir, and not
      --  absolute Path.

      -- Command processing :
      --   Principle : "There can be only one". If one action is found, then
      --               do the job and exit.
      --   (options are already processed in the analyze Cmd_Line procedure)
      case Settings.Current_Command is

      when List_Trace_Topics =>
         Cmd_Line.Put_Trace_Topics;

      when List_Keywords =>
         Scenarios.Steps.Put_Keywords;
         return;

      when List_Grammar =>
         BBT.Scenarios.Steps.Put_Grammar;

      when List_Files =>
         for File of Scenarios.Files.Document_List loop
            Ada.Text_IO.Put_Line (File);
         end loop;

      when Explain =>
         -- Dry run:
         Status_Bar.Put_Activity ("Analyzing documents");
         Analyze_Documents;
         -- Let's display our rebuild of the original test definition.
         -- File comment lines are filtered out.
         Status_Bar.Put_Activity ("Display loaded scenarios");
         Writers.Explain (Model.Documents.Doc_List.all);

      when Run | Settings.None =>
         -- Run is the default command, so None => Run
         Status_Bar.Put_Activity ("Analyzing documents");
         Analyze_Documents;

         if Settings.Gui then
            --  GUI mode: hand over the execution to the interactive
            --  debugger window, and print the results when it closes.
            BBT.GUI.Run;
            Model.Documents.Sum_Results (Model.Documents.Doc_List.all);
            Writers.Put_Overall_Results;

         elsif IO.No_Error or Settings.Keep_Going then
            -- Real run --
            Status_Bar.Put_Activity ("Running non filtered items");
            Tests.Runner.Run_All;
            Model.Documents.Sum_Results (Model.Documents.Doc_List.all);
            Writers.Put_Overall_Results;

            if Settings.Generate_Badge then
               Model.Documents.Generate_Badge;
            end if;

            if Settings.Generate_JUnit_Report then
               JUnit_Writer.Generate_Report
                 (File_Name => Settings.JUnit_File,
                  Docs      => Model.Documents.Doc_List.all);
            end if;

         end if;

      when Version =>
         Ada.Text_IO.Put_Line ("bbt version " & Settings.BBT_Version);

      when Help =>
         Cmd_Line.Put_Help (Settings.Current_Topic);

      end case;

      -- --------------------------------------------------------------------
      -- "run" is the default action, so they shouldn't be any other action
      --  processed after that point.
      -- --------------------------------------------------------------------

      Status_Bar.Clear;
      --  Erase the status bar, so that the terminal is left on a clean
      --  last line.

      if (IO.Some_Error and then not Settings.Ignore_Errors)
        or else not Model.Documents.No_Fail
      then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

   end Run;

end BBT.Driver;
