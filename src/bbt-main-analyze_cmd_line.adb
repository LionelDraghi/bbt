-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO; use BBT.IO;
with BBT.Settings;

-- with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

separate (BBT.Main)

procedure Analyze_Cmd_Line is

   -- --------------------------------------------------------------------------
   Arg_Counter : Positive := 1;

   -- --------------------------------------------------------------------------
   procedure Next_Arg is
   begin
      Arg_Counter := Arg_Counter + 1;
   end Next_Arg;

   use Ada.Directories;

begin
   -- --------------------------------------------------------------------------
   -- NB: command line, including arguments should comply with GNU Coding
   -- standards
   -- (https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)

   if Ada.Command_Line.Argument_Count = 0 then
      Settings.Help_Needed := True;
      return;
   end if;

   Opt_Analysis_Loop : while Arg_Counter <= Ada.Command_Line.Argument_Count loop

      declare
         Arg : constant String := Ada.Command_Line.Argument (Arg_Counter);
         use Settings;

      begin
         -- Commands -----------------------------------------------------------
         -- Note that after v0.0.6, commands starting with '-' are deprecated
         if  Arg in "-h" | "--help" | "he" | "help" then
            Settings.Help_Needed := True;
            return;

         elsif Arg in "-e" | "--explain" | "ex" | "explain" then
            Settings.Explain := True;

         elsif Arg in "-lf" | "--list_files" | "lf" | "list_files" then
            Settings.List_Files := True;

         elsif Arg in "-lk" | "--list_keywords" | "lk" | "list_keywords" then
            Settings.List_Keywords := True;

         elsif Arg in "-lg" | "--list_grammar" | "lg" | "list_grammar" then
            Settings.List_Grammar := True;

         elsif Arg in "-ct" | "--create_template" | "ct" | "create_template" then
            Settings.Create_Template := True;

            -- Options ---------------------------------------------------------
         elsif Arg = "-o" or Arg = "--output" then
            Next_Arg;
            Settings.Set_Result_File (Ada.Command_Line.Argument (Arg_Counter));
            IO.Enable_Tee (Settings.Result_File_Name,
                           Verbosity => Verbose);
            -- Verbose is the right detail level for the Markdown output file,
            -- even if --quiet or -- verbose is set.

         --  elsif Arg = "-ot" or Arg = "--output_tag" then
         --     Next_Arg;
         --     -- Fixme: opt -ot / --output_tag not yet coded

         elsif Arg = "-iw" or Arg = "--ignore_whitespaces" then
            Settings.Ignore_Whitespaces := True;

         elsif Arg = "-ic" or Arg = "--ignore_casing" then
            Settings.Ignore_Casing := True;

         elsif Arg = "-ibl" or Arg = "--ignore_blank_lines" then
            Settings.Ignore_Blank_Lines := True;

         elsif Arg = "-hm" or Arg = "--human_match" then
            Settings.Ignore_Whitespaces := True;
            Settings.Ignore_Casing      := True;
            Settings.Ignore_Blank_Lines := True;

         elsif Arg = "-em" or Arg = "--exact_match" then
            Settings.Ignore_Whitespaces := False;
            Settings.Ignore_Casing      := False;
            Settings.Ignore_Blank_Lines := False;

         elsif Arg = "-ed" or Arg = "--exec_dir" then
            Next_Arg;
            Settings.Set_Exec_Dir (Ada.Command_Line.Argument (Arg_Counter));

         elsif Arg = "-td" or Arg = "--tmp_dir" then
            Next_Arg;
            Settings.Set_Tmp_Dir (Ada.Command_Line.Argument (Arg_Counter));

         elsif Arg = "-r" or Arg = "--recursive" then
            Settings.Recursive := True;

         elsif Arg = "-k" or Arg = "--keep_going" then
            Settings.Keep_Going := True;

         elsif Arg = "-c" or Arg = "--cleanup" then
            Settings.Cleanup := True;

         elsif Arg = "--yes" then
            Settings.Yes := True;

         elsif Arg = "-v" or Arg = "--verbose" then
            Set_Verbosity  (Verbose);

         elsif Arg = "-q" or Arg = "--quiet" then
            Set_Verbosity  (Quiet);

         elsif Arg = "-d" then
            -- undocumented option
            Set_Verbosity  (Debug);

         elsif Arg = "-ls" then
            -- undocumented option, list settings
            Settings.List_Settings := True;

         elsif Arg = "--strict" then
            Settings.Strict_Gherkin := True;

         elsif Arg = "-sb" or Arg = "--status_bar" then
            Settings.Status_Bar := True;

         elsif Arg = "-gb" or Arg = "--generate_badge" then
            Settings.Generate_Badge := True;
            Next_Arg;
            Settings.Set_Badge_File_Name (Ada.Command_Line.Argument (Arg_Counter));

         elsif Ada.Directories.Exists (Arg) then
            -- if it's not an option, its a file name
            case Kind (Arg) is
               when Directory =>
                  Settings.No_File_Given := False;
                  Scenarios.Files.Find_BBT_Files
                    (Start_In  => Arg,
                     Recursive => Settings.Recursive);

               when Ordinary_File =>
                  Settings.No_File_Given := False;
                  Scenarios.Files.Append_File (Arg);

               when Special_File =>
                  IO.Put_Error ("Unknown file type """ & Arg & """");
                  return;

            end case;

         else
            IO.Put_Error ("Unknown option or file """ & Arg & """");

         end if;

         if IO.Some_Error then return; end if;
         -- No need to further analyze command line, or to do
         -- Options_Coherency_Tests.
      end;

      Next_Arg;

   end loop Opt_Analysis_Loop;

   -- --------------------------------------------------------------------------
   if Debug_Mode then
      Put_Settings;
   end if;

end Analyze_Cmd_Line;
