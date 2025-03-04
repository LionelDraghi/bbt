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

   function Dash_To_Underscore (S : String) return String is
   -- Transform option like "--list-file" in "--list_file"
      Tmp : String (S'Range);
      Conversion_On : Boolean := False;
      -- The conversion starts after the first letter, otherwise we
      -- end up converting --list to __list.
   begin
      for I in S'Range loop
         if S (I) = '-' and Conversion_On then
            Tmp (I) := '_';
         else
            Tmp (I) := S (I);
         end if;

         if S (I) /= '-' then
            Conversion_On := True;
         end if;
      end loop;
      return Tmp;
   end Dash_To_Underscore;

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
         -- arg are either a command or option, in which case we neutralize the
         -- '-' character to have --list_item = --list-item
         -- or a file name, in which case it remain
         File : constant String
           := Ada.Command_Line.Argument (Arg_Counter);
         Cmd  : constant String
           := Dash_To_Underscore (Ada.Command_Line.Argument (Arg_Counter));
         use Settings;

      begin
         -- Commands -----------------------------------------------------------
         -- Note that after v0.0.6, commands starting with '-' are deprecated
         if  Cmd in "-h" | "--help" | "he" | "help" then
            Settings.Help_Needed := True;
            return;

         elsif Cmd in "-e" | "--explain" | "ex" | "explain" then
            Settings.Explain := True;

         elsif Cmd in "-lf" | "--list_files" | "lf" | "list_files" then
            Settings.List_Files := True;

         elsif Cmd in "-lk" | "--list_keywords" | "lk" | "list_keywords" then
            Settings.List_Keywords := True;

         elsif Cmd in "-lg" | "--list_grammar" | "lg" | "list_grammar" then
            Settings.List_Grammar := True;

         elsif Cmd in "-ct" | "--create_template" | "ct" | "create_template" then
            Settings.Create_Template := True;

            -- Options ---------------------------------------------------------
         elsif Cmd = "-o" or Cmd = "--output" then
            Next_Arg;
            Settings.Set_Result_File (Ada.Command_Line.Argument (Arg_Counter));
            --  IO.Enable_Tee (Settings.Result_File_Name,
            --                 Verbosity => Verbose);
            Writers.Enable_Output
              (For_Format => MD, -- Fixme:, format à déterminer avec l'extension
               File_Name  => Ada.Command_Line.Argument (Arg_Counter));
            -- Verbose is the right detail level for the Markdown output file,
            -- even if --quiet or -- verbose is set.

         --  elsif Arg = "-ot" or Arg = "--output_tag" then
         --     Next_Arg;
         --     -- Fixme: opt -ot / --output_tag not yet coded

         elsif Cmd = "-iw" or Cmd = "--ignore_whitespaces" then
            Settings.Ignore_Whitespaces := True;

         elsif Cmd = "-ic" or Cmd = "--ignore_casing" then
            Settings.Ignore_Casing := True;

         elsif Cmd = "-ibl" or Cmd = "--ignore_blank_lines" then
            Settings.Ignore_Blank_Lines := True;

         elsif Cmd = "-hm" or Cmd = "--human_match" then
            Settings.Ignore_Whitespaces := True;
            Settings.Ignore_Casing      := True;
            Settings.Ignore_Blank_Lines := True;

         elsif Cmd = "-em" or Cmd = "--exact_match" then
            Settings.Ignore_Whitespaces := False;
            Settings.Ignore_Casing      := False;
            Settings.Ignore_Blank_Lines := False;

         elsif Cmd = "-ed" or Cmd = "--exec_dir" then
            Next_Arg;
            Settings.Set_Exec_Dir (Ada.Command_Line.Argument (Arg_Counter));

         elsif Cmd = "-td" or Cmd = "--tmp_dir" then
            Next_Arg;
            Settings.Set_Tmp_Dir (Ada.Command_Line.Argument (Arg_Counter));

         elsif Cmd = "-r" or Cmd = "--recursive" then
            Settings.Recursive := True;

         elsif Cmd = "-k" or Cmd = "--keep_going" then
            Settings.Keep_Going := True;

         elsif Cmd = "-c" or Cmd = "--cleanup" then
            Settings.Cleanup := True;

         elsif Cmd = "--yes" then
            Settings.Yes := True;

         elsif Cmd = "-v" or Cmd = "--verbose" then
            Set_Verbosity  (Verbose);

         elsif Cmd = "-q" or Cmd = "--quiet" then
            Set_Verbosity  (Quiet);

         elsif Cmd = "-d" then
            -- undocumented option
            Set_Verbosity  (Debug);

         elsif Cmd = "-ls" then
            -- undocumented option, list settings
            Settings.List_Settings := True;

         elsif Cmd = "--strict" then
            Settings.Strict_Gherkin := True;

         elsif Cmd = "-sb" or Cmd = "--status_bar" then
            Settings.Status_Bar := True;

         elsif Cmd = "-gb" or Cmd = "--generate_badge" then
            Settings.Generate_Badge := True;
            Next_Arg;
            Settings.Set_Badge_File_Name (Ada.Command_Line.Argument (Arg_Counter));

         elsif Ada.Directories.Exists (File) then
            -- if it's not an option, its a file name
            case Kind (File) is
               when Directory =>
                  Settings.No_File_Given := False;
                  BBT.Scenarios.Files.Get_Document_List
                    (Start_In  => File,
                     Recursive => Settings.Recursive);

               when Ordinary_File =>
                  Settings.No_File_Given := False;
                  BBT.Scenarios.Files.Append_File (File);

               when Special_File =>
                  IO.Put_Error ("Unknown file type """ & File & """");
                  return;

            end case;

         else
            IO.Put_Error ("Unknown option or file """ & File & """");

         end if;

         --  IO.Put_Error (BBT.Output_Format.One_Line_Image
         --                (BBT.Output_Format.BBT_Files));

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
