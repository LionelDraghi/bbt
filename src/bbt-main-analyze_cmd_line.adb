-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO;                use BBT.IO;
with BBT.Settings;
with BBT.Scenarios.Readers; use BBT.Scenarios.Readers;

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

   -- --------------------------------------------------------------------------
   function Last_Arg return Boolean is
     (Arg_Counter = Ada.Command_Line.Argument_Count);

   -- --------------------------------------------------------------------------
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

   -- --------------------------------------------------------------------------
   procedure Set_Cmd (C : Settings.Command) is
      use Settings;
   begin
      if Current_Command = None then
         Current_Command := C;
      else
         IO.Put_Error ("Cannot have both " & Current_Command'Image
                       & " and " & C'Image & " on command line");
      end if;
   end Set_Cmd;

   use Ada.Directories;

begin
   -- --------------------------------------------------------------------------
   -- NB: command line, including arguments should comply with GNU Coding
   -- standards
   -- (https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)

   if Ada.Command_Line.Argument_Count = 0 then
      Set_Cmd (Settings.Help);
      -- If bbt is called without parameter, Help is the default command.
      -- But if the command line is populated, and no command given,
      -- then the default command is Run.
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
            Set_Cmd (Help);
            return;

         elsif Cmd in "-e" | "--explain" | "ex" | "explain" then
            Set_Cmd (Explain);

         elsif Cmd in "-lf" | "--list_files" | "lf" | "list_files" then
            Settings.List_Files := True;

         elsif Cmd in "-lk" | "--list_keywords" | "lk" | "list_keywords" then
            Set_Cmd (List_Keywords);

         elsif Cmd in "-lg" | "--list_grammar" | "lg" | "list_grammar" then
            Set_Cmd (List_Grammar);

         elsif Cmd in "-ct" | "--create_template" | "ct" | "create_template"
         then
            Set_Cmd (Create_Template);

            -- Options ---------------------------------------------------------
         elsif Cmd = "-o" or Cmd = "--output" then
            if Last_Arg then
               IO.Put_Error ("-o must be followed by a file name");
            else
               Next_Arg;
               Settings.Set_Result_File (Ada.Command_Line.Argument (Arg_Counter));
            end if;

            declare
               Writer_Found : Boolean := False;
               Format       : Writers.Output_Format;
               File_Name    : constant String := Settings.Result_File_Name;
            begin
               Writers.File_Format (File_Name    => File_Name,
                                    Found        => Writer_Found,
                                    Found_Format => Format);
               if Writer_Found then
                  Writers.Enable_Output (For_Format => Format,
                                         File_Name  => File_Name);

               else
                  IO.Put_Error
                    ("Unkwnow format for file """ & File_Name & """");

               end if;
            end;

            --  IO.Enable_Tee (Settings.Result_File_Name,
            --                 Verbosity => Verbose);
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
            if Last_Arg then
               IO.Put_Error ("-ed must be followed by a file name");
            else
               Next_Arg;
               Settings.Set_Exec_Dir (Ada.Command_Line.Argument (Arg_Counter));
            end if;

         elsif Cmd = "-td" or Cmd = "--tmp_dir" then
            if Last_Arg then
               IO.Put_Error ("-td must be followed by a file name");
            else
               Next_Arg;
               Settings.Set_Tmp_Dir (Ada.Command_Line.Argument (Arg_Counter));
            end if;

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
            -- Undocumented option ---------------------------------------------
            if Last_Arg then
               IO.Put_Error ("-d may be followed by ");
               for T in Topics loop
                  IO.Put_Line (T'Image);
               end loop;
            else
               Next_Arg;
               if Topics'Valid_Value (Ada.Command_Line.Argument (Arg_Counter))
               then
                  -- -d is followed by the topic to watch
                  Enable_Topic
                    (Topic => Topics'Value
                       (Ada.Command_Line.Argument (Arg_Counter)));
               else
                  -- there is only -d
                  Set_Verbosity  (Debug);
                  Arg_Counter := @ - 1;
               end if;
            end if;

         elsif Cmd = "-ls" then
            -- undocumented option, list settings
            Set_Cmd (List_Settings);

         elsif Cmd = "--strict" then
            Settings.Strict_Gherkin := True;

         elsif Cmd = "-sb" or Cmd = "--status_bar" then
            Settings.Status_Bar := True;

         elsif Cmd = "-gb" or Cmd = "--generate_badge" then
            if Last_Arg then
               IO.Put_Error ("-gb must be followed by a file name");
            else
               Next_Arg;
               Settings.Generate_Badge := True;
               Settings.Set_Badge_File_Name
                 (Ada.Command_Line.Argument (Arg_Counter));
            end if;

         elsif Ada.Directories.Exists (File) then
            -- if it's not an option, its a file name
            case Kind (File) is
               when Directory =>
                  Settings.No_File_Given := False;
                  BBT.Scenarios.Files.Find_Documents
                    (Dir  => File,
                     Recursive => Settings.Recursive);

               when Ordinary_File =>
                  Settings.No_File_Given := False;
                  BBT.Scenarios.Files.Add_Document (File);

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
