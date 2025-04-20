-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Settings,
     BBT.Scenarios,
     BBT.Scenarios.Files,
     BBT.Tests.Filter_List,
     BBT.Writers;

with Ada.Command_Line,
     Ada.Directories;

use BBT.Cmd_Line,
    BBT.Scenarios,
    BBT.Scenarios.Files,
    BBT.Settings,
    BBT.Writers;

package body BBT.Cmd_Line is

   -- --------------------------------------------------------------------------
   Arg_Counter : Natural := 1;

   -- --------------------------------------------------------------------------
   procedure Go_Next_Arg is
   begin
      Arg_Counter := @ + 1;
      -- Put_Line ("Go_Next " & Arg_Counter'Image);
   end Go_Next_Arg;

   -- --------------------------------------------------------------------------
   procedure Go_Back_Previous_Arg is
   begin
      Arg_Counter := @ - 1;
      -- Put_Line ("Go_Previous_Arg " & Arg_Counter'Image);
   end Go_Back_Previous_Arg;

   -- --------------------------------------------------------------------------
   function On_Last_Arg return Boolean is
      L : constant Boolean := Arg_Counter = Ada.Command_Line.Argument_Count;
   begin
      -- Put_Line ("Last " & L'Image);
      return L;
   end On_Last_Arg;

   -- --------------------------------------------------------------------------
   function Current_Arg return String is
      S : constant String := Ada.Command_Line.Argument (Arg_Counter);
   begin
      -- Put_Line ("Current " & S'Image);
      return S;
   end Current_Arg;

   -- --------------------------------------------------------------------------
   function More_Args return Boolean is
      B : constant Boolean := Arg_Counter <= Ada.Command_Line.Argument_Count;
   begin
      -- Put_Line ("More_Arg " & B'Image);
      return B;
   end More_Args;

   -- --------------------------------------------------------------------------
   function Dash_To_Underscore (S : String) return String is
      Tmp           : String (S'Range);
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
   begin
      if Current_Command = None then
         Current_Command := C;
      else
         IO.Put_Error ("Cannot have both " & Current_Command'Image
                       & " and " & C'Image & " on command line");
      end if;
   end Set_Cmd;

   -- --------------------------------------------------------------------------
   procedure Put_Settings                           is separate;
   procedure Put_Help (Topic : Settings.Help_Topic) is separate;
   procedure Put_Trace_Topics                       is separate;
   procedure Create_Template                        is separate;

   -- --------------------------------------------------------------------------
   procedure Analyze is

      use BBT.IO,
          Ada.Directories;

   begin
      -- -----------------------------------------------------------------------
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

      Opt_Analysis_Loop : while More_Args loop

         declare
            -- arg are either a command or option, in which case we
            -- neutralize the '-' character to have
            -- --list_item = --list-item
            -- or a file name, in which case it remain.
            File : constant String := Current_Arg;
            Cmd  : constant String := Dash_To_Underscore (Current_Arg);
            use Tests.Filter_List;

         begin
            -- Commands --------------------------------------------------------
            if Cmd in "list" | "ls" then
               Set_Cmd (List);

            elsif Cmd = "run" then
               Set_Cmd (Run);

            elsif Cmd = "tt" then
               Set_Cmd (List_Trace_Topics);

            elsif Cmd in "help" | "he" | "-h" then
               Set_Cmd (Help);
               if not On_Last_Arg then
                  -- help may be followed by an help topic
                  Go_Next_Arg;
                  if Help_Topic'Valid_Value (Current_Arg) then
                     Settings.Current_Topic := Help_Topic'Value (Current_Arg);
                  else
                     -- Oups, next arg is not an help topic, let' go back
                     Go_Back_Previous_Arg;
                  end if;
               end if;

            elsif Cmd in "explain" | "ex" then
               Set_Cmd (Explain);

            elsif Cmd in "list_files" | "lf" then
               Set_Cmd (List_Files);

            elsif Cmd in "list_keywords" | "lk" then
               Set_Cmd (List_Keywords);

            elsif Cmd in "list_grammar" | "lg" then
               Set_Cmd (List_Grammar);

            elsif Cmd in "create_template" | "ct" then
               Set_Cmd (Create_Template);

               -- Options ------------------------------------------------------
            elsif Cmd = "-o" or Cmd = "--output" then
               if On_Last_Arg then
                  IO.Put_Error (Cmd & " must be followed by a file name");
               else
                  Go_Next_Arg;
                  Settings.Set_Result_File (Current_Arg);
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

            elsif Cmd = "-ed" or Cmd = "--exec_dir" then
               if On_Last_Arg then
                  IO.Put_Error (Cmd & " must be followed by a file name");
               else
                  Go_Next_Arg;
                  Settings.Set_Exec_Dir (Current_Arg);
               end if;

            elsif Cmd = "-td" or Cmd = "--tmp_dir" then
               if On_Last_Arg then
                  IO.Put_Error (Cmd & " must be followed by a file name");
               else
                  Go_Next_Arg;
                  Settings.Set_Tmp_Dir (Current_Arg);
               end if;

            elsif Cmd = "-r" or Cmd = "--recursive" then
               Settings.Recursive := True;

            elsif Cmd = "-k" or Cmd = "--keep_going" then
               Settings.Keep_Going := True;

            elsif Cmd = "-Werror" then
               Settings.Warnings_As_Errors := True;

            elsif Cmd = "-c" or Cmd = "--cleanup" then
               Settings.Cleanup := True;

            elsif Cmd = "--yes" then
               Settings.Yes := True;

            elsif Cmd = "-v" or Cmd = "--verbose" then
               IO.Set_Verbosity (Verbose);

            elsif Cmd = "-V" or Cmd = "--version" then
               Set_Cmd (Version);

            elsif Cmd = "-q" or Cmd = "--quiet" then
               IO.Set_Verbosity (Quiet);

            elsif Cmd = "--strict" then
               Settings.Strict_Gherkin := True;

            elsif Cmd = "-sb" or Cmd = "--status_bar" then
               Settings.Status_Bar := True;

            elsif Cmd = "-gb" or Cmd = "--generate_badge" then
               if On_Last_Arg then
                  IO.Put_Error (Cmd & " must be followed by a file name");
               else
                  Go_Next_Arg;
                  Settings.Generate_Badge := True;
                  Settings.Set_Badge_File_Name (Current_Arg);
               end if;

               -- Matching options ---------------------------------------------
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

               -- Debug options ------------------------------------------------
            elsif Cmd = "-d" then
               if On_Last_Arg then
                  IO.Put_Error ("-d may be followed by ");
                  for T in IO.Topics loop
                     IO.Put_Line (T'Image);
                  end loop;
               else
                  Go_Next_Arg;
                  if Topics'Valid_Value (Current_Arg)
                  then
                     -- -d is followed by the topic to watch
                     Enable_Topic (Topic => Topics'Value (Current_Arg));
                  else
                     -- there is only -d
                     Set_Verbosity (Debug);
                     Go_Back_Previous_Arg;
                  end if;
               end if;

            elsif Cmd = "-ls" then
               Settings.List_Settings := True;

            --  elsif Cmd = "-lt" then
            --     Put_Trace_Topics;

               -- Filtering options --------------------------------------------
            elsif Cmd in "-e" | "--exclude" then
               if On_Last_Arg then
                  IO.Put_Error (Cmd & " must be followed by a string");
               else
                  Go_Next_Arg;
                  Add_Filter (Pattern => Current_Arg,
                              Target  => Apply_To_All,
                              Mode    => Exclude);
               end if;

            elsif Cmd in "-i" | "--include" then
               if On_Last_Arg then
                  IO.Put_Error (Cmd & " must be followed by a string");
               else
                  Go_Next_Arg;
                  Add_Filter (Pattern => Current_Arg,
                              Target  => Apply_To_All,
                              Mode    => Include);
               end if;

            elsif Cmd in "-s" | "--select" then
               if On_Last_Arg then
                  IO.Put_Error (Cmd & " must be followed by a string");
               else
                  Go_Next_Arg;
                  Settings.Selection_Mode := True;
                  -- Set_Global_Mode (M => Selection);
                  Add_Filter (Pattern => Current_Arg,
                              Target  => Apply_To_All,
                              Mode    => Include);
               end if;

               -- File processing ----------------------------------------------
            elsif Ada.Directories.Exists (File) then
               -- if it's not an option, its a file name
               case Kind (File) is
               when Directory =>
                  Settings.No_File_Given := False;
                  Scenarios.Files.Find_Documents
                    (Dir       => File,
                     Recursive => Settings.Recursive);

               when Ordinary_File =>
                  Settings.No_File_Given := False;
                  Scenarios.Files.Add_Document (File);

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

         Go_Next_Arg;

      end loop Opt_Analysis_Loop;

      -- --------------------------------------------------------------------------
      if Debug_Mode then
         Put_Settings;
      end if;

   end Analyze;

end BBT.Cmd_Line;
