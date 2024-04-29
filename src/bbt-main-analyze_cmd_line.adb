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

with BBT.IO;
with BBT.Settings;

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

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
   procedure Put_Settings is
      function Checkbox (Switch : Boolean) return String is
        (if Switch then "[X]" else "[ ]");
      use Settings;
   begin
      IO.New_Line;
      Put_Line ("Settings / Command line analysis:");
      Put_Line ("---------------------------------");
      IO.New_Line;
      Put_Line ("   Verbosity         : "
                   & Print_Out_Level'Image (Verbosity));
      Put_Line ("   bbt files         : "
                   & Files.BBT_Files'Image);
      Put_Line ("   Initial directory : " & Initial_Directory);
      IO.New_Line;
      Put_Line ("   " & Checkbox (Build_Missing_Targets)
                   & " Build_Missing_Targets");
      Put_Line ("   " & Checkbox (Always_Make) & " Always_Make");
      Put_Line ("   " & Checkbox (Explain) & " Explain");
      Put_Line ("   " & Checkbox (Dry_Run) & " Dry_Run");
      Put_Line ("   " & Checkbox (Keep_Going) & " Keep_Going");
      Put_Line ("   " & Checkbox (Ignore_Errors) & " Ignore_Errors");
      Put_Line ("   " & Checkbox (Long_Listing_Format)
                   & " Long_Listing_Format");
      Put_Line ("   " & Checkbox (Recursive) & " Recursive");
      Put_Line ("   " & Checkbox (Warnings_As_Errors)
                   & " Warnings_As_Errors");
      IO.New_Line;
      Put_Line ("---------------------------------");
      IO.New_Line;
   end Put_Settings;

begin
   -- --------------------------------------------------------------------------
   -- NB: command line, including arguments should comply with GNU Coding
   -- standards
   -- (https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
   Opt_Analyzis_Loop : while Arg_Counter <= Ada.Command_Line.Argument_Count loop

      declare
         Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

      begin
         if  Opt = "-h" or Opt = "--help" then
            Settings.Help_Needed := True;
            return;

         elsif Opt = "-e" or Opt = "--explain" then
            Settings.Explain := True;

         elsif Opt = "-lf" or Opt = "--list-files" then
            Settings.List_Files := True;

         elsif Opt = "-wc" or Opt = "--with-comments" then
            Settings.With_Comments := True;

         elsif Opt = "-bk" or Opt = "--bold-keywords" then
            Settings.With_Bold_Keywords := True;

         elsif Opt = "-ct" or Opt = "--create-template" then
            Settings.Create_Template := True;

         elsif Opt = "-n" or Opt = "--dry-run" then
            Settings.Dry_Run := True;

         elsif Opt = "-k" or Opt = "--keep-going" then
            Settings.Keep_Going := True;

         elsif Opt = "-r" or Opt = "--recursive" then
            Settings.Recursive := True;

         elsif Opt = "-v" or Opt = "--verbose" then
            Settings.Verbosity := Settings.Verbose;

         elsif Opt = "-q" or Opt = "--quiet" then
            Settings.Verbosity := Settings.Quiet;

         elsif Opt = "-d" then
            -- undocumented option
            Settings.Verbosity := Settings.Debug;

         elsif Opt = "-lt" then
            -- undocumented option, list topics
            Put_Line ("Available topics :");
            for T in Settings.Topics loop
                  Put_Line ("- " & To_Lower (T'Image));
            end loop;

         elsif Opt = "-t" then
            -- undocumented option, print topics
            declare
               Topic : constant String :=
                         Ada.Command_Line.Argument (Arg_Counter + 1);
               use Settings;
            begin
               Settings.Enable_Topic (Topics'Value (Topic));
               Put_Line ("Enabling trace on topic " & Topic);
               Next_Arg;
            end;

         else
            -- if it's not an option, its a file name
            if Ada.Directories.Exists (Opt) then
               Settings.No_File_Given := False;
               Files.Append (Opt);
            else
               Put_Error ("Unknown bbt file """ & Opt & """",
                          With_Help => False);
               return;
            end if;

         end if;

         if IO.Some_Error then return; end if;
         -- No need to further analyze command line, or to do
         -- Options_Coherency_Tests.
      end;

      Next_Arg;

   end loop Opt_Analyzis_Loop;

   -- --------------------------------------------------------------------------
   if Settings.Debug_Mode then
      Put_Settings;
   end if;

end Analyze_Cmd_Line;
