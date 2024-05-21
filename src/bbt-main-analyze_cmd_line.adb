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

with BBT.IO; use BBT.IO;
with BBT.Settings;

with Ada.Characters.Handling; use Ada.Characters.Handling;
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
   Opt_Analysis_Loop : while Arg_Counter <= Ada.Command_Line.Argument_Count loop

      declare
         Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);
         use Settings;

      begin
         -- Commands -----------------------------------------------------------
         if  Opt = "-h" or Opt = "help" then
            Settings.Help_Needed := True;
            return;

         elsif Opt = "-e" or Opt = "--explain" then
            Settings.Explain := True;

         elsif Opt = "-lf" or Opt = "--list_files" then
            Settings.List_Files := True;

         elsif Opt = "-lk" or Opt = "--list_keywords" then
            Settings.List_Keywords := True;

         elsif Opt = "-lg" or Opt = "--list_grammar" then
            Settings.List_Grammar := True;

         elsif Opt = "-ct" or Opt = "--create_template" then
            Settings.Create_Template := True;

            -- Options ---------------------------------------------------------
         elsif Opt = "-o" or Opt = "--output" then
            Next_Arg;
            Settings.Set_Output_File (Ada.Command_Line.Argument (Arg_Counter));
            IO.Enable_Tee (Settings.Get_Output_File_Name);

            --  elsif Opt = "-n" or Opt = "--dry-run" then
            --     Settings.Dry_Run := True;

         elsif Opt = "-r" or Opt = "--recursive" then
            Settings.Recursive := True;

         elsif Opt = "-k" or Opt = "--keep_going" then
            Settings.Keep_Going := True;

         elsif Opt = "-y" or Opt = "--assume_yes" then
            Settings.Assume_Yes := True;

         elsif Opt = "-v" or Opt = "--verbose" then
            Set_Verbosity  (Verbose);

         elsif Opt = "-q" or Opt = "--quiet" then
            Set_Verbosity  (Quiet);

         elsif Opt = "-d" then
            -- undocumented option
            Set_Verbosity  (Debug);

         elsif Opt = "-ls" then
            -- undocumented option, list settings
            Settings.List_Settings := True;

         elsif Ada.Directories.Exists (Opt) then
               -- if it's not an option, its a file name
               case Kind (Opt) is
                  when Directory =>
                     Settings.No_File_Given := False;
                     Scenarios.Files.Find_BBT_Files
                       (Start_In  => Opt,
                        Recursive => Settings.Recursive);
                  when Ordinary_File =>
                     Settings.No_File_Given := False;
                     Scenarios.Files.Append_File (Opt);

                  when Special_File =>
                     IO.Put_Error ("Unknown file type """ & Opt & """");
                     return;

               end case;

         else
            IO.Put_Error ("Unknown option """ & Opt & """");

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
