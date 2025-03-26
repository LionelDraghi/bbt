-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;

separate (BBT.Main)

procedure Put_Settings is

   -- --------------------------------------------------------------------------
   function Checkbox (Switch : Boolean) return String is
     (if Switch then "[X]" else "[ ]");

   use Ada.Text_IO;
   use Scenarios.Files;

begin
   New_Line;
   Put_Line ("Settings / Command line analysis:");
   Put_Line ("---------------------------------");
   New_Line;
   Put_Line ("   Command               : " & Current_Command'Image);
   Put_Line ("   Verbosity             : " & IO.Current_Verbosity'Image);
   Put_Line ("   bbt files             : " & One_Line_Image (Document_List));
   Put_Line ("   Initial directory     : " & Launch_Directory);
   Put_Line ("   Exec    directory     : " & Settings.Exec_Dir);
   Put_Line ("   Output file directory : " & Settings.Output_File_Dir);
   Put_Line ("   Result directory      : " & Result_Dir);
   Put_Line ("   Result file           : " & Result_File_Name);
   Put_Line ("   Template_Name         : " & Template_Name);
   New_Line;
   Put_Line ("   " & Checkbox (Keep_Going) & " Keep_Going");
   Put_Line ("   " & Checkbox (Ignore_Errors) & " Ignore_Errors");
   Put_Line ("   " & Checkbox (Warnings_As_Errors) & " Warnings_As_Errors");
   Put_Line ("   " & Checkbox (No_File_Given) & " No_File_Given");
   Put_Line ("   " & Checkbox (Recursive) & " Recursive");
   Put_Line ("   " & Checkbox (Ignore_Whitespaces) & " Ignore_Whitespaces");
   Put_Line ("   " & Checkbox (Ignore_Casing) & " Ignore_Casing");
   Put_Line ("   " & Checkbox (Ignore_Blank_Lines) & " Ignore_Blank_Lines");
   Put_Line ("   " & Checkbox (List_Files) & " List_Files");
   Put_Line ("   " & Checkbox (Yes) & " Yes");
   Put_Line ("   " & Checkbox (Settings.Cleanup) & " Cleanup");
   Put_Line ("   " & Checkbox (Settings.Strict_Gherkin) & " Strict_Gherkin");
   Put_Line ("   " & Checkbox (Settings.Status_Bar) & " Status_Bar");
   Put_Line ("   " & Checkbox (Settings.Generate_Badge) & " Generate_Badge");
   Put_Line ("   " & Checkbox (Settings.Selection_Mode) & " Selection Mode");
   --  Put_Line ("   Trace enabled for topics:");
   --  for T in IO.Extended_Topics loop
   --     Put_Line ("       " & Checkbox (IO.Is_Enabled (T))
   --               & " " & To_Lower (T'Image));
   --  end loop;
   New_Line;
   Put_Line ("---------------------------------");
   New_Line;

end Put_Settings;
