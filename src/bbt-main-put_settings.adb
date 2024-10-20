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

   use Settings;
   use Ada.Text_IO;
   use Ada.Characters.Handling;

begin
   New_Line;
   Put_Line ("Settings / Command line analysis:");
   Put_Line ("---------------------------------");
   New_Line;
   Put_Line ("   Verbosity             : " & IO.Current_Verbosity'Image);
   Put_Line ("   bbt files             : " & Scenarios.Files.BBT_Files'Image);
   Put_Line ("   Initial directory     : " & Launch_Directory);
   Put_Line ("   Exec    directory     : " & Settings.Exec_Dir);
   Put_Line ("   Output file directory : " & Settings.Output_File_Dir);
   Put_Line ("   Result file           : " & Result_File_Name);
   Put_Line ("   Template_Name         : " & Template_Name);
   New_Line;
   Put_Line ("   " & Checkbox (Explain) & " Explain");
   Put_Line ("   " & Checkbox (Yes) & " Yes");
   Put_Line ("   " & Checkbox (No_File_Given) & " No_File_Given");
   Put_Line ("   " & Checkbox (Keep_Going) & " Keep_Going");
   Put_Line ("   " & Checkbox (List_Topics) & " List_Topics");
   Put_Line ("   " & Checkbox (List_Keywords) & " List_Keywords");
   Put_Line ("   " & Checkbox (List_Grammar) & " List_Grammar");
   Put_Line ("   " & Checkbox (Ignore_Errors) & " Ignore_Errors");
   Put_Line ("   " & Checkbox (Recursive) & " Recursive");
   Put_Line ("   " & Checkbox (Warnings_As_Errors) & " Warnings_As_Errors");
   Put_Line ("   " & Checkbox (Settings.Create_Template)
             & " Create_Template");
   Put_Line ("   Trace enabled for topics:");
   for T in IO.Topics loop
      Put_Line ("     " & Checkbox (IO.Is_Enabled (T))
                & " " & To_Lower (T'Image));
   end loop;
   New_Line;
   Put_Line ("---------------------------------");
   New_Line;

end Put_Settings;
