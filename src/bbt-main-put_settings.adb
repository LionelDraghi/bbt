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
   Put_Line ("   Verbosity         : "
             & Print_Out_Level'Image (Verbosity));
   Put_Line ("   bbt files         : "
             & Scenario_Files.BBT_Files'Image);
   Put_Line ("   Initial directory : " & Initial_Directory);
   if Get_Output_File_Name = "" then
      Put_Line ("   Output file       : " & Get_Output_File_Name);
   end if;
   Put_Line ("   Template_Name     : " & Template_Name);
   New_Line;
   Put_Line ("   " & Checkbox (Explain)
             & " Explain");
   Put_Line ("   " & Checkbox (No_File_Given) & " No_File_Given");
   Put_Line ("   " & Checkbox (Explain) & " Explain");
   Put_Line ("   " & Checkbox (Dry_Run) & " Dry_Run");
   Put_Line ("   " & Checkbox (Keep_Going) & " Keep_Going");
   Put_Line ("   " & Checkbox (Ignore_Errors) & " Ignore_Errors");
   Put_Line ("   " & Checkbox (With_Comments)
             & " With_Comments");
   Put_Line ("   " & Checkbox (With_Bold_Keywords)
             & " With_Bold_Keywords");
   Put_Line ("   " & Checkbox (Recursive) & " Recursive");
   Put_Line ("   " & Checkbox (Warnings_As_Errors)
             & " Warnings_As_Errors");
   Put_Line ("   " & Checkbox (Settings.Create_Template)
             & " Create_Template");
   Put_Line ("   Trace enabled for topics:");
   for T in Settings.Topics loop
      Put_Line ("     " & Checkbox (Is_Enabled (T)) & " " & To_Lower (T'Image));
   end loop;
   New_Line;
   Put_Line ("---------------------------------");
   New_Line;

end Put_Settings;
