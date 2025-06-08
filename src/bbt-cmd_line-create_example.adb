-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Cmd_Line)

procedure Create_Example is
   Template  : File_Type;
   File_Name : String renames Settings.Template_Name;

begin
   if Ada.Directories.Exists (File_Name) then
      IO.Put_Error ("File " & File_Name & " already exists", IO.No_Location);

   else
      Create (Template, Name => Settings.Template_Name);
      Set_Output (Template);

      New_Line;
      Put_Line ("## Scenario : getting gcc version  ");
      Put_Line ("- When I run `gcc --version`  ");
      Put_Line ("- Then I get `14.2.0`  ");
      New_Line;

      Close (Template);
      Set_Output (Standard_Output);

      Put_Line ("Template file " & File_Name & " created.");

   end if;

end Create_Example;
