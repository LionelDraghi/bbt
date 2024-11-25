-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Main)

procedure Create_Template is
   Template  : File_Type;
   File_Name : String renames Settings.Template_Name;

begin
   if Ada.Directories.Exists (File_Name) then
      IO.Put_Error ("File " & File_Name & " already exists", IO.No_Location);

   else
      Create (Template, Name => Settings.Template_Name);
      Set_Output (Template);

      Put_Line ("## Feature : Command line");
      New_Line;
      Put_Line ("No special marks for comments, it's just normal markdown text.");
      New_Line;
      Put_Line ("### Scenario : version message");
      Put_Line ("  - When I run `sut --version`");
      Put_Line ("  - Then I get no error");
      Put_Line ("  - And I get `sut v0.1.0`");
      New_Line;
      Put_Line ("### Scenario : Help message");
      Put_Line ("  - When I run `sut --help`");
      Put_Line ("  - Then output contains");
      Put_Line ("```");
      Put_Line ("Return code:");
      Put_Line ("Return code is set to 1 when :");
      Put_Line ("- there is a command line error (unknown option for example)");
      Put_Line ("- there is a file error (unable to open the given file, for example)");
      Put_Line ("Return code is set to 0 otherwise.");
      Put_Line ("```");
      Put_Line ("  - And output contains `Usage:`");
      Put_Line ("  - And output contains `Errors:`");
      New_Line;
      Put_Line ("## Feature : File manipulation");
      New_Line;
      Put_Line ("### Scenario : append");
      Put_Line ("  - Given the `config.ini` file");
      Put_Line ("```");
      Put_Line ("verbose=false");
      Put_Line ("```");
      Put_Line ("  - When I successfully run `sut append lang=uk config.ini`");
      Put_Line ("  - Then I get");
      Put_Line ("```");
      Put_Line ("verbose=false");
      Put_Line ("lang=uk");
      Put_Line ("```");
      New_Line;
      Put_Line ("More extensive explanations : https://github.com/LionelDraghi/bbt/tree/main");
      New_Line;
      Put_Line ("File generated with BBT " & Settings.BBT_Version);

      Close (Template);
      Set_Output (Standard_Output);

      Put_Line ("Template file " & File_Name & " created.");

   end if;

end Create_Template;
