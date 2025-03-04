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

      New_Line;
      Put_Line ("## Introduction  ");
      Put_Line ("This is a template bbt file, generated with BBT " & Settings.BBT_Version & "  ");
      Put_Line ("(It's also the shortest possible bbt tutorial!)");
      New_Line;
      Put_Line ("A bbt file contains at least :  ");
      Put_Line ("1. a Scenario header, that is a line starting with ""# Scenario : ""  ");
      Put_Line ("2. some Steps, that is lines starting with ""- Given"", ""- When"" or ""- Then""  ");
      New_Line;
      Put_Line ("Minimal example :  ");
      Put_Line ("## Scenario : getting gcc version  ");
      Put_Line ("- When I run `gcc --version`  ");
      Put_Line ("- Then I get `14.2.0`  ");
      New_Line;
      Put_Line ("## Structure of the file  ");
      Put_Line ("Several scenarios may be organized in ""Feature"".  ");
      Put_Line ("Note that the only headers reserved for bbt uses are ""Feature"", ""Scenario"" or ""Example"", and ""Background""  ");
      Put_Line ("Other header will be ignored by bbt.  ");
      New_Line;
      Put_Line ("## Steps  ");
      Put_Line ("Steps are the most important part of bbt files.  ");
      Put_Line ("  ""Given"" steps put the system in a known state  ");
      Put_Line ("  ""When""  steps run defined actions  ");
      Put_Line ("  ""Then""  steps observes outcomes  ");
      New_Line;
      Put_Line ("Examples of steps:  ");
      Put_Line ("  - Given there is no `.config` dir");
      Put_Line ("  - Given the `config.ini` file");
      Put_Line ("    ```");
      Put_Line ("    verbose=false");
      Put_Line ("    lang=am");
      Put_Line ("    ```");
      Put_Line ("  - Given the executable file `command.sh`");
      Put_Line ("    ```");
      Put_Line ("    #!/bin/bash");
      Put_Line ("    echo ""bbt rules!""");
      Put_Line ("    ```");
      Put_Line ("  - When I successfully run `xxx` (Equivalent to both lines ""- When I run `xxx`"" and ""- Then I Get No Error""");
      Put_Line ("  - Then there is no output");
      Put_Line ("  - Then I get no error");
      Put_Line ("  - Then I get an error");
      Put_Line ("  - Then output is `sut v0.1.0` (Equivalent ""Then I get"")");
      New_Line;
      Put_Line ("You can continue a list of Given / When / Then with ""And"" or ""But"":  ");
      Put_Line ("  - Then output contains `234 processed data`");
      Put_Line ("  - And  output contains `result = 29580`");
      Put_Line ("  - But  output doesn't contain `Warning:`");
      Put_Line ("  - And  output does not contain `Error:`  ");
      New_Line;
      Put_Line ("## Expected output  ");
      Put_Line ("Expected output is given in three possible ways :  ");
      Put_Line ("  1. as a string:");
      Put_Line ("     > - Then I get `string`");
      Put_Line ("  2. as a code fenced block:");
      Put_Line ("     > - Then I get");
      Put_Line ("     ```");
      Put_Line ("     This is my multi-line");
      Put_Line ("     file content");
      Put_Line ("     ```");
      Put_Line ("  3. in an external file:");
      Put_Line ("     > - Then I get file `expected.txt`  ");
      New_Line;
      Put_Line ("     Note in that case the mandatory ""file"" keyword  ");
      New_Line;
      Put_Line ("Above forms test that the output is exactly what is given.  ");
      Put_Line ("If what you want is just test that the output contains something, then use the ""contains"" keyword:  ");
      Put_Line ("  > - Then output contains `sut version v0.1.0`  ");
      New_Line;
      Put_Line ("If what you want is search for some pattern, then use the ""matches"" keyword, followed by a regexp :  ");
      Put_Line ("  > - Then output **matches** `sut version [0-9]+\.[0-9]+\.[0-9]+`  ");
      New_Line;
      Put_Line ("Note that the regexp must match the entire line,");
      Put_Line ("don't forget to put "".*"" at the beginning or at the end if necessary.  ");
      New_Line;
      Put_Line ("## Background  ");
      Put_Line ("Preconditions common to several scenarios may be put in a Background section, before scenarios :  ");
      Put_Line ("> ### Background:  ");
      Put_Line (">   - Given there is no `input.txt` file  ");
      Put_Line (">   - Given there is a `tmp` dir  ");
      New_Line;
      Put_Line ("## Help  ");
      Put_Line ("To get a complete (although less friendly) view on the grammar:  ");
      Put_Line ("> bbt -lg  ");
      New_Line;
      Put_Line ("And the to get the list of keywords:  ");
      Put_Line ("> bbt -lk  ");
      New_Line;
      Put_Line ("More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-");

      Close (Template);
      Set_Output (Standard_Output);

      Put_Line ("Template file " & File_Name & " created.");

   end if;

end Create_Template;
