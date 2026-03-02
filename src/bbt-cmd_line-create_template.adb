-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Cmd_Line)

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
      New_Line;
      Put_Line ("This is a template bbt file, generated with BBT " & Settings.BBT_Version & "  ");
      Put_Line ("(It's also the shortest possible bbt tutorial!)");
      New_Line;
      Put_Line ("A bbt file contains at least :  ");
      Put_Line ("1. a Scenario header, that is a line starting with ""# Scenario : ""  ");
      Put_Line ("2. some Steps, that is lines starting with ""- Given"", ""- When"" or ""- Then""  ");
      New_Line;
      Put_Line ("Example :  ");
      New_Line;
      Put_Line ("    ## Scenario : getting gcc version  ");
      Put_Line ("    - When I run `gcc --version`  ");
      Put_Line ("    - Then I get `14.2.0`  ");
      New_Line;
      Put_Line ("## File Structure  ");
      New_Line;
      Put_Line ("[# Background] (at most one per file))");
      New_Line;
      Put_Line ("[# Feature] (any number of features per file)");
      New_Line;
      Put_Line ("[# Background] (at most one per Feature)");
      New_Line;
      Put_Line ("# Scenario 1 (any number of scenarios per feature)");
      Put_Line ("- Given/When/Then step");
      Put_Line ("[- Given/When/Then/And/But step] (any number of steps per scenario)");
      New_Line;
      Put_Line ("Example :  ");
      New_Line;
      Put_Line ("    # Feature : Case sensitivity control ");
      New_Line;
      Put_Line ("    ## Scenario : default behavior, no option  ");
      Put_Line ("    - When I run `grep xyz input.txt`  ");
      Put_Line ("    - Then ...  ");
      New_Line;
      Put_Line ("    ## Scenario : case insensitive search  ");
      Put_Line ("    - When I run `grep -i xyz input.txt`  ");
      Put_Line ("    - Then ...  ");
      New_Line;
      Put_Line ("### Background  ");
      New_Line;
      Put_Line ("Preconditions common to several scenarios may be put in a Background section, before scenarios :  ");
      New_Line;
      Put_Line ("    ### Background:  ");
      Put_Line ("    - Given there is no `input.txt` file  ");
      Put_Line ("    - Given there is a `tmp` dir  ");
      New_Line;
      Put_Line ("Background scope is logical : if it appears at the beginning of the file, it applys to all  ");
      Put_Line ("scenario in the file, if it appears at the beginning of a feature, it apply only  ");
      Put_Line ("to the scenarios of this feature.  ");
      Put_Line ("If there is both, Backgrounds are run in appearance order.  ");
      New_Line;
      Put_Line ("Example :  ");
      New_Line;
      Put_Line ("    ## Background 1 ");
      Put_Line ("    - Given there is no `config.ini` file  ");
      Put_Line ("    - Given ...  ");
      New_Line;
      Put_Line ("    # Feature A  ");
      New_Line;
      Put_Line ("    ## Scenario A.1  ");
      Put_Line ("    Background 1 will run here  ");
      Put_Line ("    - When I run `grep -i xyz input.txt`  ");
      Put_Line ("    - Then ...  ");
      New_Line;
      Put_Line ("    # Feature B  ");
      New_Line;
      Put_Line ("    ## Background 2 ");
      Put_Line ("    - Given ...  ");
      New_Line;
      Put_Line ("    ## Scenario B.1  ");
      Put_Line ("    Background 1 run here  ");
      Put_Line ("    Background 2 run here  ");
      Put_Line ("    - When ...  ");
      New_Line;
      Put_Line ("Note that the only headers reserved for bbt uses are ""Feature"", ""Scenario"" or ""Example"", and ""Background""  ");
      Put_Line ("(Example is a synonym for Scenario).  ");
      Put_Line ("Header level is not taken into account : `# Scenario` is equivalent to `#### Scenario`.  ");
      New_Line;
      Put_Line ("### Steps  ");
      New_Line;
      Put_Line ("Steps are the most important part of bbt files, they perform the actions and checks.  ");
      Put_Line ("- Given [setup condition]  ");
      Put_Line ("- When  [action to perform]  ");
      Put_Line ("- Then  [expected result]  ");
      New_Line;
      Put_Line ("Examples of steps:  ");
      New_Line;
      Put_Line ("    - Given there is no `.config` dir");
      Put_Line ("    - Given the `config.ini` file");
      Put_Line ("      ```");
      Put_Line ("      verbose=false");
      Put_Line ("      lang=am");
      Put_Line ("      ```");
      Put_Line ("    - Given the executable file `command.sh`");
      Put_Line ("      ```");
      Put_Line ("      #!/bin/bash");
      Put_Line ("      echo ""bbt rules!""");
      Put_Line ("      ```");
      Put_Line ("    - When I successfully run `xxx`");
      Put_Line ("      (Equivalent to both lines ""- When I run `xxx`"" and ""- Then I Get No Error"")");
      Put_Line ("    - Then there is no output");
      Put_Line ("    - Then I get no error");
      Put_Line ("    - Then output is `sut v0.1.0` (Equivalent ""Then I get..."")");
      New_Line;
      Put_Line ("You can continue a list of Given / When / Then with ""And"" or ""But"":  ");
      New_Line;
      Put_Line ("    - Then output contains `234 processed data`");
      Put_Line ("    - And  output contains `result = 29580`");
      Put_Line ("    - But  output doesn't contain `Warning:`");
      Put_Line ("    - And  output does not contain `Error:`  ");
      New_Line;
      Put_Line ("*And* and *But* are synonymous of the *Given* / *When* / *Then* that preceedes.  ");
      New_Line;
      Put_Line ("### Parameters  ");
      New_Line;
      Put_Line ("Parameters are given in three possible ways :  ");
      Put_Line ("  1. as a string:");
      New_Line;
      Put_Line ("    - Then I get `string`");
      New_Line;
      Put_Line ("  2. as a code fenced block:");
      New_Line;
      Put_Line ("    - Then I get");
      Put_Line ("    ```");
      Put_Line ("    This is my multi-line");
      Put_Line ("    file content");
      Put_Line ("    ```");
      New_Line;
      Put_Line ("  3. in an external file:");
      New_Line;
      Put_Line ("    - Then I get the content of file `expected.txt`  ");
      New_Line;
      Put_Line ("     Note in that case the mandatory ""file"" keyword  ");
      New_Line;
      Put_Line ("### Matching level  ");
      New_Line;
      Put_Line ("Above forms test that the output is exactly what is given.  ");
      Put_Line ("If what you want is just test that the output contains something, then use the ""contains"" keyword:  ");
      New_Line;
      Put_Line ("    - Then output contains `sut version v0.1.0`  ");
      New_Line;
      Put_Line ("If what you want is search for some pattern, then use the ""matches"" keyword, followed by a regexp :  ");
      New_Line;
      Put_Line ("    - Then output **matches** `sut version v[0-9]+\.[0-9]+\.[0-9]+`  ");
      New_Line;
      Put_Line ("Note that the regexp must match the entire line,");
      Put_Line ("don't forget to put "".*"" at the beginning or at the end if necessary.  ");
      New_Line;
      Put_Line ("### Other content of the file  ");
      New_Line;
      Put_Line ("bbt tries not to interfere with file editing and leaves as much flexibility as possible to the writer :  ");
      Put_Line ("- Headers other than Feature / Background / Scenario / Example are ignored by bbt  ");
      Put_Line ("  and there content treated as comment.  ");
      Put_Line ("- meaning that bullet point starting with Given / When / Then / And / But may be used outside of the  ");
      Put_Line ("  considered Headers.  ");
      Put_Line ("- Comments may apper between Header and Steps or even between Steps and code blocks.  ");
      New_Line;
      Put_Line ("## Help  ");
      New_Line;
      Put_Line ("To get a complete (although less friendly) view on the grammar:  ");
      New_Line;
      Put_Line ("    bbt list_grammar  ");
      New_Line;
      Put_Line ("To check your scenario with a dry run:  ");
      New_Line;
      Put_Line ("    bbt explain scenario.md  ");
      New_Line;
      Put_Line ("More features here : https://github.com/LionelDraghi/bbt/tree/main#bbt-readme-");

      Close (Template);
      Set_Output (Standard_Output);

      Put_Line ("Template file " & File_Name & " created.");

   end if;

end Create_Template;
