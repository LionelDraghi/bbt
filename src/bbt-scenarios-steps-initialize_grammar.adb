-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

procedure Initialize_Grammar (G : in out Grammar) is
   procedure Set (Prep       : Prepositions;
                  Sub_Attrib : Subject_Attrib;
                  Sub        : Subjects;
                  Verb       : Verbs;
                  Object     : Objects;
                  Items      : Grammar_Items) is
      Grammar_Error : exception;
   begin
      if G (Prep, Sub_Attrib, Sub, Verb, Object).Action /= None then
         raise Grammar_Error with "Grammar already initialized for " &
           Prep'Image & ", " & Sub_Attrib'Image & ", " & Sub'Image & ", " &
           Verb'Image & ", " & Object'Image;
      end if;
      G (Prep, Sub_Attrib, Sub, Verb, Object) := Items;
   end Set;

begin
   Set (Given, No_SA,  No_Subject,   Is_No,      Obj_File_Name, (Setup_No_File, False)); -- Given there is no `config.ini` file
   Set (Given, No_SA,  No_Subject,   Is_No,      Obj_Dir_Name,  (Setup_No_Dir, False));  -- Given there is no `dir1`       directory
   Set (Given, No_SA,  No_Subject,   Is_V,       Obj_File_Name, (Check_File_Existence, False)); -- Given there is a `config.ini` file
   Set (Given, No_SA,  No_Subject,   Is_V,       Obj_Dir_Name, (Check_Dir_Existence, False)); -- Given there is a `dir1` directory
   Set (Given, New_SA, Subject_File, Containing, Obj_Text,     (Erase_And_Create, False)); -- Given the new file `config.ini` containing `lang=it`
   Set (Given, No_SA,  Subject_File, Containing, Obj_Text,     (Create_If_None, False)); -- Given the file `config.ini` containing `lang=it`
                                                                                         -- Fixme: we currently do not check if the existing file contains
                                                                                         --  what is expected
   Set (Given, New_SA, Subject_File, Containing, No_Object, (Erase_And_Create, True)); -- Given the new file `config.ini` containing followed by code fenced content
   Set (Given, No_SA,  Subject_File, Containing, No_Object, (Create_If_None, True));   -- Given the file `config.ini` containing followed by code fenced content
                                                                                       -- Fixme : we currently do not check if the existing file contains
                                                                                       --  what is expected
   Set (Given, New_SA, Subject_File, No_Verb,    No_Object, (Erase_And_Create, True)); -- Given the new file `config.ini` followed by code fenced content
   Set (Given, No_SA,  Subject_File, No_Verb,    No_Object, (Create_If_None, True));   -- Given the file `config.ini` followed by code fenced content
   Set (Given, New_SA, Dir_Subject,  No_Verb,    No_Object, (Erase_And_Create, False)); -- Given the new directory `dir1`
   Set (Given, No_SA,  Dir_Subject,  No_Verb,    No_Object, (Create_If_None, False));   -- Given the directory `dir1`
   Set (Given, No_SA,  No_Subject,   Run,            Obj_Text, (Run_Cmd, False));           -- Given I run `cmd`
   Set (Given, No_SA,  No_Subject,   Successful_Run, Obj_Text, (Run_Without_Error, False)); -- Given i successfully run `cmd`

   Set (When_P, No_SA, No_Subject, Run,            Obj_Text, (Run_Cmd, False));               -- when I run `cmd`
   Set (When_P, No_SA, No_Subject, Successful_Run, Obj_Text, (Run_Without_Error, False));     -- when i successfully run `cmd`
   -- Set (When_P, No_SA, No_Subject, Run,            Command_List, (Run_Cmd, False))  ;         -- when I run `cmd` or `cmd2` or `cmd3`
   -- Set (When_P, No_SA, No_Subject, Successful_Run, Command_List, (Run_Without_Error, False)); -- when i successfully run `cmd` or `cmd2` or `cmd3`

   Set (Then_P, No_SA, No_Subject,   Is_V,     Obj_File_Name, (Check_File_Existence, False)); -- Then there is a  `config.ini` file
   Set (Then_P, No_SA, No_Subject,   Is_No,    Obj_File_Name, (Check_No_File, False));        -- Then there is no `config.ini` file
   Set (Then_P, No_SA, No_Subject,   Is_V,     Obj_Dir_Name, (Check_Dir_Existence, False)); -- Then there is a  `dir1` directory
   Set (Then_P, No_SA, No_Subject,   Is_No,    Obj_Dir_Name, (Check_No_Dir, False));        -- Then there is no `dir1` directory
   Set (Then_P, No_SA, No_Subject,   Get,      Error, (Error_Return_Code, False));    -- then I get error
   Set (Then_P, No_SA, No_Subject,   Get_No,   Error, (No_Error_Return_Code, False)); -- then I get no error
   Set (Then_P, No_SA, No_Subject,   Is_V,     Error, (Error_Return_Code, False));    -- then there is an error
   Set (Then_P, No_SA, No_Subject,   Is_No,    Error, (No_Error_Return_Code, False)); -- then there is no error
   Set (Then_P, No_SA, Output_Subj,  Is_V,     Obj_Text,      (Output_Is, False)); -- then output is `msg`
   Set (Then_P, No_SA, Output_Subj,  Is_V,     Obj_File_Name, (Output_Is, False)); -- then output is file `expected.txt`
   Set (Then_P, No_SA, Output_Subj,  Is_V,     No_Object,     (Output_Is, True));  -- then output is followed by code fenced content
   Set (Then_P, No_SA, No_Subject,   Get,      Obj_Text,      (Output_Is, False)); -- then I get `msg`
   Set (Then_P, No_SA, No_Subject,   Get,      Obj_File_Name, (Output_Is, False)); -- Then I get file `flowers2.txt`
   Set (Then_P, No_SA, No_Subject,   Get,      No_Object,     (Output_Is, True));  -- then I get followed by code fenced content
   Set (Then_P, No_SA, Output_Subj,  Contains, Obj_Text,      (Output_Contains, False)); -- then output contains `msg`
   Set (Then_P, No_SA, Output_Subj,  Contains, Obj_File_Name, (Output_Contains, False)); -- Then output contains `snippet.txt` file
   Set (Then_P, No_SA, Output_Subj,  Contains, No_Object,     (Output_Contains, True));  -- then output contains followed by code fenced content
   Set (Then_P, No_SA, Output_Subj,  Does_Not_Contain, Obj_Text,      (Output_Does_Not_Contain, False)); -- then output does not contain `msg`
   Set (Then_P, No_SA, Output_Subj,  Does_Not_Contain, Obj_File_Name, (Output_Does_Not_Contain, False)); -- Then output does not contain file `snippet.txt`
   Set (Then_P, No_SA, Output_Subj,  Does_Not_Contain, No_Object,     (Output_Does_Not_Contain, True));  -- then output does not contain followed by code fenced content
   Set (Then_P, No_SA, Output_Subj,  Matches,         Obj_Text, (Output_Matches, False));        -- then output matches `[:digit:]*.[:digit:]*`
   Set (Then_P, No_SA, Output_Subj,  Does_Not_Match,  Obj_Text, (Output_Does_Not_Match, False)); -- then output does not match `[:digit:]*.[:digit:]*`
   Set (Then_P, No_SA, Subject_File, Matches,         Obj_Text, (File_Matches, False));        -- then file `list` matches `*.string.*`
   Set (Then_P, No_SA, Subject_File, Does_Not_Match,  Obj_Text, (File_Does_Not_Match, False)); -- then file `list` does not match `*.string.*`
   Set (Then_P, No_SA, Subject_File, Is_V,     Obj_Text,      (File_Is, False));       -- then `config.ini` is `mode=silent`
   Set (Then_P, No_SA, Subject_File, Is_V,     Obj_File_Name, (File_Is, False));       -- then `config.ini` is equal to file `expected/config.ini`
   Set (Then_P, No_SA, Subject_File, Is_V,     No_Object,     (File_Is, True));        -- then `config.ini` is followed by code fenced content
   Set (Then_P, No_SA, Subject_File, Is_No,    Obj_File_Name, (File_Is_Not, False));   -- then `config.ini` is no more equal to file `previous_config.ini`
   Set (Then_P, No_SA, Subject_File, Contains, Obj_Text,      (File_Contains, False)); -- Then `config.ini` contains `--version`
   Set (Then_P, No_SA, Subject_File, Contains, Obj_File_Name, (File_Contains, False)); -- Then `config.ini` contains `snippet.txt` file
   Set (Then_P, No_SA, Subject_File, Contains, No_Object,     (File_Contains, True));  -- Then `config.ini` contains followed by code fenced content
   Set (Then_P, No_SA, Subject_File, Does_Not_Contain, Obj_Text,      (File_Does_Not_Contain, False)); -- Then `config.ini` does not contain `--version`
   Set (Then_P, No_SA, Subject_File, Does_Not_Contain, Obj_File_Name, (File_Does_Not_Contain, False)); -- Then `config.ini` does not contain `snippet.txt` file
   Set (Then_P, No_SA, Subject_File, Does_Not_Contain, No_Object,     (File_Does_Not_Contain, True));  -- Then `config.ini` does not contain followed by code fenced content
   Set (Then_P, No_SA, No_Subject,   Get_No,   Output_Obj,            (No_Output, False)); -- then I get no output
   Set (Then_P, No_SA, No_Subject,   Is_No,    Output_Obj,            (No_Output, False)); -- then there is no output
end Initialize_Grammar;