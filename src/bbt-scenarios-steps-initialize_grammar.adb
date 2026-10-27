-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

procedure Initialize_Grammar (G : in out Grammar) is

   type Preposition_Lists is array (Positive range <>) of Prepositions;

   procedure Set_One (Prep       : Prepositions;
                      Sub_Attrib : Subject_Attrib;
                      Sub        : Subjects;
                      Verb       : Verbs;
                      Object     : Objects;
                      Action     : Actions;
                      Code_Block : Boolean;
                      Example    : String) is
      --  State one grammar rule. The example string is stored prefixed
      --  with the preposition, so that a rule shared by several
      --  prepositions is stated only once.
      Grammar_Error : exception;
   begin
      if G (Prep, Sub_Attrib, Sub, Verb, Object).Action /= None then
         raise Grammar_Error with "Grammar already initialized for " &
           Prep'Image & ", " & Sub_Attrib'Image & ", " & Sub'Image & ", " &
           Verb'Image & ", " & Object'Image;
      end if;
      G (Prep, Sub_Attrib, Sub, Verb, Object) :=
        (Action,
         Code_Block,
         new String'("- " & Image (Prep) & " " & Example));
   end Set_One;

   procedure Set (Prep       : Prepositions;
                  Sub_Attrib : Subject_Attrib;
                  Sub        : Subjects;
                  Verb       : Verbs;
                  Object     : Objects;
                  Action     : Actions;
                  Example    : String;
                  Code_Block : Boolean := False) is
   begin
      Set_One (Prep, Sub_Attrib, Sub, Verb, Object, Action, Code_Block, Example);
   end Set;

   procedure Set (Preps      : Preposition_Lists;
                  Sub_Attrib : Subject_Attrib;
                  Sub        : Subjects;
                  Verb       : Verbs;
                  Object     : Objects;
                  Action     : Actions;
                  Example    : String;
                  Code_Block : Boolean := False) is
   begin
      for Prep of Preps loop
         Set_One (Prep, Sub_Attrib, Sub, Verb, Object, Action, Code_Block, Example);
      end loop;
   end Set;

begin
   --  All rules are stated in their positive form. Negated forms ("is
   --  not", "does not contain", "no"...) are not grammar entries: the
   --  negation words set the step polarity (Step_Data.Negated), valid
   --  for the negatable actions (cf. Negatable in BBT.Scenarios.Steps).
   --  The same for the run verb qualifiers: "shell" and "successfully"
   --  are not distinct verbs, they set flags, and the run action is
   --  derived from them in Parse. In the examples below, the
   --  [qualifier] notation states that the qualifier is optional.
   Set ([Given], No_SA,  No_Subject,   Is_V,      Obj_File_Name, Check_File_Existence, "there is a `config.ini` file");
   Set ([Given], No_SA,  No_Subject,   Is_V,      Obj_Dir_Name,  Check_Dir_Existence,  "there is a `dir1` directory");
   Set ([Given], New_SA, Subject_File, Containing, Obj_Text,     Erase_And_Create,     "the new file `config.ini` containing `lang=it`");
   Set ([Given], No_SA,  Subject_File, Containing, Obj_Text,     Create_If_None,       "the file `config.ini` containing `lang=it`");
   Set ([Given], New_SA, Subject_File, Containing, No_Object,    Erase_And_Create,     Code_Block => True, Example => "the new file `config.ini` containing <followed by code fenced lines>");
   Set ([Given], No_SA,  Subject_File, Containing, No_Object,    Create_If_None,       Code_Block => True, Example => "the file `config.ini` containing <followed by code fenced lines>");
   Set ([Given], New_SA, Subject_File, No_Verb,    No_Object,    Erase_And_Create,     Code_Block => True, Example => "the new file `config.ini` <followed by code fenced lines>");
   Set ([Given], No_SA,  Subject_File, No_Verb,    No_Object,    Create_If_None,       Code_Block => True, Example => "the file `config.ini` <followed by code fenced lines>");
   Set ([Given], New_SA, Dir_Subject,  No_Verb,    No_Object,    Erase_And_Create,     "the new directory `dir1`");
   Set ([Given], No_SA,  Dir_Subject,  No_Verb,    No_Object,    Create_If_None,       "the directory `dir1`");
   Set ([Given, When_P, Then_P], No_SA, No_Subject, Run, Obj_Text, Run_Cmd, "I [successfully] run [shell] `cmd`");
   Set ([Given, Then_P], No_SA,  Subject_Text, Fail,       No_Object,    Run_With_Error,       "`xmllint mismatched_tag.xml` fails");
   Set ([Given], No_SA,  Env_Var_Subj, Is_V,       Obj_Text,     Set_Env_Var,          "the environment variable `LANG` is `C`");
   Set ([Given], No_SA,  Env_Var_Subj, Is_V,       No_Object,    Set_Env_Var,          "the environment variable `NO_COLOR` is set");
   --  The negated form of the last rule ("is not set") unsets the
   --  variable (cf. Run_Step in BBT.Tests.Runner)

   Set ((When_P), No_SA, No_Subject, Run,            Command_List, Run_Cmd,           "I [successfully] run [shell] `cmd` or `cmd2` or `cmd3`");
   Set ((When_P), No_SA, No_Subject, Type_Text,      Obj_Text,     Type_Text,         "I type `Y`");
   Set ((When_P), No_SA, No_Subject, Enter_Text,     Obj_Text,     Enter_Text,        "I enter `Y`");

   Set ((Then_P), No_SA, No_Subject,   Is_V,     Obj_File_Name, Check_File_Existence, "there is a  `config.ini` file");
   Set ((Then_P), No_SA, No_Subject,   Is_V,     Obj_Dir_Name,  Check_Dir_Existence,  "there is a  `dir1` directory");
   Set ((Then_P), No_SA, No_Subject,   Get,      Error,         Error_Return_Code,    "I get error");
   Set ((Then_P), No_SA, No_Subject,   Is_V,     Error,         Error_Return_Code,    "there is an error");
   Set ((Then_P), No_SA, Output_Subj,  Is_V,     Obj_Text,      Output_Is,            "the output is `msg`");
   Set ((Then_P), No_SA, Output_Subj,  Is_V,     Obj_File_Name, Output_Is,            "the output is equal to file `expected.txt`");
   Set ((Then_P), No_SA, Output_Subj,  Is_V,     No_Object,     Output_Is,            Code_Block => True, Example => "the output is equal to <followed by code fenced lines>");
   Set ((Then_P), No_SA, No_Subject,   Get,      Obj_Text,      Output_Is,            "I get `msg`");
   Set ((Then_P), No_SA, No_Subject,   Get,      Obj_File_Name, Output_Is,            "I get file `flowers2.txt`");
   Set ((Then_P), No_SA, No_Subject,   Get,      No_Object,     Output_Is,            Code_Block => True, Example => "I get <followed by code fenced lines>");
   Set ((Then_P), No_SA, Output_Subj,  Contains, Obj_Text,      Output_Contains,      "the output contains `msg`");
   Set ((Then_P), No_SA, Output_Subj,  Contains, Obj_File_Name, Output_Contains,      "the output contains `snippet.txt` file");
   Set ((Then_P), No_SA, Output_Subj,  Contains, No_Object,     Output_Contains,      Code_Block => True, Example => "the output contains <followed by code fenced lines>");
   Set ((Then_P), No_SA, Output_Subj,  Matches,  Obj_Text,      Output_Matches,       "the output matches `[:digit:]*.[:digit:]*`");
   Set ((Then_P), No_SA, Subject_File, Matches,  Obj_Text,      File_Matches,         "the file `list` matches `*.string.*`");
   Set ((Then_P), No_SA, Subject_File, Is_V,     Obj_Text,      File_Is,              "the file `list` is `mode=silent`");
   Set ((Then_P), No_SA, Subject_File, Is_V,     Obj_File_Name, File_Is,              "the file `list` is equal to file `expected/list`");
   Set ((Then_P), No_SA, Subject_File, Is_V,     No_Object,     File_Is,              Code_Block => True, Example => "the file `list` is <followed by code fenced lines>");
   Set ((Then_P), No_SA, Subject_File, Contains, Obj_Text,      File_Contains,        "the file `list` contains `--version`");
   Set ((Then_P), No_SA, Subject_File, Contains, Obj_File_Name, File_Contains,        "the file `list` contains `snippet.txt` file");
   Set ((Then_P), No_SA, Subject_File, Contains, No_Object,     File_Contains,        Code_Block => True, Example => "the file `list` contains <followed by code fenced lines>");
   Set ((Then_P), No_SA, Stderr_Subj,  Is_V,     Obj_Text,      Stderr_Is,            "the error output is `msg`");
   Set ((Then_P), No_SA, Stderr_Subj,  Is_V,     Obj_File_Name, Stderr_Is,            "the error output is equal to file `expected.txt`");
   Set ((Then_P), No_SA, Stderr_Subj,  Is_V,     No_Object,     Stderr_Is,            Code_Block => True, Example => "the error output is <followed by code fenced lines>");
   Set ((Then_P), No_SA, Stderr_Subj,  Contains, Obj_Text,      Stderr_Contains,      "the error output contains `msg`");
   Set ((Then_P), No_SA, Stderr_Subj,  Contains, Obj_File_Name, Stderr_Contains,      "the error output contains file `snippet.txt`");
   Set ((Then_P), No_SA, Stderr_Subj,  Contains, No_Object,     Stderr_Contains,      Code_Block => True, Example => "the error output contains <followed by code fenced lines>");
   Set ((Then_P), No_SA, No_Subject,   Is_V,     Stderr_Obj,    Stderr_Not_Empty,     "there is error output");
   Set ((Then_P), No_SA, No_Subject,   Get,      Stderr_Obj,    Stderr_Not_Empty,     "I get error output");
   Set ((Then_P), No_SA, No_Subject,   Is_V,     Output_Obj,    Output_Not_Empty,     "there is output");
   Set ((Then_P), No_SA, No_Subject,   Get,      Output_Obj,    Output_Not_Empty,     "I get output");
   Set ((Then_P), No_SA, Exit_Subj,    Is_V,     Obj_Text,      Exit_Code_Is,         "the exit code is `2`");

end Initialize_Grammar;
