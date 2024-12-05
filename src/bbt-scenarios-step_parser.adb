-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Scenarios.Step_Parser.Lexer;

with Text_Utilities; use Text_Utilities;

with Ada.Containers;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Strings.Maps;

package body BBT.Scenarios.Step_Parser is

   use BBT.Scenarios.Step_Parser.Lexer;

   type Tokens is ( -- Prepositions -----------------------------------------
                    Given,
                    When_P,
                    Then_P,
                    -- Subjects Attribute -----------------------------------
                    No_SA, -- = no Subject Attribute
                    -- Empty,
                    -- Existing,
                    New_SA,
                    -- Subjects ---------------------------------------------
                    No_Subject,
                    Subject_Dir,  -- dir name
                    Subject_File, -- file name
                    Subject_Text, -- content of code span or following
                    --               code fenced lines, before verb
                    Output_Subj,
                    -- Verbs ------------------------------------------------
                    No_Verb,
                    Run,
                    Successful_Run,
                    Get,
                    Get_No,
                    Does_Not_Contain,
                    Contains,
                    Containing,
                    Is_V,
                    Is_No,
                    -- Objects ----------------------------------------------
                    No_Object,
                    Output_Obj,
                    Object_File,
                    Object_Dir,  -- file or dir name
                    Object_Text, -- content of code span or following
                    --              code fenced lines, before verb
                    Command_List,
                    Error,
                    -- Adjectives
                    Unordered);

   subtype Adjectives     is Tokens range Unordered    .. Tokens'Last;
   subtype Objects        is Tokens range No_Object    .. Tokens'Pred (Adjectives'First);
   subtype Verbs          is Tokens range No_Verb      .. Tokens'Pred (Objects'First);
   subtype Subjects       is Tokens range No_Subject   .. Tokens'Pred (Verbs'First);
   subtype Subject_Attrib is Tokens range No_SA        .. Tokens'Pred (Subjects'First);
   subtype Prepositions   is Tokens range Tokens'First .. Tokens'Pred (Subject_Attrib'First);

   -- -----------------------------------------------------------------------
   function Image (T : Tokens) return String is
   begin
      case T is
         when Given            => return "Given";
         when When_P           => return "When";
         when Then_P           => return "Then";
         when No_SA            => return "";
         when New_SA           => return "new";
         when No_Subject       => return "";
         when Output_Subj      => return "output";
         when Subject_File     => return "`file`";
         when Subject_Dir      => return "`dir`";
         when Subject_Text     => return "`text`";
         when No_Verb          => return "";
         when Run              => return "run";
         when Successful_Run   => return "successfully run";
         when Get              => return "get";
         when Get_No           => return "get no";
         when Contains         => return "contains";
         when Containing       => return "containing";
         when Does_Not_Contain => return "does not contain";
         when Is_V             => return "is";
         when Is_No            => return "is no";
         when No_Object        => return "";
         when Output_Obj       => return "output";
         when Object_File      => return "`file`";
         when Object_Dir       => return "`dir`";
         when Object_Text      => return "`text`";
         when Command_List     => return "`cmd` [or `cmd`]*";
         when Error            => return "error";
         when Unordered        => return "unordered";
      end case;
   end Image;

   type Grammar is array (Prepositions, Subject_Attrib,
                          Subjects, Verbs, Objects) of Actions;
   Null_Grammar : constant Grammar
     := [others => [others => [others => [others => [others => None]]]]];

   -- -----------------------------------------------------------------------
   function Create_Grammar return Grammar is
      G : Grammar := Null_Grammar;
   begin
      G (Given, No_SA,  No_Subject,   Is_No,      Object_File) := Setup_No_File; -- Given there is no `config.ini` file
      G (Given, No_SA,  No_Subject,   Is_No,      Object_Dir)  := Setup_No_Dir;  -- Given there is no `dir1`       directory
      G (Given, No_SA,  No_Subject,   Is_V,       Object_File) := Check_File_Existence; -- Given there is a `config.ini` file
      G (Given, No_SA,  No_Subject,   Is_V,       Object_Dir)  := Check_Dir_Existence;  -- Given there is a `dir1` directory
      G (Given, New_SA, Subject_File, Containing, Object_Text) := Erase_And_Create; -- Given the new file `config.ini` containing `lang=it`
      G (Given, No_SA,  Subject_File, Containing, Object_Text) := Create_File;      -- Given the file `config.ini` containing `lang=it`
      G (Given, New_SA, Subject_File, No_Verb,    No_Object)   := Erase_And_Create; -- Given the new file `config.ini` followed by code fenced content
      G (Given, No_SA,  Subject_File, No_Verb,    No_Object)   := Create_File;      -- Given the file `config.ini` followed by code fenced content
      G (Given, New_SA, Subject_Dir,  No_Verb,    No_Object)   := Erase_And_Create; -- Given the new directory `dir1`
      G (Given, No_SA,  Subject_Dir,  No_Verb,    No_Object)   := Create_Directory; -- Given the directory `dir1`

      G (When_P, No_SA, No_Subject, Run,            Object_Text)  := Run_Cmd;           -- when I run `cmd`
      G (When_P, No_SA, No_Subject, Successful_Run, Object_Text)  := Run_Without_Error; -- when i successfully run `cmd`
      G (When_P, No_SA, No_Subject, Run,            Command_List) := Run_Cmd;           -- when I run `cmd` or `cmd2` or `cmd3`
      G (When_P, No_SA, No_Subject, Successful_Run, Command_List) := Run_Without_Error; -- when i successfully run `cmd` or `cmd2` or `cmd3`

      G (Then_P, No_SA, No_Subject,   Is_V,     Object_File) := Check_File_Existence; -- Then there is a  `config.ini` file
      G (Then_P, No_SA, No_Subject,   Is_No,    Object_File) := Check_No_File;        -- Then there is no `config.ini` file
      G (Then_P, No_SA, No_Subject,   Is_V,     Object_Dir)  := Check_Dir_Existence; -- Then there is a  `dir1` directory
      G (Then_P, No_SA, No_Subject,   Is_No,    Object_Dir)  := Check_No_Dir;        -- Then there is no `dir1` directory
      G (Then_P, No_SA, No_Subject,   Get,      Error)       := Error_Return_Code;    -- then I get error
      G (Then_P, No_SA, No_Subject,   Get_No,   Error)       := No_Error_Return_Code; -- then I get no error
      G (Then_P, No_SA, No_Subject,   Is_V,     Error)       := Error_Return_Code;    -- then there is an error
      G (Then_P, No_SA, No_Subject,   Is_No,    Error)       := No_Error_Return_Code; -- then there is no error
      G (Then_P, No_SA, Output_Subj,  Is_V,     Object_Text) := Output_Is; -- then output is `msg`
      G (Then_P, No_SA, Output_Subj,  Is_V,     Object_File) := Output_Is; -- then output is `expected.txt`
      G (Then_P, No_SA, Output_Subj,  Is_V,     No_Object)   := Output_Is; -- then output is followed by code fenced content
      G (Then_P, No_SA, No_Subject,   Get,      Object_Text) := Output_Is; -- then I get `msg`
      G (Then_P, No_SA, No_Subject,   Get,      No_Object)   := Output_Is; -- then I get followed by code fenced content
      G (Then_P, No_SA, Output_Subj,  Contains, Object_Text) := Output_Contains; -- then output contains `msg`
      G (Then_P, No_SA, Output_Subj,  Contains, No_Object)   := Output_Contains; -- then output contains followed by code fenced content
      G (Then_P, No_SA, Output_Subj,  Does_Not_Contain, Object_Text) := Output_Does_Not_Contain; -- then output does not contain `msg`
      G (Then_P, No_SA, Output_Subj,  Does_Not_Contain, No_Object)   := Output_Does_Not_Contain; -- then output does not contain followed by code fenced content
      G (Then_P, No_SA, Subject_File, Is_V,     Object_Text) := File_Is; -- then `config.ini` is `mode=silent`
      G (Then_P, No_SA, Subject_File, Is_V,     No_Object)   := File_Is; -- Then `config.ini` is followed by code fenced content
      G (Then_P, No_SA, Subject_File, Is_V,     Object_File) := File_Is; -- then `config.ini` is equal to file `expected/config.ini`
      G (Then_P, No_SA, Subject_File, Contains, Object_Text) := File_Contains; -- Then `config.ini` contains `--version`
      G (Then_P, No_SA, Subject_File, Contains, No_Object)   := File_Contains; -- Then `config.ini` contains followed by code fenced content
      G (Then_P, No_SA, Subject_File, Does_Not_Contain, Object_Text) := File_Does_Not_Contain; -- Then `config.ini` contains `--version`
      G (Then_P, No_SA, Subject_File, Does_Not_Contain, No_Object)   := File_Does_Not_Contain; -- Then `config.ini` contains followed by code fenced content
      G (Then_P, No_SA, No_Subject,   Get_No,   Output_Obj)  := No_Output; -- then I get no output
      G (Then_P, No_SA, No_Subject,   Is_No,    Output_Obj)  := No_Output; -- then there is no output
      return G;
   end Create_Grammar;

   The_Grammar : constant Grammar := Create_Grammar;

   -- -----------------------------------------------------------------------
   function Get_Action (P  : Prepositions;
                        SA : Subject_Attrib;
                        S  : Subjects;
                        V  : Verbs;
                        O  : Objects) return Actions
   is (The_Grammar (P, SA, S, V, O));

   -- -----------------------------------------------------------------------
   procedure Put_Rule (P         : Prepositions;
                       SA        : Subject_Attrib;
                       S         : Subjects;
                       V         : Verbs;
                       O         : Objects;
                       A         : Actions;
                       Verbosity : IO.Verbosity_Levels := IO.Normal)
   is
      use Ada.Text_IO;
      C : Positive_Count := Col;
   begin
      if Is_Authorized (Verbosity) then
         Ada.Text_IO.Put  ("| " & Image (P));  C := @ + Prepositions'Width + 1; Set_Col (C);
         Ada.Text_IO.Put (" | " & Image (SA)); C := @ + Subject_Attrib'Width; Set_Col (C);
         Ada.Text_IO.Put (" | " & Image (S));  C := @ + Subjects'Width - 3; Set_Col (C);
         Ada.Text_IO.Put (" | " & Image (V));  C := @ + Verbs'Width + 5; Set_Col (C);
         Ada.Text_IO.Put (" | " & Image (O));  C := @ + Objects'Width + 8; Set_Col (C);
         Ada.Text_IO.Put (" | " & A'Image);    C := @ + Actions'Width + 3; Set_Col (C);
         Ada.Text_IO.Put_Line (" |  ");
      end if;
   end Put_Rule;

   Context : Extended_Step_Categories;
   -- Some line are hard to interpret without the context:
   -- line that starts with "And" for example will inherit their type from
   -- the previously analyzed lines.
   -- This variable keep the memory of where we are between call Parse.

   -- --------------------------------------------------------------------------
   function Parse (Line     :     Unbounded_String;
                   Loc      :     Location_Type;
                   Cmd_List : out Cmd_Lists.Vector) return Step_Type
   is
      First_Token      : Boolean        := True;
      Successfully_Met : Boolean        := False;
      Or_Met           : Natural        := 0;
      Not_Met          : Boolean        := False;
      Prep             : Prepositions;
      Subject_Attr     : Subject_Attrib := No_SA;
      Subject          : Subjects       := No_Subject;
      Verb             : Verbs          := No_Verb;
      Object           : Objects        := No_Object;

      -- All component of the returned Step are initialized :
      Cat              : Extended_Step_Categories := Unknown;
      Action           : Actions                  := None;
      Step_String      : Unbounded_String         := Null_Unbounded_String;
      Subject_String   : Unbounded_String         := Null_Unbounded_String;
      Object_String    : Unbounded_String         := Null_Unbounded_String;
      Ignore_Order     : Boolean                  := False;
      -- by default, order of expected output is significant

      File_Type        : File_Kind                := Ordinary_File;

      Prefix : constant String := Image (Loc);

      function In_Subject_Part return Boolean is (Verb  = No_Verb);
      function In_Object_Part  return Boolean is (Verb /= No_Verb);

   begin
      Step_String := Line;
      Cmd_List    := Cmd_Lists.Empty_Vector;

      Initialize_Lexer;

      -- Put_Line ("Parsing """ & To_String (Line) & """", Verbosity => IO.Debug);
      Line_Processing : while More_Token loop
         declare
            TT  : Token_Type;
            Tmp : aliased constant String := To_String (Line);
            Tok : constant String := Next_Token (Tmp'Access, TT);

         begin
            -- Put_Line ("Token = " & TT'Image & " " & Tok'Image);
            case TT is
               when Keyword =>
                  declare
                     Lower_Keyword : constant String := Translate
                       (Source  => Tok,
                        Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
                  begin
                     if First_Token then
                        if    Lower_Keyword = "given" then
                           Cat  := Given_Step;
                           Prep := Given;

                        elsif Lower_Keyword = "when" then
                           Cat  := When_Step;
                           Prep := When_P;

                        elsif Lower_Keyword = "then" then
                           Cat  := Then_Step;
                           Prep := Then_P;

                        elsif Lower_Keyword = "and" or else
                          Lower_Keyword = "but"
                        then
                           Cat := Context;
                           -- inherited from the context
                           case Context is
                              when Unknown    => null;
                              when Given_Step => Prep := Given;
                              when When_Step  => Prep := When_P;
                              when Then_Step  => Prep := Then_P;
                           end case;
                        else
                           IO.Put_Warning
                             ("Keyword : " & Tok & " ignored", Loc);

                        end if;

                     else
                        null;
                        -- given/when/then may appear later on the line, but
                        -- then are not considered as keywords.
                     end if;

                     if Lower_Keyword = "run" or Lower_Keyword = "running" then
                        if Successfully_Met then
                           Verb := Successful_Run;
                        else
                           Verb := Run;
                        end if;

                     elsif Lower_Keyword = "or" then
                        Or_Met := @ + 1;
                        if Object /= Command_List then
                           Object := Command_List;
                           -- there should already be a code span, let's
                           -- store it in the list
                           Cmd_List.Append (+Object_String);
                           Object_String := Null_Unbounded_String;
                        end if;

                     elsif Lower_Keyword = "get" then
                        Verb := Get;

                     elsif Lower_Keyword = "is" then
                        Verb := Is_V;

                     elsif Lower_Keyword = "no"
                       or Lower_Keyword = "not"
                       or Lower_Keyword = "dont"
                       or Lower_Keyword = "doesnt"
                       or Lower_Keyword = "doesn't"
                     then
                        Not_Met := True;
                        if Verb = Is_V then
                           Verb := Is_No;
                        elsif Verb = Get  then
                           Verb := Get_No;
                        end if;

                     elsif Lower_Keyword = "successfully" then
                        Successfully_Met := True;

                     elsif Lower_Keyword = "error" then
                        Object := Error;

                     elsif Lower_Keyword = "output" then
                        if In_Subject_Part then
                           Subject := Output_Subj;
                        else
                           Object := Output_Obj;
                        end if;

                     elsif Lower_Keyword = "contains" or
                       Lower_Keyword = "contain"
                     then
                        if Not_Met then
                           Verb := Does_Not_Contain;
                        else
                           Verb := Contains;
                        end if;

                     elsif Lower_Keyword = "containing" then
                        Verb := Containing;

                     elsif Lower_Keyword = "new" then
                        Subject_Attr := New_SA;
                        File_Type := Ordinary_File;
                        -- "file" keyword is not mandatory, this is the default.

                     elsif Lower_Keyword = "directory" or Lower_Keyword = "dir"
                     then
                        File_Type := Directory;

                        if In_Subject_Part then
                           Subject := Subject_Dir;

                        elsif In_Object_Part then
                           Object := Object_Dir;

                        end if;

                     elsif Lower_Keyword = "file" then
                        File_Type := Ordinary_File;

                        if In_Subject_Part then
                           Subject := Subject_File;

                        elsif In_Object_Part then
                           Object := Object_File;

                        end if;

                     elsif Lower_Keyword = "unordered" then
                        Ignore_Order := True;

                     end if;
                  end;

               when Identifier =>
                  -- Put_Line (Prefix & "   Identifier : " & Tok);
                  null;

               when Code_Span =>
                  if In_Subject_Part then
                     Subject_String := To_Unbounded_String (Tok);

                  else
                     Object_String := To_Unbounded_String (Tok);
                  end if;

                  if Object = Command_List then
                     Cmd_List.Append (Tok);

                  elsif In_Subject_Part and then Subject = No_Subject then
                     if File_Type = Directory then
                        Subject := Subject_Dir;
                     else
                        Subject := Subject_File;
                     end if;
                     -- Subject_String := To_Unbounded_String (Tok);

                  elsif In_Object_Part and then Object = No_Object then
                     case Verb is
                        when No_Verb |
                             Is_No   =>
                           -- Verbs always followed by a file/dir
                           if File_Type = Directory then
                              Object := Object_Dir;
                           else
                              Object := Object_File;
                           end if;

                        when Run              |
                             Successful_Run   |
                             Get              |
                             Get_No           |
                             Contains         |
                             Does_Not_Contain |
                             Containing       =>
                           -- Verbs are always followed by a text
                           Object := Object_Text;

                        when Is_V =>
                           -- Complex case where it depends not only on the
                           -- verb...
                           if Subject = No_Subject then
                              -- Given there is a `config.ini` file
                              if File_Type = Directory then
                                 Object := Object_Dir;
                              else
                                 Object := Object_File;
                              end if;

                           else
                              -- Then output is xxxx
                              -- or
                              -- Then `file` is xxxx
                              Object := Object_Text;

                           end if;
                     end case;
                  end if;

               when Empty =>
                  null;

            end case;

         end;
         First_Token := False;

      end loop Line_Processing;

      -- Some coherency tests to give to the user a more helpful
      -- message than "Unrecogniezd Step"
      declare
         use Ada.Containers;
      begin
         if Verb = Run or Verb = Successful_Run then
            if Or_Met > 0 and then Cmd_List.Length /= Count_Type (Or_Met + 1) then
               IO.Put_Warning ("Command missing after last ""or""", Loc);
            else
               if Object_String = Null_Unbounded_String then
                  IO.Put_Warning ("No command after ""run"" keyword", Loc);
               end if;
            end if;

         end if;
      end;

      Action := Get_Action (Prep, Subject_Attr, Subject, Verb, Object);

      IO.Put (Prefix & " Rule = ", Verbosity => IO.Debug);
      Put_Rule (Prep, Subject_Attr, Subject, Verb, Object, Action,
                Verbosity => IO.Debug);

      Context := Cat;

      return (Cat,
              Action,
              Step_String,
              Loc,
              Subject_String,
              Object_String,
              File_Type,
              Ignore_Order,
              File_Content    => Empty_Text,
              Parent_Scenario => null);

   end Parse;

   -- --------------------------------------------------------------------------
   procedure Put_Keywords renames Lexer.Put_Keywords;

   -- -----------------------------------------------------------------------
   procedure Put_Grammar is
   begin
      Ada.Text_IO.Put_Line ("| Prep  |     |Subject |       Verb       |      Object       |        Action        |  ");
      Ada.Text_IO.Put_Line ("|-------|-----|--------|------------------|-------------------|----------------------|  ");
      for P in The_Grammar'Range (1) loop -- A of G when A /= None loop
         for SA in The_Grammar'Range (2) loop -- A of G when A /= None loop
            for S in The_Grammar'Range (3) loop -- A of G when A /= None loop
               for V in The_Grammar'Range (4) loop -- A of G when A /= None loop
                  for O in The_Grammar'Range (5) loop -- A of G when A /= None loop
                     if The_Grammar (P, SA, S, V, O) /= None then
                        Put_Rule (P, SA, S, V, O,
                                  The_Grammar (P, SA, S, V, O));
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      -- Ada.Text_IO.Put_Line ("|-------|-----|--------|------------------|-------------------|----------------------|");
   end Put_Grammar;

end BBT.Scenarios.Step_Parser;
