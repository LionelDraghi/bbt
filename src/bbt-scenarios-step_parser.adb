-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Scenarios.Step_Parser.Lexer;

with Text_Utilities;

with Ada.Containers;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Strings.Maps;


use BBT.Scenarios.Step_Parser.Lexer,
    Text_Utilities,
    Ada.Directories,
    Ada.Strings.Fixed;

package body BBT.Scenarios.Step_Parser is

   type Tokens is ( -- Prepositions -----------------------------------------
                    Given,
                    When_P,
                    Then_P,
                    -- Subjects Attribute -----------------------------------
                    No_SA, -- = no Subject Attribute
                    Executable,
                    -- Empty,
                    -- Existing,
                    New_SA,
                    -- Subjects ---------------------------------------------
                    No_Subject,
                    Dir_Subject,  -- dir name
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
                    Matches,
                    Does_Not_Match,
                    Containing,
                    Is_V,
                    Is_No,
                    -- Objects ----------------------------------------------
                    No_Object,
                    Output_Obj,
                    Obj_File_Name,
                    Obj_Dir_Name, -- file or dir name
                    Obj_Text,     -- content of code span on the same line or
                    --               code fenced block expected on following lines
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

   -- --------------------------------------------------------------------------
   function Image (T : Tokens) return String is
   begin
      case T is
         when Given            => return "Given";
         when When_P           => return "When";
         when Then_P           => return "Then";
         when No_SA            => return "";
         when Executable       => return "executable";
         when New_SA           => return "new";
         when No_Subject       => return "";
         when Output_Subj      => return "output";
         when Subject_File     => return "`file`";
         when Dir_Subject      => return "`dir`";
         when Subject_Text     => return "`text`";
         when No_Verb          => return "";
         when Run              => return "run";
         when Successful_Run   => return "successfully run";
         when Get              => return "get";
         when Get_No           => return "get no";
         when Contains         => return "contains";
         when Matches          => return "matches";
         when Containing       => return "containing";
         when Does_Not_Contain => return "does not contain";
         when Does_Not_Match   => return "does not match";
         when Is_V             => return "is";
         when Is_No            => return "is no";
         when No_Object        => return "";
         when Output_Obj       => return "output";
         when Obj_File_Name    => return "`file`";
         when Obj_Dir_Name     => return "`dir`";
         when Obj_Text         => return "`text`";
         when Command_List     => return "`cmd`"; -- [or `cmd`]*";
         when Error            => return "error";
         when Unordered        => return "unordered";
      end case;
   end Image;

   type Grammar_Items is record
      Action              : Actions;
      Code_Block_Expected : Boolean;
   end record;

   type Grammar is array (Prepositions, Subject_Attrib,
                          Subjects, Verbs, Objects) of Grammar_Items; --  Actions;
   Null_Grammar : constant Grammar
     := [others => [others => [others => [others => [others => (None, False)]]]]];

   -- -----------------------------------------------------------------------
   function Create_Grammar return Grammar is
      G : Grammar := Null_Grammar;
      -- the last boolean is set when a code fenced block is expected after the line
   begin
      G (Given, No_SA,  No_Subject,   Is_No,      Obj_File_Name)       := (Setup_No_File, False); -- Given there is no `config.ini` file
      G (Given, No_SA,  No_Subject,   Is_No,      Obj_Dir_Name)        := (Setup_No_Dir, False);  -- Given there is no `dir1`       directory
      G (Given, No_SA,  No_Subject,   Is_V,       Obj_File_Name)       := (Check_File_Existence, False); -- Given there is a `config.ini` file
      G (Given, No_SA,  No_Subject,   Is_V,       Obj_Dir_Name)        := (Check_Dir_Existence, False);  -- Given there is a `dir1` directory
      G (Given, New_SA, Subject_File, Containing, Obj_Text)            := (Erase_And_Create, False); -- Given the new file `config.ini` containing `lang=it`
      G (Given, No_SA,  Subject_File, Containing, Obj_Text)            := (Create_If_None, False); -- Given the file `config.ini` containing `lang=it`
                                                                                                   -- Fixme: we currently do not check if the existing file contains
                                                                                                   --  what is expected
      G (Given, New_SA, Subject_File, Containing, No_Object)           := (Erase_And_Create, True); -- Given the new file `config.ini` followed by code fenced content
      G (Given, No_SA,  Subject_File, Containing, No_Object)           := (Create_If_None, True);   -- Given the file `config.ini` followed by code fenced content
                                                                                                    -- Fixme : we currently do not check if the existing file contains
                                                                                                    --  what is expected
      G (Given, New_SA, Subject_File, No_Verb,    No_Object)           := (Erase_And_Create, True); -- Given the new file `config.ini` followed by code fenced content
      G (Given, No_SA,  Subject_File, No_Verb,    No_Object)           := (Erase_And_Create, True); -- Given the file `config.ini` followed by code fenced content
      G (Given, New_SA, Dir_Subject,  No_Verb,    No_Object)           := (Erase_And_Create, False); -- Given the new directory `dir1`
      G (Given, No_SA,  Dir_Subject,  No_Verb,    No_Object)           := (Create_If_None, False);   -- Given the directory `dir1`
      G (Given, No_SA,  No_Subject,   Run,            Obj_Text)        := (Run_Cmd, False);           -- Given I run `cmd`
      G (Given, No_SA,  No_Subject,   Successful_Run, Obj_Text)        := (Run_Without_Error, False); -- Given i successfully run `cmd`

      G (When_P, No_SA, No_Subject, Run,            Obj_Text)          := (Run_Cmd, False);           -- when I run `cmd`
      G (When_P, No_SA, No_Subject, Successful_Run, Obj_Text)          := (Run_Without_Error, False); -- when i successfully run `cmd`
      G (When_P, No_SA, No_Subject, Run,            Command_List)      := (Run_Cmd, False);           -- when I run `cmd` or `cmd2` or `cmd3`
      G (When_P, No_SA, No_Subject, Successful_Run, Command_List)      := (Run_Without_Error, False); -- when i successfully run `cmd` or `cmd2` or `cmd3`

      G (Then_P, No_SA, No_Subject,   Is_V,     Obj_File_Name)         := (Check_File_Existence, False); -- Then there is a  `config.ini` file
      G (Then_P, No_SA, No_Subject,   Is_No,    Obj_File_Name)         := (Check_No_File, False);        -- Then there is no `config.ini` file
      G (Then_P, No_SA, No_Subject,   Is_V,     Obj_Dir_Name)          := (Check_Dir_Existence, False); -- Then there is a  `dir1` directory
      G (Then_P, No_SA, No_Subject,   Is_No,    Obj_Dir_Name)          := (Check_No_Dir, False);        -- Then there is no `dir1` directory
      G (Then_P, No_SA, No_Subject,   Get,      Error)                 := (Error_Return_Code, False);    -- then I get error
      G (Then_P, No_SA, No_Subject,   Get_No,   Error)                 := (No_Error_Return_Code, False); -- then I get no error
      G (Then_P, No_SA, No_Subject,   Is_V,     Error)                 := (Error_Return_Code, False);    -- then there is an error
      G (Then_P, No_SA, No_Subject,   Is_No,    Error)                 := (No_Error_Return_Code, False); -- then there is no error
      G (Then_P, No_SA, Output_Subj,  Is_V,     Obj_Text)              := (Output_Is, False); -- then output is `msg`
      G (Then_P, No_SA, Output_Subj,  Is_V,     Obj_File_Name)         := (Output_Is, False); -- then output is file `expected.txt`
      G (Then_P, No_SA, Output_Subj,  Is_V,     No_Object)             := (Output_Is, True); -- then output is followed by code fenced content
      G (Then_P, No_SA, No_Subject,   Get,      Obj_Text)              := (Output_Is, False); -- then I get `msg`
      G (Then_P, No_SA, No_Subject,   Get,      Obj_File_Name)         := (Output_Is, False); -- Then I get file `flowers2.txt`
      G (Then_P, No_SA, No_Subject,   Get,      No_Object)             := (Output_Is, True); -- then I get followed by code fenced content
      G (Then_P, No_SA, Output_Subj,  Contains, Obj_Text)              := (Output_Contains, False); -- then output contains `msg`
      G (Then_P, No_SA, Output_Subj,  Contains, Obj_File_Name)         := (Output_Contains, False); -- Then output contains `snippet.txt` file
      G (Then_P, No_SA, Output_Subj,  Contains, No_Object)             := (Output_Contains, True); -- then output contains followed by code fenced content
      G (Then_P, No_SA, Output_Subj,  Does_Not_Contain, Obj_Text)      := (Output_Does_Not_Contain, False); -- then output does not contain `msg`
      G (Then_P, No_SA, Output_Subj,  Does_Not_Contain, Obj_File_Name) := (Output_Does_Not_Contain, False); -- Then output does not contain file `snippet.txt`
      G (Then_P, No_SA, Output_Subj,  Does_Not_Contain, No_Object)     := (Output_Does_Not_Contain, True); -- then output does not contain followed by code fenced content
      G (Then_P, No_SA, Output_Subj,  Matches,  Obj_Text)              := (Output_Matches, False); -- then output matches `[:digit:]*.[:digit:]*`
      G (Then_P, No_SA, Output_Subj,  Does_Not_Match,  Obj_Text)       := (Output_Does_Not_Match, False); -- then output does not match `[:digit:]*.[:digit:]*`
      G (Then_P, No_SA, Subject_File, Is_V,     Obj_Text)              := (File_Is, False); -- then `config.ini` is `mode=silent`
      G (Then_P, No_SA, Subject_File, Is_V,     Obj_File_Name)         := (File_Is, False); -- then `config.ini` is equal to file `expected/config.ini`
      G (Then_P, No_SA, Subject_File, Is_V,     No_Object)             := (File_Is, True); -- then `config.ini` is followed by code fenced content
      G (Then_P, No_SA, Subject_File, Is_No,    Obj_File_Name)         := (File_Is_Not, False); -- then `config.ini` is no more equal to file `previous_config.ini`
      G (Then_P, No_SA, Subject_File, Contains, Obj_Text)              := (File_Contains, False); -- Then `config.ini` contains `--version`
      G (Then_P, No_SA, Subject_File, Contains, Obj_File_Name)         := (File_Contains, False); -- Then `config.ini` contains `snippet.txt` file
      G (Then_P, No_SA, Subject_File, Contains, No_Object)             := (File_Contains, True); -- Then `config.ini` contains followed by code fenced content
      G (Then_P, No_SA, Subject_File, Does_Not_Contain, Obj_Text)      := (File_Does_Not_Contain, False); -- Then `config.ini` does not contain `--version`
      G (Then_P, No_SA, Subject_File, Does_Not_Contain, Obj_File_Name) := (File_Does_Not_Contain, False); -- Then `config.ini` does not contain `snippet.txt` file
      G (Then_P, No_SA, Subject_File, Does_Not_Contain, No_Object)     := (File_Does_Not_Contain, True); -- Then `config.ini` does not contain followed by code fenced content
      G (Then_P, No_SA, No_Subject,   Get_No,   Output_Obj)            := (No_Output, False); -- then I get no output
      G (Then_P, No_SA, No_Subject,   Is_No,    Output_Obj)            := (No_Output, False); -- then there is no output
      return G;
   end Create_Grammar;

   The_Grammar : constant Grammar := Create_Grammar;

   -- -----------------------------------------------------------------------
   procedure Put_Rule (P           : Prepositions;
                       SA          : Subject_Attrib;
                       S           : Subjects;
                       V           : Verbs;
                       O           : Objects;
                       A           : Grammar_Items;
                       Verbosity   : IO.Verbosity_Levels := IO.Normal)
   is
      use Ada.Text_IO;
      C : Positive_Count := Col;
   begin
      if Is_Authorized (Verbosity) then
         Ada.Text_IO.Put  ("| " & Image (P));
         C := @ + Prepositions'Width + 1; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (SA));
         C := @ + Subject_Attrib'Width; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (S));
         C := @ + Subjects'Width - 3; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (V));
         C := @ + Verbs'Width + 3; Set_Col (C);

         Ada.Text_IO.Put (" | " & Image (O));
         C := @ + 9; Set_Col (C);

         Ada.Text_IO.Put (" | " & A.Action'Image);
         C := @ + Actions'Width + 3; Set_Col (C);

         Ada.Text_IO.Put (" | " &
                          (if A.Code_Block_Expected then "    X" else ""));
         C := @ + 13; Set_Col (C);

         Ada.Text_IO.Put_Line (" |  ");
      end if;
   end Put_Rule;

   Previous_Step_Kind : Extended_Step_Categories := Unknown;
   -- Step starting with "And" or "But" have the kind of the last "Given" /
   -- "When" or "then" encountered, this variable keep this memory
   -- between calls to Parse.

   -- --------------------------------------------------------------------------
   package Chunk is
      procedure Initialize;
      procedure Set_Verb (V : Verbs);
      procedure Set_Loc (L : Location_Type);
      function Verb return Verbs;
      function In_Subject_Part return Boolean;
      function In_Object_Part  return Boolean;
   end Chunk;

   -- --------------------------------------------------------------------------
   package body Chunk is

      Current_Verb : Verbs := No_Verb;
      Loc : Location_Type;

      procedure Initialize is
      begin
         Current_Verb := No_Verb;
      end Initialize;

      procedure Set_Verb (V : Verbs) is
      begin
         if Current_Verb = No_Verb
           or else (Current_Verb = Get      and V = Get_No)
           or else (Current_Verb = Contains and V = Does_Not_Contain)
           or else (Current_Verb = Matches  and V = Does_Not_Match)
           or else (Current_Verb = Is_V     and V = Is_No)
         then
            -- Setting a verb for the first time, or changing it
            -- to the negative form is OK.
            Current_Verb := V;
         else
            -- But if 'is' is detected after 'contains' we warn the user
            -- that the step wording may be ambiguous.
            IO.Put_Warning
              ("Verb is '" & Image (Current_Verb)
               & "', ignoring following '" & Image (V) & "'",
               Loc);
         end if;
      end Set_Verb;

      procedure Set_Loc (L : Location_Type) is
      begin
         Loc := L;
      end Set_Loc;

      function Verb return Verbs is (Current_Verb);

      function In_Subject_Part return Boolean is (Current_Verb  = No_Verb);
      function In_Object_Part  return Boolean is (Current_Verb /= No_Verb);

   end Chunk;

   -- --------------------------------------------------------------------------
   function Parse (Line                :        Unbounded_String;
                   Loc                 : in out Location_Type;
                   Code_Block_Expected :    out Boolean;
                   Cmd_List            :    out Cmd_Lists.Vector)
                   return Step_Type
   is
      Successfully_Met : Boolean        := False;
      Or_Met           : Natural        := 0;
      Not_Met          : Boolean        := False;
      Prep             : Prepositions   := Prepositions'First;
      Subject_Attr     : Subject_Attrib := No_SA;
      Subject          : Subjects       := No_Subject;
      Object           : Objects        := No_Object;
      File_Type        : File_Kind      := Ordinary_File;

      -- All component of the returned Step are initialized :
      Cat              : Extended_Step_Categories := Unknown;
      Action           : Actions                  := None;
      Step_String      : Unbounded_String         := Null_Unbounded_String;
      Subject_String   : Unbounded_String         := Null_Unbounded_String;
      Object_String    : Unbounded_String         := Null_Unbounded_String;
      Object_File_Name : Unbounded_String         := Null_Unbounded_String;
      Ignore_Order     : Boolean                  := False;
      -- by default, order of expected output is significant
      Executable       : Boolean                  := False;

      use Chunk;

      Tmp : aliased constant String := To_String (Line);
      TT            : Token_Type;

   begin
      Step_String := Line;
      Cmd_List    := Cmd_Lists.Empty_Vector;

      Initialize_Lexer;
      Chunk.Initialize;

      -- First token processing
      declare
         Tok           : constant String := Next_Token (Tmp'Access, TT); --, Loc);
         Lower_Keyword : constant String := Translate
           (Source  => Tok,
            Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);

      begin
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
            Cat := Previous_Step_Kind;
            -- Inherited from the context
            case Previous_Step_Kind is
               when Unknown    => null;
               when Given_Step => Prep := Given;
               when When_Step  => Prep := When_P;
               when Then_Step  => Prep := Then_P;
            end case;

         else
            IO.Put_Error ("line starting with '- " & Tok
                          & "' is not a step, ignored", Loc);
            -- If the first word after `-` is not one of those,
            -- then this is not a step

         end if;
         -- given/when/then may appear later on the line, but
         -- then are not considered as keywords.
      end;

      if No_Error then

         Chunk.Set_Loc (Loc);

         Line_Processing : while More_Token loop

            declare
               Tok : constant String := Next_Token (Tmp'Access, TT); -- , Loc);

            begin
               -- Put_Line ("Token = " & TT'Image & " " & Tok'Image);
               case TT is
               when Keyword =>
                  declare
                     Lower_Keyword : constant String := Translate
                       (Source  => Tok,
                        Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
                  begin
                     if Lower_Keyword = "executable" then
                        Executable := True;

                     elsif Lower_Keyword = "run" or Lower_Keyword = "running" then
                        if Successfully_Met then
                           Set_Verb (Successful_Run);
                        else
                           Set_Verb (Run);
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
                        Set_Verb (Get);

                     elsif Lower_Keyword = "is" then
                        Set_Verb (Is_V);

                     elsif Lower_Keyword = "no"
                       or Lower_Keyword = "not"
                       or Lower_Keyword = "dont"
                       or Lower_Keyword = "doesnt"
                       or Lower_Keyword = "doesn't"
                     then
                        Not_Met := True;
                        if Verb = Is_V then
                           Set_Verb (Is_No);
                        elsif Verb = Get then
                           Set_Verb (Get_No);
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
                           Set_Verb (Does_Not_Contain);
                        else
                           Set_Verb (Contains);
                        end if;

                     elsif Lower_Keyword = "match" or
                       Lower_Keyword = "matches"
                     then
                        if Not_Met then
                           Set_Verb (Does_Not_Match);
                        else
                           Set_Verb (Matches);
                        end if;

                     elsif Lower_Keyword = "containing" then
                        Set_Verb (Containing);

                     elsif Lower_Keyword = "new" then
                        Subject_Attr := New_SA;

                     elsif Lower_Keyword = "directory" or Lower_Keyword = "dir"
                     then
                        File_Type := Directory;

                        if In_Subject_Part then
                           Subject := Dir_Subject;

                        elsif In_Object_Part then
                           Object := Obj_Dir_Name;

                           Object_File_Name := Object_String;
                           Object_String    := Null_Unbounded_String;
                        end if;

                     elsif Lower_Keyword = "file" then
                        File_Type := Ordinary_File;

                        if In_Subject_Part then
                           Subject := Subject_File;

                        elsif In_Object_Part then
                           Object := Obj_File_Name;
                           -- If file name was given before keyword "file",
                           -- then it was considered an Object_String,
                           -- let's update that:
                           Object_File_Name := Object_String;
                           Object_String    := Null_Unbounded_String;
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
                     if Object = Obj_File_Name or Object = Obj_Dir_Name then
                        -- "file" or "dir" keyword already meet
                        Object_File_Name := To_Unbounded_String (Tok);
                     else
                        -- Otherwise, we don't know yet if the Code_Span is
                        -- a simple string or a file name.
                        Object_String := To_Unbounded_String (Tok);
                     end if;
                  end if;

                  if Object = Command_List then
                     Cmd_List.Append (Tok);

                  elsif In_Subject_Part and then Subject = No_Subject then
                     if File_Type = Directory then
                        Subject := Dir_Subject;
                     else
                        Subject := Subject_File;
                     end if;

                  elsif In_Object_Part and then Object = No_Object then
                     case Verb is
                        when No_Verb |
                             Is_No   =>
                           -- Those verbs are always followed by a file/dir
                           if File_Type = Directory then
                              Object := Obj_Dir_Name;
                           else
                              Object := Obj_File_Name;
                           end if;

                        when Run              |
                             Successful_Run   |
                             Get              |
                             Get_No           |
                             Contains         |
                             Matches          |
                             Does_Not_Contain |
                             Does_Not_Match   |
                             Containing       =>
                           -- Those verbs are always followed by a text
                           Object := Obj_Text;

                        when Is_V =>
                           -- Complex case where it depends not only on the
                           -- verb...
                           if Subject = No_Subject then
                              -- Example : Given there is a `config.ini` file
                              if File_Type = Directory then
                                 Object := Obj_Dir_Name;
                              else
                                 Object := Obj_File_Name;
                              end if;

                           else
                              -- Then output is xxxx
                              -- or
                              -- Then `file` is xxxx
                              Object := Obj_Text;

                           end if;
                     end case;
                  end if;

               when Empty =>
                  null;

               end case;

            end;

         end loop Line_Processing;

         -- Some coherency tests to give to the user a more helpful
         -- message than "Unrecognized Step"
         declare
            use Ada.Containers;
         begin
            if Verb = Run or Verb = Successful_Run then
               if Or_Met > 0 and then Cmd_List.Length /= Count_Type (Or_Met + 1) then
                  IO.Put_Error ("Command missing after last ""or""", Loc);
               else
                  if Object_String = Null_Unbounded_String then
                     IO.Put_Error ("No command after ""run"" keyword", Loc);
                  end if;
               end if;

            end if;
         end;

         --  Ada.Text_IO.Put_Line ("Prep         = " & Prep'Image);
         --  Ada.Text_IO.Put_Line ("Subject_Attr = " & Subject_Attr'Image);
         --  Ada.Text_IO.Put_Line ("Subject      = " & Subject'Image);
         --  Ada.Text_IO.Put_Line ("Verb         = " & Verb'Image);
         --  Ada.Text_IO.Put_Line ("Object       = " & Object'Image);

         Action := The_Grammar
           (Prep, Subject_Attr, Subject, Verb, Object).Action;
         Code_Block_Expected := The_Grammar
           (Prep, Subject_Attr, Subject, Verb, Object).Code_Block_Expected;

         --  Ada.Text_IO.Put_Line ("Action       = " & Action'Image);
         --  Ada.Text_IO.Put_Line ("Code_Block_Expected = " & Code_Block_Expected'Image);

         Previous_Step_Kind := Cat;

      end if;

      return (Cat              => Cat,
              Action           => Action,
              Step_String      => Step_String,
              Location         => Loc,
              Comment          => Empty_Text,
              Subject_String   => Subject_String,
              Object_String    => Object_String,
              Object_File_Name => Object_File_Name,
              File_Type        => File_Type,
              Executable_File  => Executable,
              Ignore_Order     => Ignore_Order,
              File_Content     => Empty_Text,
              Filtered         => False,
              Parent_Scenario  => null);

   end Parse;

   -- --------------------------------------------------------------------------
   procedure Put_Keywords renames Lexer.Put_Keywords;

   -- -----------------------------------------------------------------------
   procedure Put_Grammar is
   begin
      Ada.Text_IO.Put_Line ("| Prep  |         |Subject |       Verb       | Object |         Action          | Code block expected |  ");
      Ada.Text_IO.Put_Line ("|-------|---------|--------|------------------|--------|-------------------------|------------|  ");
      for P in The_Grammar'Range (1) loop -- A of G when A /= None loop
         for SA in The_Grammar'Range (2) loop -- A of G when A /= None loop
            for S in The_Grammar'Range (3) loop -- A of G when A /= None loop
               for V in The_Grammar'Range (4) loop -- A of G when A /= None loop
                  for O in The_Grammar'Range (5) loop -- A of G when A /= None loop
                     if The_Grammar (P, SA, S, V, O).Action /= None then
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
