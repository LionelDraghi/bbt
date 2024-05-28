-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Text_Utilities; use Text_Utilities;

with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Strings.Maps;

package body BBT.Scenarios.Step_Parser is

   -- --------------------------------------------------------------------------
   package Lexer is

      type Token_Type is (Keyword, Identifier, Code_Span, Empty);
      -- In Markdown, Code_Span denote a word or phrase enclosed in
      -- backticks (`).
      -- Refer to https://spec.commonmark.org/0.31.2/#code-spans
      -- for specification
      -- In BBT, backticks enclose the command to run, or a file name,
      -- or an expected output.

      procedure Initialize_Lexer;

      function Next_Token (Line     : access constant String;
                           Tok_Type : out Token_Type)
                           return String;

      function More_Token return Boolean;

      function Is_A_Keyword (S     : access constant String;
                             First : Positive;
                             Last  : Natural := 0) return Boolean;

      procedure Put_Keywords;

   end Lexer;

   -- --------------------------------------------------------------------------
   package body Lexer is

      Cursor         : Natural := 1;
      Line_Finished  : Boolean := False;
      Backtick       : constant Character := '`';
      The_Delimiters : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set (" _*" & Ada.Characters.Latin_1.HT);

      package String_Arrays is new Ada.Containers.Indefinite_Vectors (Positive,
                                                                      String);
      Keywords : constant String_Arrays.Vector
        := ["given",
            "when",
            "then",
            "and",      -- "and" and "but" are equivalent to "Given" if they
            "but",      -- appear in the "Given" section, to "Then" if tey
            "run",      -- appear in the "Then" section, etc.
            "running",  -- "When I run" = "When running"
            "get",
            "new",
            "no",       -- "no" = "not" = "dont"
            "not",
            "dont",
            "error",
            "is",
            "or",
            "output",
            "contains",
            "containing",
            "successfully",
            "file",
            "directory"];
      -- NB : all keywords must be in lower case!

      -- -----------------------------------------------------------------------
      procedure Initialize_Lexer is begin
         Cursor := 1;
         Line_Finished := False;
      end Initialize_Lexer;

      -- -----------------------------------------------------------------------
      function Next_Token (Line     : access constant String;
                           Tok_Type : out Token_Type)
                           return String is
         First : Positive;      -- Index of first character in token
         Last  : Natural := 0;  -- Index of last character in token

         -- Note that Cursor is >= Last
         -- Cursor is the point where next call to Next_Token will start,
         -- Last is the last character of the Token
         -- In the case of a code span, Last will designate a character before
         -- the closing backtick, and the cursor the character after.

         procedure Finish_Line is
         begin
            Line_Finished := True;
            Cursor := 1;
         end Finish_Line;

      begin
         Find_Token (Source => Line.all (Cursor .. Line'Last),
                     Set    => The_Delimiters,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
         --  Put_Line ("processing token = """ & Line.all (First .. Last) &
         --              """ in " & Line.all,
         --            Verbosity => IO.Debug);
         Cursor := (Natural'Min (Line.all'Length, Last + 1));
         -- Jump to next char unless already on the last
         if Is_A_Keyword (Line, First, Last) then
            -- Keyword ---------------------------------------------------------
            Tok_Type := Keyword;
            --  Put_Line ("Found Keyword """ & Line.all (First .. Last) &
            --              """ in " & Line.all,
            --            Verbosity => IO.Debug);

         elsif Line (First) = Backtick then
            if Line'Last > First and then Line (First + 1) /= Backtick then
               -- Code span start
               -- Note that we test also First + 1 because
               -- "``" is not a code span start, backtick should not be
               -- followed by another
               Last := Index (Line.all,
                              [Backtick],
                              From => First + 1);
               if Last = 0 then
                  IO.Put_Line ("Missing closing backtick in """ &
                                 Line.all (Cursor .. Line'Last),
                               Verbosity => IO.Quiet);
                  Finish_Line;
               else
                  -- Code span -------------------------------------------------
                  Cursor := Last + 1; -- the cursor goes over the final backtick
                  First := @ + 1; -- remove first backtick
                  Last  := @ - 1; -- remove final backtick

                  --  Put_Line ("Found code span = """ & Line.all (First .. Last) &
                  --              """ in " & Line.all,
                  --            Verbosity => IO.Debug);
                  Tok_Type := Code_Span;
               end if;

            else
               -- Jump over "``"
               Tok_Type := Empty;
               Cursor := First + 2;

            end if;

         else
            -- Identifier ------------------------------------------------------
            Tok_Type := Identifier;

         end if;

         if Cursor >= Line.all'Last then
            -- It's the end of line
            -- -> next line.
            Finish_Line;
            -- Put_Line ("EOL");
         end if;

         if Last = 0 then
            -- nothing found
            Finish_Line;
            Tok_Type := Empty;

            return ""; ---------------------------------------------------------

         else
            return (Line.all (First .. Last)); ---------------------------------

         end if;

      end Next_Token;

      -- -----------------------------------------------------------------------
      function More_Token return Boolean is
      begin
         return not Line_Finished;
      end More_Token;

      -- -----------------------------------------------------------------------
      function Is_A_Keyword (S      : access constant String;
                             First  : Positive;
                             Last   : Natural := 0)
                             return Boolean
      is
         -- We first remove potential emphasis on keyword
         -- [](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis)
         Emphasis : constant Ada.Strings.Maps.Character_Set
           := Ada.Strings.Maps.To_Set ("*_");
         Trimmed   : constant String := Trim (S.all (First .. Last),
                                              Left  => Emphasis,
                                              Right => Emphasis);
         Lower    : constant String := Translate
           (Source  => Trimmed,
            Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
      begin
         -- Put_Line ("K = " & S.all (First .. Last) & ", Trimmed = " & Trimmed);
         return Keywords.Contains (Lower);
      end Is_A_Keyword;

      -- -----------------------------------------------------------------------
      procedure Put_Keywords is
      begin
         for K of Keywords loop
            Ada.Text_IO.Put_Line ("- " & K);
         end loop;
      end Put_Keywords;

   end Lexer;

   -- --------------------------------------------------------------------------
   package Parser is

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
                       --                   code fenced lines, before verb
                       Output,
                       -- Verbs ------------------------------------------------
                       No_Verb,
                       Run,
                       Successful_Run,
                       Get,
                       Get_No,
                       Contains,
                       Containing,
                       Is_V,
                       Is_No,
                       -- Objects ----------------------------------------------
                       No_Object,
                       Object_File,
                       Object_Dir,  -- file or dir name
                       Object_Text, -- content of code span or following
                       --              code fenced lines, before verb
                       Command_List,
                       Error);

      subtype Objects        is Tokens range No_Object    .. Tokens'Last;
      subtype Verbs          is Tokens range No_Verb      .. Tokens'Pred (Objects'First);
      subtype Subjects       is Tokens range No_Subject   .. Tokens'Pred (Verbs'First);
      subtype Subject_Attrib is Tokens range No_SA        .. Tokens'Pred (Subjects'First);
      subtype Prepositions   is Tokens range Tokens'First .. Tokens'Pred (Subject_Attrib'First);

      -- -----------------------------------------------------------------------
      function Image (T : Tokens) return String;

      -- -----------------------------------------------------------------------
      function Get_Action (P  : Prepositions;
                           SA : Subject_Attrib;
                           S  : Subjects;
                           V  : Verbs;
                           O  : Objects) return Actions;
      procedure Put_Rule (P         : Prepositions;
                          SA        : Subject_Attrib;
                          S         : Subjects;
                          V         : Verbs;
                          O         : Objects;
                          A         : Actions;
                          Verbosity : IO.Verbosity_Levels := IO.Normal);

      -- -----------------------------------------------------------------------
      procedure Put_Grammar;

   end Parser;

   -- --------------------------------------------------------------------------
   package body Parser is

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
            when Output           => return "output";
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
            when Is_V             => return "is";
            when Is_No            => return "is no";
            when No_Object        => return "";
            when Object_File      => return "`file`";
            when Object_Dir       => return "`dir`";
            when Object_Text      => return "`text`";
            when Command_List     => return "`cmd` [or `cmd`]*";
            when Error            => return "error";
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
         G (Given, New_SA, Subject_File, No_Verb,    No_Object)   := Erase_And_Create; -- Given the new file `config.ini` followed by code fenced content
         G (Given, New_SA, Subject_Dir,  No_Verb,    No_Object)   := Erase_And_Create; -- Given the new directory `dir1`
         G (Given, No_SA,  Subject_File, No_Verb,    No_Object)   := Create_File;      -- Given the file `config.ini` followed by code fenced content
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
         G (Then_P, No_SA, Output,       Is_V,     Object_Text) := Output_Is; -- then output is `msg`
         G (Then_P, No_SA, Output,       Is_V,     No_Object)   := Output_Is; -- then output is followed by code fenced content
         G (Then_P, No_SA, No_Subject,   Get,      Object_Text) := Output_Is; -- then I get `msg`
         G (Then_P, No_SA, No_Subject,   Get,      No_Object)   := Output_Is; -- then I get followed by code fenced content
         G (Then_P, No_SA, Output,       Contains, Object_Text) := Output_Contains; -- then output contains `msg`
         G (Then_P, No_SA, Output,       Contains, No_Object)   := Output_Contains; -- then output contains followed by code fenced content
         G (Then_P, No_SA, Subject_File, Is_V,     Object_Text) := File_Is; -- then `config.ini` is `mode=silent`
         G (Then_P, No_SA, Subject_File, Is_V,     No_Object)   := File_Is; -- Then `config.ini` is followed by code fenced content
         G (Then_P, No_SA, Subject_File, Contains, Object_Text) := File_Contains; -- Then `config.ini` contains `--version`
         G (Then_P, No_SA, Subject_File, Contains, No_Object)   := File_Contains; -- Then `config.ini` contains followed by code fenced content
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

   end Parser;

   Context : Extended_Step_Categories;
   -- Some line are hard to interpret without the context:
   -- line that starts with "And" for example will inherit their type from
   -- the previouly analyzed lines.
   -- This variable keep the memory of where we are between call Parse.

   -- --------------------------------------------------------------------------
   function Parse (Line     :     Unbounded_String;
                   Loc      :     Location_Type;
                   Cmd_List : out Cmd_Lists.Vector) return Step_Type
   is
      use Lexer;
      use Parser;

      First_Token      : Boolean        := True;
      Successfully_Met : Boolean        := False;
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
            TT       : Token_Type;
            Tmp      : aliased constant String := To_String (Line);
            Tok      : constant String := Next_Token (Tmp'Access, TT);

         begin
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
                           IO.Put_Warning ("Keyword : " & Tok & " ignored", Loc);

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
                     then
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
                        Subject := Output;

                     elsif Lower_Keyword = "contains" then
                        Verb := Contains;

                     elsif Lower_Keyword = "containing" then
                        Verb := Containing;

                     elsif Lower_Keyword = "new" then
                        Subject_Attr := New_SA;
                        File_Type := Ordinary_File;
                        -- "file" keyword is not mandatory, this is the default.

                     elsif Lower_Keyword = "directory" then
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

                        when Run            |
                             Successful_Run |
                             Get            |
                             Get_No         |
                             Contains       |
                             Containing     =>
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
              File_Content    => Empty_Text,
              Parent_Scenario => null);

   end Parse;

   -- --------------------------------------------------------------------------
   procedure Put_Keywords is
   begin
      Lexer.Put_Keywords;
   end Put_Keywords;

   -- --------------------------------------------------------------------------
   procedure Put_Grammar is
   begin
      Parser.Put_Grammar;
   end Put_Grammar;

end BBT.Scenarios.Step_Parser;
