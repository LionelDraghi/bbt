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

      -- -----------------------------------------------------------------------
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

      -- -----------------------------------------------------------------------
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
      -- -----------------------------------------------------------------------
      Keywords : constant String_Arrays.Vector
        := [
            "given",
            "when",
            "then",
            "and",      -- "and" and "but" are equivalent to "Given" if they
            "but",      -- appear in the "Given" section, to "Then" if tey
            "run",      -- appear in the "Then" section, etc.
            "running",  -- "When I run" = "When running"
            "get",
            "existing",
            "no",       -- "no" = "not" = "dont"
            "not",
            "dont",
            "error",
            "is",
            "output",
            "contains",
            "containing",
            "successfully",
            "directory",
            "file"
           ];
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

         -- --------------------------------------------------------------------
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

      -- -----------------------------------------------------------------------
      type Tokens is ( -- Prepositions -----------------------------------------
                       Given,
                       When_P,
                       Then_P,
                       -- Subjects Attribute -----------------------------------
                       No_SA, -- = no Subject Attribute
                       -- Empty,
                       Existing,
                       -- Subjects ---------------------------------------------
                       No_Subject,
                       Subject_File_Ref, -- file or dir name
                       Subject_Text,     -- content of code span or following
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
                       Id,
                       Object_File_Ref, -- file or dir name
                       Object_Text, -- content of code span or following
                       --              code fenced lines, before verb
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
            when Existing         => return "existing";
            when No_Subject       => return "";
            when Output           => return "output";
            when Subject_File_Ref => return "file|dir";
            when Subject_Text     => return "text|file";
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
            when Id               => return "`Id`";
            when Object_File_Ref  => return "file|dir";
            when Object_Text      => return "text|file";
            when Error            => return "error";
         end case;
      end Image;

      -- -----------------------------------------------------------------------
      type Grammar is array (Prepositions, Subject_Attrib,
                             Subjects, Verbs, Objects) of Actions;
      Null_Grammar : constant Grammar
        := [others => [others => [others => [others => [others => None]]]]];

      -- -----------------------------------------------------------------------
      function Create_Grammar return Grammar is
         G : Grammar := Null_Grammar;
      begin
         G (Given,  No_SA, No_Subject,      Is_No,    Object_File_Ref) := Check_No_File; -- Given there is no `config.ini` file
         --                                                                                 Given there is no `dir1`       directory
         G (Given,  No_SA, No_Subject,      Is_V,     Object_File_Ref) := Check_File_Existence; -- Given there is an existing `config.ini` file
         G (Given,  No_SA, Subject_File_Ref, Containing, Object_Text)  := Create_New; -- Given file `config.ini` containing `lang=it`
         G (Given,  Existing, Subject_Text, No_Verb,  No_Object)       := Check_File_Existence; -- Given the existing `config.ini` file
         --                                                                                        Given the existing directory `dir1`
         G (Given,  No_SA, Subject_File_Ref, No_Verb, Object_Text)     := Create_If_None; -- Given the directory `dir1`
         G (Given,  No_SA, Subject_File_Ref, No_Verb, No_Object)       := Create_If_None; -- Given the file `config.ini`
         --                                                                               followed by code fenced content
         G (When_P, No_SA, No_Subject,      Run,      Object_Text)     := Run_Cmd; -- when I run `cmd`
         G (When_P, No_SA, No_Subject, Successful_Run, Object_Text)    := Run_Without_Error; -- when i successfully run `cmd`
         G (Then_P, No_SA, No_Subject,      Get,      Error)           := Error_Return_Code; -- then I get error
         G (Then_P, No_SA, No_Subject,      Get_No,   Error)           := No_Error_Return_Code; -- then I get no error
         G (Then_P, No_SA, Output,          Is_V,     Object_Text)     := Output_Is; -- then output is `msg`
         G (Then_P, No_SA, Output,          Is_V,     No_Object)       := Output_Is; -- then output is
         --                                                                             followed by code fenced content
         G (Then_P, No_SA, No_Subject,      Get,      Object_Text)     := Output_Is; -- then I get `msg`
         G (Then_P, No_SA, No_Subject,      Get,      No_Object)       := Output_Is; -- then I get
         --                                                                             followed by code fenced content
         G (Then_P, No_SA, Output,          Contains, Object_Text)     := Output_Contains; -- then output contains `msg`
         G (Then_P, No_SA, Output,          Contains, No_Object)       := Output_Contains; -- then output contains
         --                                                                                   followed by code fenced content
         G (Then_P, No_SA, Subject_Text,    Is_V,     Object_Text)     := File_Is; -- then `config.ini` is `mode=silent`
         G (Then_P, No_SA, Subject_Text,    Is_V,     No_Object)       := File_Is; -- Then `config.ini` is
         --                                                                           followed by code fenced content
         G (Then_P, No_SA, Subject_Text,    Contains, Object_Text)     := File_Contains; -- Then `config.ini` contains `--version`
         G (Then_P, No_SA, Subject_Text,    Contains, No_Object)       := File_Contains; -- Then `config.ini` contains
         --                                                                                 followed by code fenced content
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
            Ada.Text_IO.Put (Image (P));  C := @ +  6; Set_Col (C);
            Ada.Text_IO.Put (Image (SA)); C := @ +  9; Set_Col (C);
            Ada.Text_IO.Put (Image (S));  C := @ + 10; Set_Col (C);
            Ada.Text_IO.Put (Image (V));  C := @ + 17; Set_Col (C);
            Ada.Text_IO.Put (Image (O));  C := @ +  9; Set_Col (C);
            Ada.Text_IO.Put_Line (" --> " & A'Image);
         end if;
      end Put_Rule;

      -- -----------------------------------------------------------------------
      procedure Put_Grammar is
      begin
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
      end Put_Grammar;

   end Parser;

   Context : Extended_Step_Categories;
   -- Some line are hard to interpret without the context:
   -- line that starts with "And" for example will inherit their type from
   -- the previouly analyzed lines.
   -- This variable keep the memory of where we are between call Parse.

   -- --------------------------------------------------------------------------
   function Parse (Line : Unbounded_String;
                   Loc  : Location_Type) return Step_Type
   is
      use Lexer;
      use Parser;

      First_Token      : Boolean            := True;
      Successfully_Met : Boolean            := False;
      Prep             : Prepositions;
      Subject_Attr     : Subject_Attrib := No_SA;
      Subject          : Subjects           := No_Subject;
      Verb             : Verbs              := No_Verb;
      Object           : Objects            := No_Object;

      -- All component of the returned Step are initialized :
      Cat              : Extended_Step_Categories := Unknown;
      Action           : Actions                  := None;
      Step_String      : Unbounded_String         := Null_Unbounded_String;
      Subject_String   : Unbounded_String         := Null_Unbounded_String;
      Object_String    : Unbounded_String         := Null_Unbounded_String;
      File_Type        : File_Kind                := Ordinary_File;

      Prefix : constant String := Image (Loc);

   begin
      Step_String := Line;

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
                  -- Put_Line (Prefix & "   Keyword    : """ & Tok & """");
                  -- Put_Line (Prefix & "   Previous   : """ & To_String (Previous_Token) & """");
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
                           null;
                           IO.Put_Warning ("Keyword : " & Tok & " ignored", Loc);
                        end if;
                        -- Put_Line (Prefix & "Prep = " & Prep'Image);

                     else
                        null;
                        -- given/when/then may appear later on the line, but
                        -- then are not considered as keywords.
                     end if;

                     if Lower_Keyword = "run" or Lower_Keyword = "running" then
                        if Successfully_Met then
                           Verb := Successful_Run;
                           -- Put_Line (Prefix & "Verb = successfully run");
                        else
                           Verb := Run;
                           -- Put_Line (Prefix & "Verb = run");
                        end if;

                     elsif Lower_Keyword = "get" then
                        Verb := Get;
                        -- Put_Line (Prefix & "Verb = get");

                     elsif Lower_Keyword = "is" then
                        Verb := Is_V;
                        -- Put_Line (Prefix & "Verb = is");

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
                        -- Put_Line (Prefix & "   error");
                        Object := Error;

                     elsif Lower_Keyword = "output" then
                        Subject := Output;

                     elsif Lower_Keyword = "contains" then
                        Verb := Contains;

                     elsif Lower_Keyword = "containing" then
                        Verb := Containing;

                     elsif Lower_Keyword = "existing" then
                        Subject_Attr := Existing;
                        File_Type := Ordinary_File;

                     elsif Lower_Keyword = "directory" then
                        File_Type := Directory;

                     end if;

                  end;

               when Identifier =>
                  -- Put_Line (Prefix & "   Identifier : " & Tok);
                  null;

               when Code_Span =>
                  if Verb = No_Verb then
                     -- string before verb
                     Subject_String := To_Unbounded_String (Tok);
                     if Prep = Given then
                        -- Special case for : Given the `config.ini` file
                        -- followed by the content.
                        -- This mess may diapear if (when!) I will switch to
                        -- MD file notation
                        Subject := Subject_File_Ref;
                     else
                        Subject := Subject_Text;
                     end if;
                  else
                     -- string after verb
                     Object_String := To_Unbounded_String (Tok);
                     if Prep = Given and Subject = No_Subject then
                        -- Special case for : Given there is no `config.ini` file
                        -- This mess may diapear if (when!) I will switch to
                        -- MD file notation
                        Object := Object_File_Ref;
                     else
                        Object := Object_Text;
                     end if;
                  end if;

               when Empty =>
                  null;

            end case;

         end;
         First_Token := False;

      end loop Line_Processing;

      --  Put_Line (Prep'Image);
      --  Put_Line (Subject_Attr'Image);
      --  Put_Line (Subject'Image);
      --  Put_Line (Verb'Image);
      --  Put_Line (Object'Image);

      Action := Get_Action (Prep, Subject_Attr, Subject, Verb, Object);
      IO.Put (Prefix & "Rule = ", Verbosity => IO.Debug);
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
              File_Content => Empty_Text);

   end Parse;

   procedure Put_Keywords_and_Grammar is
   begin
      Lexer.Put_Keywords;
      Parser.Put_Grammar;
   end Put_Keywords_and_Grammar;

end BBT.Scenarios.Step_Parser;
