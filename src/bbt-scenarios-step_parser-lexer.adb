-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Text_Utilities; use Text_Utilities;

with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Strings.Maps;

package body BBT.Scenarios.Step_Parser.Lexer is

   Cursor         : Natural := 1;
   Line_Finished  : Boolean := False;
   Backtick       : constant Character := '`';
   The_Delimiters : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" _*()" & Ada.Characters.Latin_1.HT);

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
         "dir",
         "directory",
         "unordered"];
   -- NB : all keywords must be here in lower case!

   -- -----------------------------------------------------------------------
   function Is_A_Keyword (S      : access constant String;
                          First  : Positive;
                          Last   : Natural := 0)
                             return Boolean
   is
   -- We first remove potential emphasis on keyword
   -- [](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis)
      Emphasis  : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("*_");
      Trimmed   : constant String := Trim (S.all (First .. Last),
                                           Left  => Emphasis,
                                           Right => Emphasis);
      Lower     : constant String := Translate
        (Source  => Trimmed,
         Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
   begin
      -- Put_Line ("K = " & S.all (First .. Last) & ", Trimmed = " & Trimmed);
      return Keywords.Contains (Lower);
   end Is_A_Keyword;

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
   procedure Put_Keywords is
   begin
      for K of Keywords loop
         Ada.Text_IO.Put_Line ("- " & K);
      end loop;
   end Put_Keywords;

end BBT.Scenarios.Step_Parser.Lexer;
