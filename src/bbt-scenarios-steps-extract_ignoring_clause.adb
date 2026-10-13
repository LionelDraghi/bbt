-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

function Extract_Ignoring_Clause (Line     : String;
                                   Loc      : Location_Type;
                                   Patterns : out Ignore_Pattern_Set)
                                   return String
is
   use Ada.Strings.Maps,
       Ada.Strings.Maps.Constants;

   Clause_Marker : constant String := "(ignoring";

   Lower_Line   : constant String := Translate (Line, Lower_Case_Map);
   Clause_Start : Natural         := 0;
   Clause_End   : Natural;

   procedure Set_Pattern (Word : String) is
   -- Word is a lowercase word of the pattern list
   begin
      if Word = "" or else Word = "and" then
         -- separators, or an empty word, are not patterns
         null;

      elsif Word = "time" then
         Patterns (Time) := True;

      elsif Word = "dir" or else Word = "directory" then
         Patterns (Dir) := True;

      elsif Word = "version" then
         Patterns (Version) := True;

      else
         IO.Put_Error ("unknown ignoring pattern """ & Word
                       & """, supported patterns: time, dir, version", Loc);
      end if;
   end Set_Pattern;

begin
   Patterns := No_Ignore_Pattern;

   -- The clause is searched from the end, so that a command or an
   -- expected text between backticks containing parenthesis is not
   -- mistaken for a clause
   declare
      Pos : Natural := Index (Lower_Line, Clause_Marker);
   begin
      while Pos /= 0 loop
         Clause_Start := Pos;
         Pos := Index (Lower_Line, Clause_Marker, From => Pos + 1);
      end loop;
   end;

   if Clause_Start = 0 then
      return Line;
   end if;

   Clause_End := Index (Line, ")", From => Clause_Start + 1);
   if Clause_End = 0 then
      IO.Put_Error ("missing ) in the ignoring clause", Loc);
      return Line;
   end if;

   if Index_Non_Blank (Line (Clause_End + 1 .. Line'Last)) /= 0 then
      -- something else after the parenthesis: not an ignoring clause
      -- at the end of the step line
      return Line;
   end if;

   -- Parse the pattern list, the words being separated by blanks,
   -- commas, and the "and" word, in any mix
   declare
      List       : constant String :=
        Lower_Line (Clause_Start + Clause_Marker'Length .. Clause_End - 1);
      Separators : constant Character_Set := To_Set (" ,");
      From       : Positive := List'First;
      Last       : Natural;
   begin
      while From <= List'Last loop
         -- jump over the separators
         while From <= List'Last and then Is_In (List (From), Separators) loop
            From := @ + 1;
         end loop;
         exit when From > List'Last;
         Last := From;
         while Last < List'Last and then not Is_In (List (Last + 1), Separators) loop
            Last := @ + 1;
         end loop;
         Set_Pattern (List (From .. Last));
         From := Last + 1;
      end loop;
   end;

   if Patterns = No_Ignore_Pattern and then not IO.Some_Error then
      IO.Put_Error ("no pattern given in the ignoring clause", Loc);
   end if;

   return Line (Line'First .. Clause_Start - 1);

end Extract_Ignoring_Clause;
