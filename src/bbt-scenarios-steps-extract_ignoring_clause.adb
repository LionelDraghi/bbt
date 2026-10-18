-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

separate (BBT.Scenarios.Steps)

function Extract_Ignoring_Clause (Line     : String;
                                   Loc      : Location_Type;
                                   Patterns : out Ignore_Pattern_Set;
                                   Regexps  : out Ignore_Regexp_List;
                                   Order    : out Boolean)
                                   return String
is
   use Ada.Strings.Maps,
       Ada.Strings.Maps.Constants;

   Clause_Marker : constant String := "(ignoring";

   Lower_Line   : constant String := Translate (Line, Lower_Case_Map);
   Clause_Start : Natural         := 0;
   Clause_End   : Natural;
   -- The casing, whitespaces and blank_lines patterns state the matching
   -- settings: no state to return, the comparison settings keep the
   -- command line values; a contradiction only raises a warning.
   -- Order, on the contrary, is a per-step setting, returned in Order.
   Settings_Pattern : Boolean := False;

   procedure Check_Setting (Word          : String;
                            Setting       : Boolean;
                            Option_Symbol : String) is
   -- Warn when the clause asks to ignore what the command line options
   -- require to check: the option takes priority over the clause, and
   -- the divergence must be visible instead of silent
   begin
      Settings_Pattern := True;
      if not Setting then
         IO.Put_Warning ("the (ignoring " & Word & ") clause is ignored:"
                          & " the " & Option_Symbol
                          & " option takes priority over the scenario"
                          & " clause", Loc);
      end if;
   end Check_Setting;

   function Is_Legal_Regexp (S : String) return Boolean is
   -- Compiles S, to reject an illegal regexp at parsing time,
   -- before any run
      use GNAT.Regpat;
   begin
      -- An exception raised in a declarative part is not handled by
      -- the enclosing body handlers, hence the inner block
      declare
         Matcher : constant Pattern_Matcher := Compile (S);
         pragma Unreferenced (Matcher);
      begin
         return True;
      end;
   exception
      when Expression_Error =>
         return False;
   end Is_Legal_Regexp;

   procedure Set_Pattern (Word    : String;
                          Original : String) is
   -- Word is a lowercase word of the pattern list, Original the same
   -- word in the original casing, that a regexp pattern must keep
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

      elsif Word = "order" then
         Order := True;

      elsif Word = "casing" then
         Check_Setting ("casing",
                        Setting       => Settings.Ignore_Casing,
                        Option_Symbol => "--exact_match");

      elsif Word = "whitespaces" then
         Check_Setting ("whitespaces",
                        Setting       => Settings.Ignore_Whitespaces,
                        Option_Symbol => "--exact_match");

      elsif Word = "blank_lines" then
         Check_Setting ("blank_lines",
                        Setting       => Settings.Ignore_Blank_Lines,
                        Option_Symbol => "--exact_match");

      else
         -- Any other word is a custom pattern: a regexp masking the
         -- matching substrings of both sides
         if Is_Legal_Regexp (Original) then
            Regexps.Append (Original);
         else
            IO.Put_Error ("invalid regexp in the ignoring clause: """
                          & Original & """", Loc);
         end if;
      end if;
   end Set_Pattern;

begin
   Patterns := No_Ignore_Pattern;
   Regexps  := No_Ignore_Regexp;
   Order    := False;

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
      Original_List : constant String :=
        Line (Clause_Start + Clause_Marker'Length .. Clause_End - 1);
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
         Set_Pattern (List (From .. Last), Original_List (From .. Last));
         From := Last + 1;
      end loop;
   end;

   if Patterns = No_Ignore_Pattern and then Regexps.Is_Empty
     and then not Order and then not Settings_Pattern
     and then not IO.Some_Error
   then
      IO.Put_Error ("no pattern given in the ignoring clause", Loc);
   end if;

   return Line (Line'First .. Clause_Start - 1);

end Extract_Ignoring_Clause;
