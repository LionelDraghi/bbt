-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings;       use Ada.Strings;
-- with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;

with GNAT.Regexp;

package body BBT.Scenarios.Readers.MDG_Reader is

   -- --------------------------------------------------------------------------
   Processor       : aliased MDG_Reader;
   Regexp          : constant String := ("*.md");
   Compiled_Regexp : constant GNAT.Regexp.Regexp
     := GNAT.Regexp.Compile (Pattern        => Regexp,
                             Glob           => True,
                             Case_Sensitive => False);

   -- --------------------------------------------------------------------------
   Blanks  : constant Character_Set := To_Set (" " & Ada.Characters.Latin_1.HT);
   -- cf. https://spec.commonmark.org/0.31.2/#blank-line
   Separators : constant Character_Set := To_Set (':') or Blanks;
   Decorators : constant Character_Set := To_Set ('#'); -- or Markdown_Emphasis
   -- All surrounding chars that should be trimmed

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Reader     => Processor'Access,
                For_Format => MDG);
   end Initialize;

   -- --------------------------------------------------------------------------
   function File_Pattern
     (Reader : MDG_Reader) return String is (Regexp);

   -- --------------------------------------------------------------------------
   function Is_Of_The_Format
     (Reader    : MDG_Reader;
      File_Name : String) return Boolean is
   begin
      return GNAT.Regexp.Match (S => File_Name,
                                R => Compiled_Regexp);
   end Is_Of_The_Format;

   -- --------------------------------------------------------------------------
   function Remove_Emphasis (Reader    : MDG_Reader;
                             S         : String) return String is
      Emphasis  : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("*_");
   begin
      return Trim (S,
                   Left  => Emphasis,
                   Right => Emphasis);
   end Remove_Emphasis;

   -- --------------------------------------------------------------------------
   function Find_Heading_Mark
     (Reader      : MDG_Reader;
      Line        : String;
      First       : out Natural;
      Last        : out Natural;
      Title_First : out Natural;
      Title_Last  : out Natural;
      Location    : Location_Type) return Boolean is

   -- In
   -- ### **Header** : xyz
   -- First will point 'H'
   -- Last  will point 'r'
   -- Title_First will point 'x'
   -- Title_Last  will point 'z'
   --
   -- Refer to https://spec.commonmark.org/0.31.2/#atx-heading
   -- for specification
   begin
      -- First point to the first  #
      First := Index_Non_Blank (Source => Line, From => Line'First);

      if Line (First) /= '#' then
         return False;
      end if;

      -- let's jump over all others '#'
      First := Index (Source => Line,
                      Set    => Ada.Strings.Maps.To_Set ("#"),
                      Test   => Ada.Strings.Outside,
                      From   => First + 1,
                      Going  => Forward);
      -- Now First point to the first character after all '#'
      -- First is set, and should not be overwritten from now on

      if Line'Last = First then
         return False;
      end if;

      if Is_In (Line (First), Blanks) then
         -- let's jump over blanks
         First := Index (Source => Line,
                         Set    => Blanks,
                         Test   => Ada.Strings.Outside,
                         From   => First,
                         Going  => Forward);
      else
         IO.Put_Warning ("Markdown expect space in Headings after last '#'", Location);
         --  IO.Put_Warning ("First = " & First'Image);
         --  IO.Put_Warning ("Line  = " & Line);
         --  IO.Put_Warning ("Line (First) = " & """" & Line (First)'Image & """");
      end if;

      Find_Token (Source => Line,
                  Set    => Separators,
                  From   => First,
                  Test   => Ada.Strings.Outside,
                  First  => First,
                  Last   => Last);

      -- Last is set, and should not be overwritten from now on

      if Line'Last = Last then
         -- There is noting after "# Scenario" (for example)
         Title_First := 1;
         Title_Last  := 0;
         return True;
      end if;

      -- Now we hare in the Title
      Title_First := Index (Source => Line,
                            Set    => Separators or Decorators,
                            Test   => Ada.Strings.Outside,
                            From   => Last + 1,
                            Going  => Forward);
      if Title_First = 0 then
         -- There is only Separators or Decorators after "# Scenario" (for example)
         -- Put_Line ("only Separators after # Scenario");
         Title_First := 1;
         Title_Last  := 0;
         return True;
      end if;

      Title_Last := Index (Source => Line,
                           Set    => Decorators or Blanks,
                           Test   => Ada.Strings.Outside,
                           From   => Line'Last,
                           Going  => Backward);
      if Title_Last = 0 then
         -- nothing to remove at the end
         Title_Last := Line'Last;
      end if;

      return True;

   end Find_Heading_Mark;

   -- --------------------------------------------------------------------------
   subtype Marker_String is String (1 .. 3);
   Tilda_Fence_Marker    : constant Marker_String := 3 * '~';
   Backtick_Fence_Marker : constant Marker_String := 3 * '`';
   No_Marker             : constant Marker_String := 3 * ' ';
   Current_Fence_Marker  : Marker_String := No_Marker;
   -- Stores what marker was used to open the block
   -- This allows to ignore code block inserted with the other marker

   function Code_Fence_Line (Reader           : MDG_Reader;
                             Line             : String;
                             Look_For_Closing : Boolean) return Boolean is
   begin
      if Look_For_Closing then
         -- Looking for the closing marker
         -- The closing marker must be equal to the opening one
         if Index (Trim (Line, Left),
                   Current_Fence_Marker) = 1
         then
            -- this is the closing marker, let's reset
            Current_Fence_Marker := No_Marker;
            return True;
         else
            return False;
         end if;

      else
         -- The opening marker is one of the two possibility
         if Index (Trim (Line, Left), Tilda_Fence_Marker) = 1 then
            Current_Fence_Marker := Tilda_Fence_Marker;
            return True;
         elsif Index (Trim (Line, Left), Backtick_Fence_Marker) = 1 then
            Current_Fence_Marker := Backtick_Fence_Marker;
            return True;
         else
            return False;
         end if;

      end if;

   end Code_Fence_Line;

end BBT.Scenarios.Readers.MDG_Reader;
