-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings;       use Ada.Strings;
-- with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;

with GNAT.Regexp;

package body BBT.Scenarios.Readers.Adoc_Reader is

   -- --------------------------------------------------------------------------
   Processor       : aliased Adoc_Reader;
   Regexp          : constant String := ("*.adoc");
   Compiled_Regexp : constant GNAT.Regexp.Regexp
     := GNAT.Regexp.Compile (Pattern        => Regexp,
                             Glob           => True,
                             Case_Sensitive => False);

   -- --------------------------------------------------------------------------
   Blanks  : constant Character_Set := To_Set (" " & Ada.Characters.Latin_1.HT);
   Separators : constant Character_Set := To_Set (':') or Blanks;
   Decorators : constant Character_Set := To_Set ('#');
   -- AsciiDoc highlight : https://docs.asciidoctor.org/asciidoc/latest/text/highlight/

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Reader     => Processor'Access,
                For_Format => Adoc);
   end Initialize;

   -- --------------------------------------------------------------------------
   function File_Pattern
     (Reader : Adoc_Reader) return String is (Regexp);

   -- --------------------------------------------------------------------------
   function Is_Of_The_Format
     (Reader    : Adoc_Reader;
      File_Name : String) return Boolean is
   begin
      return GNAT.Regexp.Match (S => File_Name,
                                R => Compiled_Regexp);
   end Is_Of_The_Format;

   -- --------------------------------------------------------------------------
   function Remove_Emphasis (Reader    : Adoc_Reader;
                             S         : String) return String is
      Emphasis  : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("*_`");
   begin
      return Trim (S,
                   Left  => Emphasis,
                   Right => Emphasis);
   end Remove_Emphasis;

   -- --------------------------------------------------------------------------
   function Find_Heading_Mark
     (Reader      : Adoc_Reader;
      Line        : String;
      First       : out Natural;
      Last        : out Natural;
      Title_First : out Natural;
      Title_Last  : out Natural;
      Location    : Location_Type) return Boolean is

   -- === **Header** : xyz
   -- First will point 'H'
   -- Last  will point 'r'
   -- Title_First will point 'x'
   -- Title_Last  will point 'z'
   --
   -- Refer to https://docs.asciidoctor.org/asciidoc/latest/sections/titles-and-levels/
   -- for specification

   -- Fixme : This code is very similar to Markdown, and should be factorized.

   begin
      -- First point to the first =
      First := Index_Non_Blank (Source => Line, From => Line'First);

      if Line (First) /= '=' then
         return False;
      end if;

      -- let's jump over all others '='
      First := Index (Source => Line,
                      Set    => Ada.Strings.Maps.To_Set ("="),
                      Test   => Ada.Strings.Outside,
                      From   => First + 1,
                      Going  => Forward);
      -- Now First point to the first character after all '='
      -- First is set, and should not be overwritten from now on

      if Line'Last = First then
         return False;
      end if;

      if Is_In (Line (First), Blanks) then
         -- Let's jump over blanks
         First := Index (Source => Line,
                         Set    => Blanks,
                         Test   => Ada.Strings.Outside,
                         From   => First,
                         Going  => Forward);
      else
         IO.Put_Warning ("Asciidoc expect space in Headings after last '='", Location);
         -- Couldn't find a ref in asciidoc
      end if;

      Find_Token (Source => Line,
                  Set    => Separators,
                  From   => First,
                  Test   => Ada.Strings.Outside,
                  First  => First,
                  Last   => Last);

      -- Last is set, and should not be overwritten from now on

      if Line'Last = Last then
         -- There is noting after "= Scenario" (for example)
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
         -- There is only Separators or Decorators after "= Scenario" (for example)
         -- Put_Line ("only Separators after = Scenario");
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

      -- Put_Line ("Token >" & Line (First .. Last) & "<");

      return True;

   end Find_Heading_Mark;

   -- --------------------------------------------------------------------------
   subtype Marker_String is String (1 .. 4);
   Code_Block_Delimiter : constant Marker_String := 4 * '-';

   function Code_Fence_Line (Reader           : Adoc_Reader;
                             Line             : String;
                             Look_For_Closing : Boolean) return Boolean is
   -- We use the source block syntax
   -- https://docs.asciidoctor.org/asciidoc/latest/verbatim/source-blocks/
   begin
      return Index (Trim (Line, Left),
                    Code_Block_Delimiter) = 1;
   end Code_Fence_Line;

end BBT.Scenarios.Readers.Adoc_Reader;
