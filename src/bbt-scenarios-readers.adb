-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;

package body BBT.Scenarios.Readers is

   -- --------------------------------------------------------------------------
   Reader_List : array (Valid_Input_Format) of Interface_Access;

   -- --------------------------------------------------------------------------
   Blanks  : constant Character_Set := To_Set (" " & Ada.Characters.Latin_1.HT);
   -- cf. https://spec.commonmark.org/0.31.2/#blank-line

   -- Bold_And_Blanks : constant Character_Set := To_Set ('*') or Blanks;

   -- Separators : constant Character_Set := To_Set (':') or Blanks;

   -- Markdown_Emphasis : constant Character_Set := To_Set ("*_");
   -- Markdown treats asterisks (*) and underscores (_) as indicators of emphasis

   -- Decorators : constant Character_Set := To_Set ('#'); -- or Markdown_Emphasis
   -- All surrounding chars that should be trimmed

   -- --------------------------------------------------------------------------
   function Bullet_List_Marker (Line  : String;
                                First : out Natural) return Boolean is
   -- Refer to https://spec.commonmark.org/0.31.2/#bullet-list-marker
   -- for specification.
   -- We intentionally limit bullet to '-', so that bullet list may appear in
   -- comments, providing you use '*' or '+', without interfering with bbt.

   -- Same for AsciiDoc, '*' and '-' works, but bbt only uses the first.
   -- Cf. https://docs.asciidoctor.org/asciidoc/latest/lists/unordered/

   begin
      First := Index_Non_Blank (Line, Going => Forward);
      if Line'Last - First < 2 then
         -- pathological line of this kind :
         -- "   - "
         return False;
      end if;

      if Line (First) = '-' and Line (First + 1) = ' '
      then
         -- test the space after the list mark, because
         -- "* text" is a list entry
         -- "*text" isn't
         First := Index_Non_Blank (Line, From => First + 1, Going => Forward);
         return True;
      else
         return False;
      end if;
   end Bullet_List_Marker;

   -- --------------------------------------------------------------------------
   function Format (File_Name : String) return Valid_Input_Format is
   begin
      for Reader of Reader_List loop
         if Is_Of_The_Format (Reader.all, File_Name) then
            return Format (Reader.all);
         end if;
      end loop;
      IO.Put_Warning ("Defaulting to MDG format for file """ & File_Name & """");
      return MDG; -- If the file was not recognized by the Readers,
                  -- let's fallback on the default format.
   end Format;

   -- --------------------------------------------------------------------------
   function File_Pattern (For_Format : Valid_Input_Format) return String is
     (File_Pattern (Reader_List (For_Format).all));
   -- Dispatching call

   -- --------------------------------------------------------------------------
   function Initialize_Context return Parsing_Context is
     ((In_Code_Fence => False, In_Scenario => False));

   -- --------------------------------------------------------------------------
   function Parse_Line (Line       : access constant String;
                        For_Format :        Valid_Input_Format;
                        Context    : in out Parsing_Context;
                        Loc        :        Location_Type)
                        return Line_Attributes is
      First, Last, Title_First, Title_Last : Natural;
   begin
      Find_Token (Source => Line.all,
                  Set    => Blanks,
                  Test   => Ada.Strings.Outside,
                  First  => First,
                  Last   => Last);

      if Last = 0 then
         -- Null line ----------------------------------------------------------
         return (Kind => Empty_Line);

      elsif Code_Fence_Line (Line             => Line.all,
                             For_Format       => For_Format,
                             Look_For_Closing => Context.In_Code_Fence)
      then
         Context.In_Code_Fence := not @;
         return (Kind => Code_Fence);

      elsif Context.In_Code_Fence then
         -- While in code fence, all is considered as text (otherwise a comment
         -- with a bullet marker '-' would be interpreted as a Step).
         return (Kind => Text_Line,
                 Line => To_Unbounded_String (Line.all));

      elsif Context.In_Scenario
        and then Bullet_List_Marker (Line.all, First)
      then
         -- Step line ----------------------------------------------------------
         -- We don't take into account bullet list before being in a Scenario.
         -- to let the use most Markdown possibilities before Steps, for
         -- example to Have a TOC, or just better comments.

         --  Put_Line ("List Mark line = "
         --               & Line.all (First .. Line.all'Last),
         --               Level => IO.Debug);
         return (Kind      => Step_Line,
                 Step_Ln   => To_Unbounded_String
                   (Line.all (First .. Line.all'Last)));

      elsif Find_Heading_Mark (Reader      => Reader_List (For_Format).all,
                               Line        => Line.all (First .. Line.all'Last),
                               First       => First,
                               Last        => Last,
                               Title_First => Title_First,
                               Title_Last  => Title_Last,
                               Location    => Loc)
      then
         -- There is a header --------------------------------------------------
         declare
            Header : constant String := Line.all (First .. Last);
            Title  : constant String := Line.all (Title_First .. Title_Last);
         begin
            -- Put_Line ("Header = """ & Header & """, Title = """ & Title & """");
            if Ada.Strings.Equal_Case_Insensitive (Header, "Feature") then
               return (Kind => Feature_Line,
                       Name => To_Unbounded_String (Title));

            elsif Ada.Strings.Equal_Case_Insensitive (Header, "Scenario")
              or else Ada.Strings.Equal_Case_Insensitive (Header, "Example")
            then
               Context.In_Scenario := True;
               return (Kind => Scenario_Line,
                       Name => To_Unbounded_String (Title));

            elsif Ada.Strings.Equal_Case_Insensitive (Header, "Background") then
               Context.In_Scenario := True;
               return (Kind => Background_Line,
                       Name => To_Unbounded_String (Title));

            else
               -- Non processed header
               Put_Line ("Header = """ & Header'Image & """ ignored by bbt",
                         Location  => Loc,
                         Verbosity => IO.Debug);
            end if;
         end;
      end if;

      -- Text line -------------------------------------------------------------
      return (Kind => Text_Line,
              Line => To_Unbounded_String (Line.all));

   end Parse_Line;

   function Code_Fence_Line (Line             : String;
                             For_Format       : Valid_Input_Format;
                             Look_For_Closing : Boolean) return Boolean is
   begin
      return Code_Fence_Line (Reader           => Reader_List (For_Format).all,
                              Line             => Line,
                              Look_For_Closing => Look_For_Closing);
   end Code_Fence_Line;

   -- --------------------------------------------------------------------------
   procedure Register (Reader     : Interface_Access;
                       For_Format : Valid_Input_Format) is
   begin
      Reader_List (For_Format) := Reader;
   end Register;

end BBT.Scenarios.Readers;
