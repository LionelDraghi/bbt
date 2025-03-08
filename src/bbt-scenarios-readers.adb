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
   subtype Marker_String is String (1 .. 3);
   Tilda_Fence_Marker    : constant Marker_String := 3 * '~';
   Backtick_Fence_Marker : constant Marker_String := 3 * '`';
   No_Marker             : constant Marker_String := 3 * ' ';
   Current_Fence_Marker  : Marker_String := No_Marker;
   -- Stores what marker was used to open the block
   -- This allows to ignore code block inserted with the other marker
   function Code_Fence_Line (Line             : String;
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
            --  Put_Line ("Header = """ & Header & """, Title = """ & Title & """",
            --            Level => IO.Debug);
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
               -- WTF Header ---------------------------------------------------
               Put_Line ("Unknown Header = " & Header'Image
                         & ", should be Features or Scenario",
                         Location  => Loc,
                         Verbosity => IO.Debug);
            end if;
         end;
      end if;

      -- Text line -------------------------------------------------------------
      return (Kind => Text_Line,
              Line => To_Unbounded_String (Line.all));

   end Parse_Line;

   -- --------------------------------------------------------------------------
   procedure Register (Reader     : Interface_Access;
                       For_Format : Valid_Input_Format) is
   begin
      Reader_List (For_Format) := Reader;
   end Register;

end BBT.Scenarios.Readers;
