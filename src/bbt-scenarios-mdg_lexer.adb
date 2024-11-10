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

package body BBT.Scenarios.MDG_Lexer is

   -- --------------------------------------------------------------------------
   Blanks  : constant Character_Set := To_Set (" " & Ada.Characters.Latin_1.HT);
   -- cf. https://spec.commonmark.org/0.31.2/#blank-line

   Bold_And_Blanks : constant Character_Set := To_Set ('*') or Blanks;

   Separators : constant Character_Set := To_Set (':') or Blanks;

   Markdown_Emphasis : constant Character_Set := To_Set ("*_");
   -- Markdown treats asterisks (*) and underscores (_) as indicators of emphasis

   Decorators : constant Character_Set := To_Set ('#'); -- or Markdown_Emphasis
   -- All surrounding chars that should be trimmed

   -- --------------------------------------------------------------------------
   function Find_Heading_Mark (Line        : String;
                               First       : out Natural;
                               Last        : out Natural;
                               Title_First : out Natural;
                               Title_Last  : out Natural;
                               Location    : Location_Type) return Boolean is
      --  In
      --  ### **Header** : text
      --  First will point 'H'
      --  Last  will point 'r'
      --  Colon will point after ':'
      --
      -- refer to https://spec.commonmark.org/0.31.2/#atx-heading
      -- for specification
      Colon : Natural;
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
      end if;

      --  Put_Line ("before Find_Token """ & Line &
      --               """, First = " & First'Image);
      Find_Token (Source => Line,
                  Set    => Separators,
                  From   => First,
                  Test   => Ada.Strings.Outside,
                  First  => First,
                  Last   => Last);
      --  Put_Line ("after Find_Token """ & Line &
      --               """, First = " & First'Image &
      --               ", Line [" & First'Image & "] = " & Line (First) &
      --               ", Last  = " & Last'Image &
      --               ", Token = """ & Line (First .. Last) & """");

      -- Last is set, and should not be overwritten from now on

      if Line'Last = Last then
      -- There is noting after "# Scenario" (for example)
         -- Put_Line ("noting after # Scenario");
         Title_First := 1;
         Title_Last  := 0;
         return True;
      end if;

      -- Now we hare in the Title

      --  Put_Line ("before Find Title, Token = " & Line (First .. Last) &
      --               ", Title = " & Line (Title_First .. Title_Last)
      --               );

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
   function Bullet_List_Marker (Line  : String;
                                First : out Natural) return Boolean is
      -- refer to https://spec.commonmark.org/0.31.2/#bullet-list-marker
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
   function Code_Fence_Line (Line : String) return Boolean is
     (Index (Trim (Line, Left), "```") = 1);
   -- We intentionally ignore "~~~"

   -- --------------------------------------------------------------------------
   function Initialize_Context return Parsing_Context is
     ((In_Code_Fence => False, In_Scenario => False));

   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      A      :        Line_Attributes) is
   begin
      Output.Put (Line_Kind'Image (A.Kind) & " | ");
      case A.Kind is
         when Feature_Line    |
              Scenario_Line   |
              Background_Line => Output.Put (A.Name'Image);
         when Text_Line       => Output.Put (A.Line'Image);
         when Step_Line       => Output.Put (A.Step_Ln'Image);
         when Code_Fence      |
              Empty_Line      => null;
      end case;
   end Put_Image;

   -- --------------------------------------------------------------------------
   function Parse_Line (Line    : access constant String;
                        Context : in out Parsing_Context;
                        Loc     : Location_Type)
                        return Line_Attributes is
      First, Last, Title_First, Title_Last : Natural;
   begin
      --  IO.Put_Line ("Line  = """ & Line.all & """",
      --               Location  => Loc,
      --               Verbosity => IO.Debug);
      Find_Token (Source => Line.all,
                  Set    => Blanks,
                  Test   => Ada.Strings.Outside,
                  First  => First,
                  Last   => Last);

      if Last = 0 then
         -- Null line ----------------------------------------------------------
         --  Put_Line ("null line ",
         --               Level => IO.Debug);
         return (Kind => Empty_Line);

      elsif Code_Fence_Line (Line.all) then
         -- Code_Block Mark ----------------------------------------------------
         --  Put_Line ("BlockCode mark = " & Line.all,
         --               Level => IO.Debug);
         Context.In_Code_Fence := not @;
         return (Kind => Code_Fence);

      elsif Context.In_Code_Fence then
         -- While in code fence, al is considered as text (otherwise a comment
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

      elsif Find_Heading_Mark (Line        => Line.all (First .. Line.all'Last),
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
               -- Feature line -------------------------------------------------
               --  IO.Put ("Feature line = " & Line.all (First .. Last),
               --          Level => IO.Debug);
               return (Kind => Feature_Line,
                       Name => To_Unbounded_String (Title));

            elsif Ada.Strings.Equal_Case_Insensitive (Header, "Scenario")
              or else Ada.Strings.Equal_Case_Insensitive (Header, "Example")
            then
               -- Scenario line ------------------------------------------------
               --  Put_Line ("Scenarios.line = " & Line.all (First .. Last),
               --               Level => IO.Debug);
               Context.In_Scenario := True;
               return (Kind => Scenario_Line,
                       Name => To_Unbounded_String (Title));

            elsif Ada.Strings.Equal_Case_Insensitive (Header, "Background") then
               -- Background Scenario line -------------------------------------
               --  Put_Line ("Background scenario line = "
               --            & Line.all (First .. Last),
               --               Level => IO.Debug);
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

end BBT.Scenarios.MDG_Lexer;
