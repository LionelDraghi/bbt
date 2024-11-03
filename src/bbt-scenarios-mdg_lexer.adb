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
with Ada.Strings.Maps;

package body BBT.Scenarios.MDG_Lexer is

   -- --------------------------------------------------------------------------
   Blanks  : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" " & Ada.Characters.Latin_1.HT);

   -- --------------------------------------------------------------------------
   function Find_Heading_Mark (Line       : String;
                               First      : out Natural;
                               Last       : out Natural;
                               Colon_Succ : out Natural) return Boolean is
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
      First := Index (Source => Line,
                      Set    => Ada.Strings.Maps.To_Set ("#"),
                      Test   => Ada.Strings.Inside,
                      From   => Line'First,
                      Going  => Forward);
      -- First point to the first  #
      if First /= 0 then
         First := Index (Source => Line,
                         Set    => Ada.Strings.Maps.To_Set ("# "),
                         Test   => Ada.Strings.Outside,
                         From   => First,
                         Going  => Forward);
         -- Now First point to the first non blank character after all '#'
         Colon := Index (Source  => Line,
                         Pattern => ":",
                         From    => First,
                         Going   => Forward);
         if Colon = 0 then
            --  Put_Line ("no Heading_Mark found within """ & Line);
            return False;

         else
            Find_Token (Source => Line (First .. Colon - 1),
                        Set    => Ada.Strings.Maps.To_Set ("* "),
                        -- If the Header is surrounded by bold marks,
                        -- or spaces, lets remove it
                        Test   => Ada.Strings.Outside,
                        First  => First,
                        Last   => Last);
            if Colon = Line'Last then
               -- pathological case where the line ends on the colon character
               -- (no title after)
               Colon_Succ := Colon;
            else
               Colon_Succ := Colon + 1;
            end if;

            --  Put_Line ("Find_Heading_Mark within """ & Line & """, First = "
            --            & First'Image & ", Last =" & Last'Image);
            return True;

         end if;

      else
         return False;

      end if;

   end Find_Heading_Mark;

   -- --------------------------------------------------------------------------
   function Bullet_List_Marker (Line  : String;
                                First : out Natural) return Boolean is
      -- refer to https://spec.commonmark.org/0.31.2/#bullet-list-marker
      -- for specification.
      -- We voluntary limit bullet to '-', so that bullet list may appear in
      -- comments, providing you use '*' or '+', without interfering with bbt.

   begin
      First := Index_Non_Blank (Line, Going => Forward);
      if Line'Last - First < 2 then
         -- pathological line of this kind :
         -- "   - "
         return False;
      end if;

      if Line (First) = '-' -- or Line (First) = '*'  or Line (First) = '+')
        and Line (First + 1) = ' '
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
     (Index (Trim (Line, Left), "```") = 1 or else
     Index (Trim (Line, Left), "~~~") = 1);
   -- refer to https://spec.commonmark.org/0.31.2/#fenced-code-block
   -- for specification

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
      First, Last, Title_First : Natural;
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

      elsif Find_Heading_Mark (Line       => Line.all (First .. Line.all'Last),
                               First      => First,
                               Last       => Last,
                               Colon_Succ => Title_First)
      then
         -- There is a header --------------------------------------------------
         declare
            Header : constant String :=
                       Trim (Line.all (First .. Last),
                             Left  => Blanks,
                             Right => Blanks);
            Title  : constant String :=
                       Trim (Line.all (Title_First + 1 .. Line.all'Last),
                             Left  => Blanks,
                             Right => Blanks);
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
