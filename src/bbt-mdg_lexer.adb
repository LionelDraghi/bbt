-- Header TBD

with Ada.Characters.Latin_1;
-- with Ada.Containers.Indefinite_Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Maps;

with BBT.IO;
with BBT.Settings;

package body BBT.MDG_Lexer is

   -- --------------------------------------------------------------------------
   -- IO renamed with local Topic
   procedure Put_Line
     (Item  : String;
      File  : String  := "";
      Line  : Integer := 0;
      Level : Settings.Print_Out_Level := IO.Normal;
      Topic : Settings.Extended_Topics := Settings.Lexer) renames IO.Put_Line;

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
                      Test   => Ada.Strings.Outside,
                      From   => Line'First,
                      Going  => Forward);
      -- First point to the first character after #
      if First /= 0 then
         Colon := Index (Source  => Line,
                         Pattern => ":",
                         From    => First,
                         Going   => Forward);
         if Colon = 0 then
            return False;

         else
            Find_Token (Source => Line (First .. Colon - 1),
                        Set    => Ada.Strings.Maps.To_Set ("* "),
                        -- If the Header is surrounded by bold marks,
                        -- or spaces, lets remove it
                        Test   => Ada.Strings.Outside,
                        First  => First,
                        Last   => Last);
            --  Put_Line ("Find_Heading_Mark within """ & Line & """, First = "
            --            & First'Image & ", Last =" & Last'Image);
            if Colon = Line'Last then
               -- pathological case where the line ends on the colon character
               -- (no title after)
               Colon_Succ := Colon;
            else
               Colon_Succ := Colon + 1;
            end if;

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
     ((In_Code_Fence => False));

   -- --------------------------------------------------------------------------
   function Parse_Line (Line    : access constant String;
                        Context : in out Parsing_Context)
                        return Line_Attributes is
      First, Last, Title_First : Natural;
   begin
      Put_Line ("Parsing = """ & Line.all & """", Level => IO.Debug);
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

      elsif Bullet_List_Marker (Line.all, First) then
         -- Step line ----------------------------------------------------------
         --  Put_Line ("List Mark line = "
         --               & Line.all (First .. Line.all'Last),
         --               Level => IO.Debug);
         return (Kind      => Step_Line,
                 Step_Ln   => To_Unbounded_String
                   (Line.all (First .. Line.all'Last)));

      elsif Find_Heading_Mark (Line       => Line.all (First .. Line.all'Last),
                               First      => First,
                               Last       => Last,
                               Colon_Succ => Title_First) then
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
            Put_Line ("Header = """ & Header & """, Title = """ & Title & """",
                      Level => IO.Debug);
            if Ada.Strings.Equal_Case_Insensitive (Header, "Feature") then
               -- Feature line -------------------------------------------------
               --  IO.Put ("Feature line = " & Line.all (First .. Last),
               --          Level => IO.Debug);
               return (Kind => Feature_Line,
                       Name => To_Unbounded_String (Title));

            elsif Ada.Strings.Equal_Case_Insensitive (Header, "Scenario") or
            else Ada.Strings.Equal_Case_Insensitive (Header, "Example") then
               -- Scenario line ------------------------------------------------
               --  Put_Line ("Scenario line = " & Line.all (First .. Last),
               --               Level => IO.Debug);
               return (Kind => Scenario_Line,
                       Name => To_Unbounded_String (Title));
            else
               -- WTF Header ---------------------------------------------------
               Put_Line ("Unkown Header = """ & Header
                         & ", should be Features or Scenario",
                         Level => IO.Debug);
            end if;
         end;
      end if;

      -- Text line -------------------------------------------------------------
      return (Kind => Text_Line,
              Line => To_Unbounded_String (Line.all));

   end Parse_Line;

   --  -- --------------------------------------------------------------------------
   --  function Is_A_Keyword (S      : access constant String;
   --                         First  : Positive;
   --                         Last   : Natural := 0
   --                        ) return Boolean is
   --  begin
   --     return Keywords.Contains
   --       (Translate (Source  => S.all (First .. Last),
   --                   Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map));
   --  end Is_A_Keyword;


end BBT.MDG_Lexer;
