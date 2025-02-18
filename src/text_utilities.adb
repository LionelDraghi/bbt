-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Equal_Case_Insensitive;

with GNAT.Regexp;

package body Text_Utilities is

   -- --------------------------------------------------------------------------
   function Is_Eq (S1, S2           : String;
                   Case_Insensitive : Boolean) return Boolean is
     ((Case_Insensitive and Ada.Strings.Equal_Case_Insensitive (S1, S2))
      or else S1 = S2);

   -- --------------------------------------------------------------------------
   function Is_Equal (S1, S2           : String;
                      Case_Insensitive : Boolean := True;
                      Ignore_Blanks    : Boolean := True) return Boolean is
   begin
      if Ignore_Blanks then
         return Is_Eq (Join_Spaces (S1), Join_Spaces (S2), Case_Insensitive);

      else
         return Is_Eq (S1, S2, Case_Insensitive);

      end if;
   end Is_Equal;

   -- --------------------------------------------------------------------------
   function Is_Equal (Text1, Text2           : Text;
                      Case_Insensitive : Boolean := True;
                      Ignore_Blanks    : Boolean := True) return Boolean
   is
      use type Ada.Containers.Count_Type;
   begin
      if Text1.Length /= Text2.Length then
         return False;
      end if;

      for I in Text2.First_Index .. Text2.Last_Index loop
         if not Is_Equal (Text1 (I), Text2 (I),
                          Case_Insensitive => Case_Insensitive,
                          Ignore_Blanks    => Ignore_Blanks)
         then
            return False;
         end if;
      end loop;
      return True;
   end Is_Equal;

   -- --------------------------------------------------------------------------
   procedure Create_File (File_Name    : String;
                          With_Content : Text) is
      Output : File_Type;
      pragma Warnings (Off, Output);
   begin
      Create (Output, Out_File, File_Name);
      for L of With_Content loop
         Put_Line (Output, Item => L);
      end loop;
      Close (Output);
   end Create_File;

   -- --------------------------------------------------------------------------
   function Create_File (File_Name    : Unbounded_String;
                         With_Content : Text) return Boolean is
   begin
      Create_File (To_String (File_Name), With_Content);
      return True;
   exception
      when others => return False;
   end Create_File;

   -- --------------------------------------------------------------------------
   procedure Put_Text (File : File_Type := Standard_Output;
                       Item : Text) is
   begin
      for L of Item loop
         Put_Line (File, L);
      end loop;
   end Put_Text;

   --  -- --------------------------------------------------------------------------
   --  procedure Put_Text (Item      : Text;
   --                      File_Name : String) is
   --     File : File_Type;
   --     pragma Warnings (Off, File);
   --  begin
   --     Put_Text (File, Item);
   --     -- Close (File);
   --  end Put_Text;

   -- --------------------------------------------------------------------------
   procedure Put_Text_Head (Item       : Text;
                            File       : File_Type := Standard_Output;
                            Line_Count : Positive) is
      I : Positive := 1;
   begin
      for L of Item loop
         exit when I = Line_Count;
         Put_Line (File, L);
         I := @ + I;
      end loop;
   end Put_Text_Head;

   -- --------------------------------------------------------------------------
   --  procedure Put_Text_Head (Item       : Text;
   --                           File_Name  : String;
   --                           Line_Count : Positive) is
   --     File : File_Type;
   --     pragma Warnings (Off, File);
   --  begin
   --     Open (File, Name => File_Name, Mode => Out_File);
   --     Put_Text_Head (Item, File, Line_Count);
   --     Close (File);
   --  end Put_Text_Head;

   -- --------------------------------------------------------------------------
   procedure Put_Text_Tail (Item       : Text;
                            File       : File_Type := Standard_Output;
                            Line_Count : Positive) is
      I : Positive := 1;
   begin
      for L of reverse Item loop
         exit when I = Line_Count;
         Put_Line (File, L);
         I := @ + I;
      end loop;
   end Put_Text_Tail;

   -- --------------------------------------------------------------------------
   --  procedure Put_Text_Tail (Item       : Text;
   --                           File_Name  : String;
   --                           Line_Count : Positive) is
   --     File : File_Type;
   --     pragma Warnings (Off, File);
   --  begin
   --     Open (File, Name => File_Name, Mode => Out_File);
   --     Put_Text_Tail (Item, File, Line_Count);
   --     Close (File);
   --  end Put_Text_Tail;

   -- --------------------------------------------------------------------------
   function Get_Text (File : File_Type) return Text is
      T : Text := Empty_Text;
   begin
      while not End_Of_File (File) loop
         T.Append (Get_Line (File));
      end loop;
      return T;
   end Get_Text;

   -- --------------------------------------------------------------------------
   function Get_Text (File_Name : String) return Text is
      File : File_Type;
      T    : Text;
   begin
      Open (File, Name => File_Name, Mode => In_File);
      T := Empty_Text;
      begin
         loop
            T.Append (Get_Line (File));
         end loop;
      exception
         when End_Error =>
            null;
      end;
      Close (File);
      return T;
   end Get_Text;

   -- --------------------------------------------------------------------------
   --  function Get_Text (File_Name : Unbounded_String) return Text is
   --    (Get_Text (To_String (File_Name)));
   --
   --  -- --------------------------------------------------------------------------
   --  function Get_Text_Head (From       : Text;
   --                          Line_Count : Positive) return Text is
   --     use Ada.Containers;
   --  begin
   --     if From = Empty_Text then
   --        return Empty_Text;
   --     elsif From.Length < Count_Type (Line_Count) then
   --        return From;
   --     else
   --        declare
   --           Tmp : Text;
   --        begin
   --           for I in From.First_Index .. From.First_Index + Line_Count - 1 loop
   --              Tmp.Append (From (From.First_Index + I - 1));
   --           end loop;
   --           return Tmp;
   --        end;
   --     end if;
   --  end Get_Text_Head;

   --  -- --------------------------------------------------------------------------
   --  function Get_Text_Tail (From       : Text;
   --                          Line_Count : Positive) return Text is
   --     use Ada.Containers;
   --     use Texts;
   --  begin
   --     if From = Empty_Text then
   --        return Empty_Text;
   --     elsif From.Length < Count_Type (Line_Count) then
   --        return From;
   --     else
   --        declare
   --           Tmp : Text := To_Vector (Count_Type (Line_Count));
   --        begin
   --           for I in reverse Tmp.Iterate loop
   --              Tmp (I) := From (From.Last_Index - To_Index (I) + 1);
   --           end loop;
   --           return Tmp;
   --        end;
   --     end if;
   --  end Get_Text_Tail;
   --
   --  -- --------------------------------------------------------------------------
   --  function Shrink (The_Text   : Text;
   --                   Line_Count : Min_Shrinked_Length;
   --                   Cut_Mark   : String := "...") return Text is
   --     use Ada.Containers;
   --     use Texts;
   --  begin
   --     if The_Text = Empty_Text or else
   --       The_Text.Length <= Count_Type (Line_Count) then
   --        return The_Text;
   --
   --     elsif Line_Count = 2 then
   --        return [The_Text (The_Text.First), Cut_Mark];
   --
   --     else
   --        declare
   --           Tmp : Text := To_Vector (Count_Type (Line_Count));
   --           subtype Head_Index is Positive range The_Text.First ..
   --             The_Text.First + Texts.Count (Line_Count / 2 - 1);
   --           subtype Tail_Index is Positive range
   --             Head_Index.Last + 2 .. The_Text.Last;
   --           Shift : constant Positive := The_Text.Last_Index - Tmp.Last_Index;
   --        begin
   --           for I in Head_Index loop
   --              Tmp (I) := The_Text (I);
   --           end loop;
   --           Tmp (Head_Index'Last + 1) := Cut_Mark;
   --           for I in Tail_Index loop
   --              Tmp (I) := The_Text (I);
   --           end loop;
   --           return Tmp;
   --        end;
   --     end if;
   --  end Shrink;

   procedure Sort (The_Text : in out Text) renames Texts_Sorting.Sort;

   -- --------------------------------------------------------------------------
   procedure Compare (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Blanks      : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False;
                      Identical          : out Boolean)
                      -- Diff_Index         : out Natural)
   is
      T1 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text1)
                    else Text1);
      T2 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text2)
                    else Text2);
      -- Diff_Index : Natural;
      use type Ada.Containers.Count_Type;
      Same_Size : Boolean;

   begin
      if Sort_Texts then
         Sort (T1);
         Sort (T2);
      end if;

      Same_Size := T1.Length = T2.Length;
      if not Same_Size then
         Identical := False;

      elsif T1.Length = 0 then
         Identical := True;

      else
         -- Brut compare
         for Diff_Index in T1.First_Index .. T1.Last_Index loop
            Identical := Is_Equal (T1 (Diff_Index),
                                   T2 (Diff_Index),
                                   Case_Insensitive => Case_Insensitive,
                                   Ignore_Blanks    => Ignore_Blanks);
            exit when not Identical;
         end loop;
      end if;

      --  if Identical then
      --     Diff_Index := 0;
      --  end if;

   end Compare;

   -- --------------------------------------------------------------------------
   function Is_Equal (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Blanks      : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False) return Boolean
   is
      Identical  : Boolean;
      -- Diff_Index : Natural;
   begin
      Compare (Text1, Text2,
               Ignore_Blank_Lines => Ignore_Blank_Lines,
               Ignore_Blanks      => Ignore_Blanks,
               Case_Insensitive   => Case_Insensitive,
               Sort_Texts         => Sort_Texts,
               Identical          => Identical);
               -- Diff_Index         => Diff_Index);
      return Identical;
   end Is_Equal;

   -- --------------------------------------------------------------------------
   function Search (Source,
                    Pattern          : String;
                    Case_Insensitive : Boolean := True;
                    Ignore_Blanks    : Boolean := True) return Boolean is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Src : constant String := (if Ignore_Blanks then Join_Spaces (Source)
                                  else Source);
      Pat : constant String := (if Ignore_Blanks then Join_Spaces (Pattern)
                                else Pattern);
   begin
      return (Index (Source  => Src,
                     Pattern => Pat,
                     From    => Src'First) /= 0)
        or else (Case_Insensitive and
                   Index (Source  => Ada.Characters.Handling.To_Lower (Src),
                          Pattern => Ada.Characters.Handling.To_Lower (Pat),
                          From    => Src'First) /= 0);
   end Search;

   -- --------------------------------------------------------------------------
   function Contains (Text1, Text2       : Text;
                      Sort_Texts         : Boolean := False;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Blanks      : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True) return Boolean is
   -- After eliminating easy cases T1 = T2 and T2 is longer than T1, the
   -- comparison algorithm is :
   -- I1 and I2 (in the loop below) are the two cursor respectively
   -- in T1 and T2.
   -- For each line in T1 (until there is enough lines left in T1 to
   -- match all T2), we search for a matching line in T2.
   -- Then, we move I1 and I2 to see if following line in both
   -- text matches also.
   -- If it matches until T2 last lines, return True, false otherwise.
      use type Ada.Containers.Count_Type;
      T1 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text1)
                    else Text1);
      T2 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text2)
                    else Text2);

   begin
      if T1.Length < T2.Length then
         return False;

      elsif T1 = T2 then
         return True;

      else
         declare
            Last_I1 : constant Positive
              := T1.Last_Index - Positive (T2.Length) + 1;
            I1      : Positive;

         begin
            if Sort_Texts then
               Sort (T1);
               Sort (T2);
            end if;

            for Start in T1.First_Index .. Last_I1 loop
               I1 := Start;
               Inner : for I2 in T2.First_Index .. T2.Last_Index loop
                  -- We look for a first match between texts.
                  if Search (T1 (I1), T2 (I2),
                             Case_Insensitive,
                             Ignore_Blanks)
                  then
                     -- Lines match
                     if I2 = T2.Last_Index then
                        -- It was the last line of T2
                        -- => Text match
                        return True;
                     else
                        -- Not the last line of T2, so lets' go to T1 next line
                        -- (T2 next line will be set by the loop)
                        I1 := @ + 1;
                     end if;
                  else
                     exit Inner;
                  end if;
               end loop Inner;

            end loop;
            return False;

         end;
      end if;
   end Contains;

   -- --------------------------------------------------------------------------
   function Contains_Line (The_Text         : Text;
                           The_Line         : String;
                           Case_Insensitive : Boolean := True;
                           Ignore_Blanks    : Boolean := True) return Boolean is
   begin
      for L of The_Text loop
         if Is_Equal (L, The_Line,
                      Case_Insensitive => Case_Insensitive,
                      Ignore_Blanks    => Ignore_Blanks)
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Line;

   -- --------------------------------------------------------------------------
   function Contains_String
     (The_Text         : Text;
      The_String       : String;
      Case_Insensitive : Boolean := True;
      Ignore_Blanks    : Boolean := True) return Boolean is
   begin
      for L of The_Text loop
         if Search (L, The_String,
                    Case_Insensitive => Case_Insensitive,
                    Ignore_Blanks    => Ignore_Blanks)
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains_String;

   -- --------------------------------------------------------------------------
   function Contains_Line
     (File_Name        : String;
      The_Line         : String;
      Case_Insensitive : Boolean := True;
      Ignore_Blanks    : Boolean := True) return Boolean is
   begin
      return Contains_Line (Get_Text (File_Name),
                            The_Line,
                            Case_Insensitive => Case_Insensitive,
                            Ignore_Blanks    => Ignore_Blanks);
   end Contains_Line;

   -- --------------------------------------------------------------------------
   function Contains_String
     (File_Name          : String;
      The_String         : String;
      Case_Insensitive   : Boolean := True;
      Ignore_Blanks      : Boolean := True) return Boolean is
   begin
      return Contains_String (Get_Text (File_Name),
                              The_String,
                              Case_Insensitive => Case_Insensitive,
                              Ignore_Blanks    => Ignore_Blanks);
   end Contains_String;

   -- --------------------------------------------------------------------------
   function Matches (In_Text    : Text;
                     Regexp     : String) -- ;
                     -- Line       : out Natural;
                     -- Matches    : in out GNAT.Regpat.Match_Array)
                     return Boolean
   is
      -- use GNAT.Regpat;
      use GNAT.Regexp;
      -- Matcher : Pattern_Matcher := Compile (Regexp);
      Matcher : GNAT.Regexp.Regexp;
      Line : Natural := 0;

   begin
      -- Line    := 0;
      -- Compile (Matcher, Regexp);
      Matcher := Compile (Regexp);
      --  Put_Line ("regexp = " & Regexp);
      for I in In_Text.First_Index .. In_Text.Last_Index loop
         --  Put_Line ("Line = " & In_Text (I));

         Line := I;
         -- Match (Self => Matcher, Data => In_Text (I), Matches => Matches);
         -- Put_Line ("Matches = " & Matches'Image);
         if Match (In_Text (I), Matcher) then
            -- Put_Line ("Match " & In_Text (I));
            return True;
         end if;
      end loop;
      return False;
   end Matches;

   -- --------------------------------------------------------------------------
   function First_Non_Blank_Line (In_Text : Text;
                                  From    : Positive := 1) return Natural is
   begin
      if In_Text.Last_Index = 0 then
         -- Null Text
         return 0;

      elsif From > In_Text.Last_Index then
         Put_Line ("Non_Blank_Line : starting search outside of Text range");
         Put_Line ("First Non_Blank_Line : From " & From'Image
                   & " not in In_Text range " & In_Text.First_Index'Image
                   & " .. "
                   & In_Text.Last_Index'Image);
      else
         for I in From .. In_Text.Last_Index loop
            -- Put_Line ("First_Non_Blank_Line : I = " & I'Image);
            if Ada.Strings.Fixed.Index_Non_Blank (In_Text (I)) /= 0 then
               -- Character found on that line
               -- Put_Line (" First_Non_Blank_Line returning I = " & I'Image);
               return I;
            end if;
         end loop;
      end if;
      return 0;
   end First_Non_Blank_Line;

   -- --------------------------------------------------------------------------
   function Remove_Blank_Lines (From_Text : Text) return Text is
      T : Text := Empty_Text;
   begin
      for L of From_Text loop
         if Ada.Strings.Fixed.Index_Non_Blank (L) /= 0 then
            T.Append (L);
         end if;
      end loop;
      return T;
   end Remove_Blank_Lines;

   function Join_Spaces (From : String) return String is
      Tmp : String (From'Range);
      I   : Natural := Tmp'First;
   begin
      for J in From'Range loop
         if From (J) /= ' ' then
            Tmp (I) := From (J);
            I := @ + 1;
         end if;
      end loop;
      return Tmp (From'First .. I - 1);
   end Join_Spaces;

end Text_Utilities;
