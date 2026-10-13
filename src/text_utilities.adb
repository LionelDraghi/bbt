-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Equal_Case_Insensitive;

with GNAT.OS_Lib;
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

   --  -- --------------------------------------------------------------------------
   --  function Is_Equal (Text1, Text2     : Text;
   --                     Case_Insensitive : Boolean := True;
   --                     Ignore_Blanks    : Boolean := True) return Boolean
   --  is
   --     use type Ada.Containers.Count_Type;
   --  begin
   --     if Text1.Length /= Text2.Length then
   --        return False;
   --     end if;

   --     for I in Text2.First_Index .. Text2.Last_Index loop
   --        if not Is_Equal (Text1 (I), Text2 (I),
   --                         Case_Insensitive => Case_Insensitive,
   --                         Ignore_Blanks    => Ignore_Blanks)
   --        then
   --           return False;
   --        end if;
   --     end loop;
   --     return True;
   --  end Is_Equal;

   -- --------------------------------------------------------------------------
   procedure Create_File (File_Name    : String;
                          With_Content : Text;
                          Executable   : Boolean) is
      Output : File_Type;
      pragma Warnings (Off, Output);
   begin
      Create (Output, Out_File, File_Name);
      for L of With_Content loop
         Put_Line (Output, Item => L);
      end loop;
      Close (Output);
      if Executable then GNAT.OS_Lib.Set_Executable (File_Name); end if;
   end Create_File;

   -- --------------------------------------------------------------------------
   function Create_File (File_Name    : Unbounded_String;
                         With_Content : Text;
                         Executable   : Boolean) return Boolean is
   begin
      Create_File (To_String (File_Name), With_Content, Executable);
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

   -- --------------------------------------------------------------------------
   function To_String (T : Text) return String is
      Result : Unbounded_String;
   begin
      for Line of T loop
         Result := Result & Line & Ada.Characters.Latin_1.LF;
      end loop;
      return To_String (Result);
   end To_String;

   --  -- --------------------------------------------------------------------------
   --  procedure Put_Text (Item      : Text;
   --                      File_Name : String) is
   --     File : File_Type;
   --     pragma Warnings (Off, File);
   --  begin
   --     Put_Text (File, Item);
   --     -- Close (File);
   --  end Put_Text;

   --  -- --------------------------------------------------------------------------
   --  procedure Put_Text_Head (Item       : Text;
   --                           File       : File_Type := Standard_Output;
   --                           Line_Count : Positive) is
   --     I : Positive := 1;
   --  begin
   --     for L of Item loop
   --        exit when I = Line_Count;
   --        Put_Line (File, L);
   --        I := @ + I;
   --     end loop;
   --  end Put_Text_Head;

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

   --  -- --------------------------------------------------------------------------
   --  procedure Put_Text_Tail (Item       : Text;
   --                           File       : File_Type := Standard_Output;
   --                           Line_Count : Positive) is
   --     I : Positive := 1;
   --  begin
   --     for L of reverse Item loop
   --        exit when I = Line_Count;
   --        Put_Line (File, L);
   --        I := @ + I;
   --     end loop;
   --  end Put_Text_Tail;

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
      T    : Text := Empty_Text;
   begin
      Open (File, Name => File_Name, Mode => In_File);
      begin
         loop
            T.Append (Get_Line (File));
         end loop;
      exception
         when End_Error => null;
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
   -- Helper function to truncate a string to a maximum length
   --  function Truncate_To (S : String; Max_Len : Line_Length) return String is
   --  begin
   --     if S'Length <= Max_Len then
   --        return S;
   --     else
   --        return S (S'First .. S'First + Natural (Max_Len) - 1);
   --     end if;
   --  end Truncate_To;

   -- Helper function to create a padding string
   function Pad_To (S : String; Length : Line_Length) return String is
   begin
      --  Head pads with spaces when Length > S'Length,
      --  and truncates when Length < S'Length
      return Ada.Strings.Fixed.Head (Source => S,
                                     Count  => Natural (Length));
   end Pad_To;

   -- --------------------------------------------------------------------------
   function Side_By_Side (T1, T2            : Text;
                          Case_Insensitive   : Boolean := True;
                          Ignore_Whitespaces : Boolean := True) return Text is
      Sep_Diff       : constant String := " | ";
      Sep_Only_Left  : constant String := " < ";
      Sep_Only_Right : constant String := " > ";
      Sep_Same       : constant String := "   ";

      T1_Max_Len : constant Line_Length := Max_Line_Length (T1);
      Sep_Col    : constant Line_Length := T1_Max_Len + 1;

      --  Line_Index is Positive based, and Last_Index is 0 on an empty Text,
      --  so these two constants shall be computed on Line_Index'Base,
      --  otherwise comparing an empty Text raises a CONSTRAINT_ERROR.
      Last_Common_Index : constant Line_Index'Base :=
        Line_Index'Min (T1.Last_Index, T2.Last_Index);
      Last_Index        : constant Line_Index'Base :=
        Line_Index'Max (T1.Last_Index, T2.Last_Index);

      Result : Text := Empty_Text;

      --  A hunk is a group of consecutive differing lines,
      --  preceded by a git diff style header
      In_Hunk    : Boolean    := False;
      Hunk_Start : Line_Index := T1.First_Index;
      Exp_Count  : Natural    := 0;
      Act_Count  : Natural    := 0;
      Hunk_Lines : Text       := Empty_Text;

      --  Number of common lines displayed before and after each hunk,
      --  as sdiff does with the -l option: in the left column only
      Context_Lines : constant Natural := 3;

      --  Last common lines seen, kept as potential leading context
      Leading_Buffer : Text := Empty_Text;
      --  Common lines seen while In_Hunk, potential inner or
      --  trailing context
      Pending_Ctx : Text := Empty_Text;

      function Img (N : Natural) return String is
        (Ada.Strings.Fixed.Trim (N'Image, Ada.Strings.Both));

      --  In the git diff style, the count is omitted when it is 1
      function Count_Image (N : Natural) return String is
        (if N = 1 then "" else "," & Img (N));

      procedure Roll_Leading_Buffer (L : String) is
      --  Keep only the last Context_Lines common lines
      begin
         Leading_Buffer.Append (L);
         if Natural (Leading_Buffer.Length) > Context_Lines then
            Leading_Buffer.Delete_First;
         end if;
      end Roll_Leading_Buffer;

      procedure Flush_Hunk is
      --  Emits the current hunk: leading context, hunk header,
      --  diff lines with inner context, then trailing context
      --  taken from the Pending_Ctx lines closest to the hunk
      begin
         if In_Hunk then
            --  Hunk header, placed before the leading context,
            --  but pointing at the first differing line
            Result.Append (String'("@@ -" & Img (Natural (Hunk_Start)) &
                                     Count_Image (Exp_Count) &
                                     " +" & Img (Natural (Hunk_Start)) &
                                     Count_Image (Act_Count) & " @@"));
            --  Leading context, diff lines and inner context
            for L of Hunk_Lines loop
               Result.Append (L);
            end loop;
            --  Trailing context
            declare
               Nb : constant Natural := Natural'Min
                 (Context_Lines, Natural (Pending_Ctx.Length));
            begin
               if Nb > 0 then
                  for K in Pending_Ctx.First_Index ..
                    Pending_Ctx.First_Index + Line_Index (Nb) - 1
                  loop
                     Result.Append (Pending_Ctx (K));
                  end loop;
               end if;
            end;
            In_Hunk    := False;
            Hunk_Lines := Empty_Text;
         end if;
      end Flush_Hunk;

      procedure Start_Hunk (I : Line_Index) is
      begin
         if not In_Hunk then
            In_Hunk    := True;
            Hunk_Start := I;
            Exp_Count  := 0;
            Act_Count  := 0;
            Hunk_Lines := Leading_Buffer;
            Leading_Buffer := Empty_Text;
         else
            --  Continuing hunk: the pending common lines
            --  are inner context
            for L of Pending_Ctx loop
               Hunk_Lines.Append (L);
            end loop;
         end if;
         Pending_Ctx := Empty_Text;
      end Start_Hunk;

   begin
      for I in T1.First_Index .. Last_Index loop
         if I <= Last_Common_Index then
            -- Lines are present in both texts, so we compare them
            declare
               Left  : constant String := T1 (I);
               Right : constant String := T2 (I);
               Sep   : constant String := (if Is_Equal (Left, Right,
                                                       Case_Insensitive => Case_Insensitive,
                                                       Ignore_Blanks    => Ignore_Whitespaces)
                                           then Sep_Same
                                           elsif Right = "" then Sep_Only_Left
                                           else Sep_Diff);
            begin
               if Sep = Sep_Same then
                  if In_Hunk then
                     --  Potential inner or trailing context
                     Pending_Ctx.Append (Left);
                     if Natural (Pending_Ctx.Length) > 2 * Context_Lines then
                        --  The gap is too large: the hunk ends here.
                        --  Up to Context_Lines of Pending_Ctx are emitted
                        --  as trailing context by Flush_Hunk, the remaining
                        --  lines may become leading context of the next hunk.
                        Flush_Hunk;
                        for L of Pending_Ctx loop
                           Roll_Leading_Buffer (L);
                        end loop;
                        Pending_Ctx := Empty_Text;
                     end if;
                  else
                     --  Potential leading context of the next hunk
                     Roll_Leading_Buffer (Left);
                  end if;
               else
                  Start_Hunk (I);
                  Exp_Count := @ + 1;
                  Act_Count := @ + 1;
                  Hunk_Lines.Append (String'(Pad_To (Left, Sep_Col) & Sep & Right));
               end if;
            end;

         elsif I <= T1.Last_Index then
            -- Only the left text has this line
            Start_Hunk (I);
            Exp_Count := @ + 1;
            Hunk_Lines.Append (String'(Pad_To (T1 (I), Sep_Col) & Sep_Only_Left));

         else
            -- Only the right text has this line
            Start_Hunk (I);
            Act_Count := @ + 1;
            Hunk_Lines.Append (String'(Pad_To (" ", Sep_Col) & Sep_Only_Right & T2 (I)));
         end if;
      end loop;
      Flush_Hunk;

      return Result;

   end Side_By_Side;

   -- --------------------------------------------------------------------------
   procedure Compare (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Blanks      : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False;
                      Identical          : out Boolean) is
      T1 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text1)
                    else Text1);
      T2 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text2)
                    else Text2);
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

   end Compare;

   -- --------------------------------------------------------------------------
   procedure Get_Text_Diff (Text1, Text2       : Text;
                            Case_Insensitive   : Boolean := True;
                            Ignore_Blanks      : Boolean := True;
                            Ignore_Blank_Lines : Boolean := True;
                            Sort_Texts         : Boolean := False;
                            T1_Diff            : out Text;
                            T2_Diff            : out Text;
                            Diff_Start_In_T1   : out Line_Index) is
      T1 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text1)
                    else Text1);
      T2 : Text := (if Ignore_Blank_Lines then Remove_Blank_Lines (Text2)
                    else Text2);
      use type Ada.Containers.Count_Type;
      Same_Size : Boolean;
      Identical : Boolean;

   begin
      -- Initialize OUT parameters
      T1_Diff := Empty_Text;
      T2_Diff := Empty_Text;
      Diff_Start_In_T1 := 1;

      if Sort_Texts then
         Sort (T1);
         Sort (T2);
      end if;

      Same_Size := T1.Length = T2.Length;
      if not Same_Size then
         Identical := False;
         -- Set diff info for size mismatch
         Diff_Start_In_T1 := 1; ----*****************
         if T1.Length > 0 then
            T1_Diff := [T1 (T1.First_Index)];
         end if;
         if T2.Length > 0 then
            T2_Diff := [T2 (T2.First_Index)];
         end if;
         return;

      elsif T1.Length = 0 then
         Identical := True;

      else
         -- Brut compare
         for Diff_Index in T1.First_Index .. T1.Last_Index loop
            Identical := Is_Equal (T1 (Diff_Index),
                                   T2 (Diff_Index),
                                   Case_Insensitive => Case_Insensitive,
                                   Ignore_Blanks    => Ignore_Blanks);
            if not Identical then
               Diff_Start_In_T1 := Diff_Index;
               -- Extract diff context (up to 5 lines)
               declare
                  Lines_To_Show : constant Line_Count :=
                    Line_Count'Min (T1.Length - Line_Count (Diff_Index) + 1, Line_Count (5));
               begin
                  for I in 1 .. Line_Index (Lines_To_Show) loop
                     if Diff_Index + I - 1 <= T1.Last_Index then
                        T1_Diff.Append (T1 (Diff_Index + I - 1));
                     end if;
                     if Diff_Index + I - 1 <= T2.Last_Index then
                        T2_Diff.Append (T2 (Diff_Index + I - 1));
                     end if;
                  end loop;
               end;
               exit;
            end if;
         end loop;
      end if;

   end Get_Text_Diff;

   -- --------------------------------------------------------------------------
   function Is_Equal (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Blanks      : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False) return Boolean
   is
      Identical : Boolean;
   begin
      Compare (Text1, Text2,
               Ignore_Blank_Lines => Ignore_Blank_Lines,
               Ignore_Blanks      => Ignore_Blanks,
               Case_Insensitive   => Case_Insensitive,
               Sort_Texts         => Sort_Texts,
               Identical          => Identical);
      return Identical;
   end Is_Equal;

   -- --------------------------------------------------------------------------
   function Search (Source,
                    Pattern            : String;
                    Case_Insensitive   : Boolean := True;
                    Ignore_Whitespaces : Boolean := True) return Boolean is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Src : constant String := (if Ignore_Whitespaces then Join_Spaces (Source)
                                else Source);
      Pat : constant String := (if Ignore_Whitespaces then Join_Spaces (Pattern)
                                else Pattern);
   begin
       --  Ada.Text_IO.Put_Line ("Source  = """ & Source  & """");
       --  Ada.Text_IO.Put_Line ("Pattern = """ & Pattern & """");
      if Src = "" or Pat = "" then
         return False;
      end if;
      return (Index (Source  => Src,
                     Pattern => Pat,
                     From    => Src'First) /= 0)
        or else (Case_Insensitive and
                   Index (Source  => Ada.Characters.Handling.To_Lower (Src),
                          Pattern => Ada.Characters.Handling.To_Lower (Pat),
                          From    => Src'First) /= 0);
   end Search;

   -- --------------------------------------------------------------------------
   function Index_Of (T1, T2              : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Whitespaces : Boolean := True) return Line_Index'Base is
   -- Returns the index in T1 of the first line containing the first line of
   -- T2, with the same search criteria as Contains.
   -- Useful to locate where an unexpected text was found.
   -- Note: when T2 is multi-line, the returned index is the first line
   -- where T2 first line is found, which may differ from the position
   -- where the whole of T2 matches.
   begin
      if Is_Empty (T1) or else Is_Empty (T2) then
         return 0;
      end if;
      for I in T1.First_Index .. T1.Last_Index loop
         if Search (Source             => T1 (I),
                    Pattern            => T2 (T2.First_Index),
                    Case_Insensitive   => Case_Insensitive,
                    Ignore_Whitespaces => Ignore_Whitespaces)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Of;

   -- --------------------------------------------------------------------------
   function Contains (Text1, Text2       : Text;
                      Case_Insensitive   : Boolean := True;
                      Ignore_Whitespaces : Boolean := True;
                      Ignore_Blank_Lines : Boolean := True;
                      Sort_Texts         : Boolean := False) return Boolean is
   -- After eliminating easy cases T1 = T2 and T2 is longer than T1, the
   -- comparison algorithm is :
   -- I1 and I2 (in the loop below) are the two cursor respectively
   -- in T1 and T2.
   -- For each line in T1 (until there is not enough lines left in T1 to
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

      elsif Is_Empty (T2) then
         return False;
         -- If T2 is empty, what does "contains" means?
         -- But returning True could mask an error
         -- (file not found resulting in T2 empty for example)
         -- Even if it's on the caller responsibility to ensure T2 is not empty,
         -- "return False" seems safer here.

      else
         declare
            Last_I1 : constant Line_Index
              := T1.Last_Index - Line_Index (T2.Length) + 1;
            I1      : Line_Index;

         begin
            if Sort_Texts then
               Sort (T1);
               Sort (T2);
            end if;

            for Start in T1.First_Index .. Last_I1 loop
               I1 := Start;
               Inner : for I2 in T2.First_Index .. T2.Last_Index loop
                  -- We look for a first match between texts.
                  if Search (Source             => T1 (I1),
                             Pattern            => T2 (I2),
                             Case_Insensitive   => Case_Insensitive,
                             Ignore_Whitespaces => Ignore_Whitespaces)
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
   function Contains_Line (The_Text           : Text;
                           The_Line           : String;
                           Case_Insensitive   : Boolean := True;
                           Ignore_Whitespaces : Boolean := True) return Boolean is
   begin
      for L of The_Text loop
         if Is_Equal (L, The_Line,
                      Case_Insensitive => Case_Insensitive,
                      Ignore_Blanks    => Ignore_Whitespaces)
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Line;

   -- --------------------------------------------------------------------------
   function Contains_String
     (The_Text           : Text;
      The_String         : String;
      Case_Insensitive   : Boolean := True;
      Ignore_Whitespaces : Boolean := True) return Boolean is
   begin
      for L of The_Text loop
         if Search (L, The_String,
                    Case_Insensitive      => Case_Insensitive,
                    Ignore_Whitespaces    => Ignore_Whitespaces)
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains_String;

   -- --------------------------------------------------------------------------
   function Contains_Line
     (File_Name          : String;
      The_Line           : String;
      Case_Insensitive   : Boolean := True;
      Ignore_Whitespaces : Boolean := True) return Boolean is
   begin
      return Contains_Line (Get_Text (File_Name),
                            The_Line,
                            Case_Insensitive      => Case_Insensitive,
                            Ignore_Whitespaces    => Ignore_Whitespaces);
   end Contains_Line;

   -- --------------------------------------------------------------------------
   function Contains_String
     (File_Name          : String;
      The_String         : String;
      Case_Insensitive   : Boolean := True;
      Ignore_Whitespaces : Boolean := True) return Boolean is
   begin
      return Contains_String (Get_Text (File_Name),
                              The_String,
                              Case_Insensitive      => Case_Insensitive,
                              Ignore_Whitespaces    => Ignore_Whitespaces);
   end Contains_String;

   -- --------------------------------------------------------------------------
   function Matches (In_Text    : Text;
                     Regexp     : String)
                     return Boolean
   is
      use GNAT.Regexp;
      Matcher : GNAT.Regexp.Regexp;

   begin
      Matcher := Compile (Regexp);
      for I in In_Text.First_Index .. In_Text.Last_Index loop
         if Match (In_Text (I), Matcher) then
            -- Put_Line ("Match " & In_Text (I));
            return True;
         end if;
      end loop;
      return False;
   end Matches;

   -- -----------------------------------------------------------------------
   -- Masking of the volatile parts, cf. docs/features/A300_Ignoring.md

   Ignore_Marker : constant String := "***";
   -- Fixed marker replacing every masked part of the expected and actual
   -- texts, so that the comparison only checks that something was printed
   -- where a volatile value stands, not its exact value

   function Is_Digit (C : Character) return Boolean is
     (C in '0' .. '9');

   function Match_Digits (S     : String;
                          From  : Positive;
                          Count : Positive) return Boolean is
     (From + Count - 1 <= S'Last
        and then (for all I in From .. From + Count - 1 => Is_Digit (S (I))));

   function Match_Date_At (S : String; From : Positive) return Natural is
   -- Returns the last index of a date YYYY-MM-DD starting at From,
   -- 0 if the characters at From do not form a date
   begin
      if From + 9 > S'Last
        or else not Match_Digits (S, From, 4)
        or else S (From + 4) /= '-'
        or else not Match_Digits (S, From + 5, 2)
        or else S (From + 7) /= '-'
        or else not Match_Digits (S, From + 8, 2)
      then
         return 0;
      end if;
      return From + 9;
   end Match_Date_At;

   function Match_Clock_At (S : String; From : Positive) return Natural is
   -- Returns the last index of a clock time HH:MM, HH:MM:SS or
   -- HH:MM:SS.S... starting at From, 0 if the characters at From do not
   -- form a clock time
      Last : Natural := 0;
   begin
      if From + 4 > S'Last
        or else not Match_Digits (S, From, 2)
        or else S (From + 2) /= ':'
        or else not Match_Digits (S, From + 3, 2)
      then
         return 0;
      end if;
      Last := From + 4;
      if Last + 2 <= S'Last
        and then S (Last + 1) = ':'
        and then Match_Digits (S, Last + 2, 2)
      then
         -- HH:MM:SS
         Last := Last + 3;
         if Last + 1 <= S'Last and then S (Last + 1) = '.' then
            -- optional fractional part, one digit or more
            declare
               Frac_Last : Natural := Last + 1;
            begin
               while Frac_Last + 1 <= S'Last
                 and then Is_Digit (S (Frac_Last + 1))
               loop
                  Frac_Last := @ + 1;
               end loop;
               if Frac_Last > Last + 1 then
                  Last := Frac_Last;
               end if;
            end;
         end if;
      end if;
      return Last;
   end Match_Clock_At;

   function Date_Start_OK (S : String; From : Positive) return Boolean is
   -- A date must not be embedded in a longer run of digits or dates
     (From = S'First or else not Is_Digit (S (From - 1)));

   function Date_End_OK (S : String; Last : Natural) return Boolean is
     (Last = S'Last
        or else (not Is_Digit (S (Last + 1)) and then S (Last + 1) /= '-'));

   function Clock_Start_OK (S : String; From : Positive) return Boolean is
   -- A clock time must not be the tail of a longer time or number
     (From = S'First
        or else (not Is_Digit (S (From - 1))
                   and then S (From - 1) /= ':' and then S (From - 1) /= '.'));

   function Clock_End_OK (S : String; Last : Natural) return Boolean is
     (Last = S'Last
        or else (not Is_Digit (S (Last + 1))
                   and then S (Last + 1) /= ':' and then S (Last + 1) /= '.'));

   function Match_Time_At (S : String; From : Positive) return Natural is
   -- Returns the last index of a date, a clock time or a date-time
   -- starting at From, 0 if the characters at From do not form one.
   -- The longest form wins: a date-time before a date, a clock time
   -- with seconds and fraction before a plain clock time.
      Date_Last : constant Natural := Match_Date_At (S, From);
   begin
      if Date_Last > 0 and then Date_Start_OK (S, From) then
         if Date_Last < S'Last
           and then (S (Date_Last + 1) = 'T' or else S (Date_Last + 1) = 't')
         then
            -- date-time: a date immediately followed by a clock time
            declare
               Clock_Last : constant Natural :=
                 Match_Clock_At (S, Date_Last + 2);
            begin
               if Clock_Last > 0 and then Clock_End_OK (S, Clock_Last) then
                  return Clock_Last;
               end if;
            end;
         end if;
         if Date_End_OK (S, Date_Last) then
            return Date_Last;
         end if;
      end if;

      declare
         Clock_Last : constant Natural := Match_Clock_At (S, From);
      begin
         if Clock_Last > 0
           and then Clock_Start_OK (S, From)
           and then Clock_End_OK (S, Clock_Last)
         then
            return Clock_Last;
         end if;
      end;

      return 0;
   end Match_Time_At;

   procedure Mask_Time (S     : in out Unbounded_String;
                        Count : in out Natural) is
   -- Replaces every clock, date or date-time value in S by Ignore_Marker
      Src : constant String := To_String (S);
      R   : Unbounded_String;
      I   : Positive := Src'First;
   begin
      while I <= Src'Last loop
         declare
            Last : constant Natural := Match_Time_At (Src, I);
         begin
            if Last > 0 then
               Append (R, Ignore_Marker);
               Count := @ + 1;
               I     := Last + 1;
            else
               Append (R, Src (I));
               I     := @ + 1;
            end if;
         end;
      end loop;
      S := R;
   end Mask_Time;

   procedure Mask_Dir (S     : in out Unbounded_String;
                       Count : in out Natural) is
   -- Replaces by Ignore_Marker the directory part of every path in S.
   -- A path is a maximal sequence of non blank characters containing
   -- at least one separator (/ or \); everything up to and including
   -- the last separator is masked, the final component stays checked.
   -- The drive letter is part of the masked prefix.
      Src : constant String := To_String (S);
      R   : Unbounded_String;
      I   : Positive := Src'First;
      HT  : Character renames Ada.Characters.Latin_1.HT;
   begin
      while I <= Src'Last loop
         if Src (I) = ' ' or else Src (I) = HT then
            Append (R, Src (I));
            I := @ + 1;

         else
            -- start of a run of non blank characters
            declare
               Run_Last : Natural := I;
               Last_Sep : Natural  := 0;
            begin
               while Run_Last < Src'Last
                 and then Src (Run_Last + 1) /= ' '
                 and then Src (Run_Last + 1) /= HT
               loop
                  Run_Last := @ + 1;
               end loop;
               for J in I .. Run_Last loop
                  if Src (J) = '/' or else Src (J) = '\' then
                     Last_Sep := J;
                  end if;
               end loop;
               if Last_Sep > 0 then
                  Append (R, Ignore_Marker);
                  if Run_Last >= Last_Sep + 1 then
                     Append (R, Src (Last_Sep + 1 .. Run_Last));
                  end if;
                  Count := @ + 1;
               else
                  Append (R, Src (I .. Run_Last));
               end if;
               I := Run_Last + 1;
            end;
         end if;
      end loop;
      S := R;
   end Mask_Dir;

   -- -----------------------------------------------------------------------
   function Image (P : Ignore_Pattern) return String is
     (case P is
         when No_Pattern => "",
         when Time       => "time",
         when Dir        => "dir");

   function Image (Patterns : Ignore_Pattern_Set) return String is
      R : Unbounded_String;
   begin
      for P in Ignore_Pattern range Time .. Ignore_Pattern'Last loop
         if Patterns (P) then
            if R /= Null_Unbounded_String then
               Append (R, " and ");
            end if;
            Append (R, Image (P));
         end if;
      end loop;
      return To_String (R);
   end Image;

   function Mask (T        : Text;
                  Patterns : Ignore_Pattern_Set;
                  Counts   : out Pattern_Counts) return Text is
   begin
      Counts := [others => 0];
      if Patterns = No_Ignore_Pattern then
         return T;
      end if;

      declare
         R : Text := Empty_Text;
      begin
         if T.Is_Empty then
            return R;
         end if;
         for I in T.First_Index .. T.Last_Index loop
            declare
               L : Unbounded_String := To_Unbounded_String (T (I));
            begin
               if Patterns (Time) then
                  Mask_Time (L, Counts (Time));
               end if;
               if Patterns (Dir) then
                  Mask_Dir (L, Counts (Dir));
               end if;
               R.Append (To_String (L));
            end;
         end loop;
         return R;
      end;
   end Mask;

   function Mask (T        : Text;
                  Patterns : Ignore_Pattern_Set) return Text is
      Counts : Pattern_Counts;
   begin
      return Mask (T, Patterns, Counts);
   end Mask;

   -- --------------------------------------------------------------------------
   function First_Non_Blank_Line (In_Text : Text;
                                  From    : Line_Index := 1) return Natural is
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
               return Natural (I);
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
         --  a line holding only spaces and CRs is blank
         if Ada.Strings.Fixed.Index_Non_Blank (Join_Spaces (L)) /= 0 then
            T.Append (L);
         end if;
      end loop;
      return T;
   end Remove_Blank_Lines;

   -- --------------------------------------------------------------------------
   function Join_Spaces (From : String) return String is
      Tmp : String (From'Range);
      I   : Natural := Tmp'First;
   begin
      for J in From'Range loop
         --  CRs are ignored like spaces: they are an insignificant
         --  presentation detail at line ends
         if From (J) /= ' ' and then From (J) /= Ada.Characters.Latin_1.CR then
            Tmp (I) := From (J);
            I := @ + 1;
         end if;
      end loop;
      return Tmp (From'First .. I - 1);
   end Join_Spaces;

end Text_Utilities;
