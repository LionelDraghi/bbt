-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Equal_Case_Insensitive;

with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Regpat;

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
   --  Alignment of two texts prior to their side-by-side comparison:
   --  each operation pairs a left and a right line (Match), or carries
   --  a line present in only one of the texts (Delete, Insert)
   type Diff_Op_Kind is (Match, Delete, Insert);
   type Diff_Op is record
      Kind : Diff_Op_Kind;
      I    : Line_Index'Base := 0; --  left line index, 0 when none
      J    : Line_Index'Base := 0; --  right line index, 0 when none
   end record;

   package Diff_Op_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Diff_Op);
   subtype Diff_Op_List is Diff_Op_Lists.Vector;

   Max_Alignment_Cost : constant := 1_000;
   --  Number of insertions, deletions and changed lines above which
   --  the alignment gives up and the texts are paired positionally,
   --  to keep a bounded execution time on pathological inputs

   function Align (T1, T2            : Text;
                   Case_Insensitive   : Boolean;
                   Ignore_Whitespaces : Boolean) return Diff_Op_List;
   --  Returns the sequence of Match, Delete and Insert operations
   --  that aligns T1 and T2 on their longest common subsequence of
   --  lines, computed with the Myers algorithm, the basis of diff and
   --  git diff: an insertion or deletion does not shift the following
   --  pairings. Lines are compared with the same criteria as the
   --  display (Case_Insensitive, Ignore_Whitespaces).

   function Align (T1, T2            : Text;
                   Case_Insensitive   : Boolean;
                   Ignore_Whitespaces : Boolean) return Diff_Op_List is
      N : constant Natural := Natural (T1.Length);
      M : constant Natural := Natural (T2.Length);

      --  V (K) holds the furthest abscissa reached on the diagonal
      --  X - Y = K along the rounds
      type Diagonal is new Integer range -Max_Alignment_Cost
        .. Max_Alignment_Cost;
      V : array (Diagonal) of Natural := [others => 0];

      --  Snapshot of the active diagonals of V at the end of each
      --  round, flattened in Trace at the offsets Round_Start:
      --  backtracking the path requires the previous rounds values
      package Natural_Lists is new Ada.Containers.Indefinite_Vectors
        (Positive, Natural);
      Trace       : Natural_Lists.Vector;
      Round_Start : array (0 .. Max_Alignment_Cost) of Natural :=
        [others => 0];

      Ops     : Diff_Op_List;
      Found   : Boolean  := N = 0 and M = 0;
      Found_D : Natural := 0;

      procedure Snapshot (Round : Natural) is
      --  Save V (-Round .. Round) in Trace
      begin
         Round_Start (Round) := Natural (Trace.Length);
         for K in -Round .. Round loop
            Trace.Append (V (Diagonal (K)));
         end loop;
      end Snapshot;

   begin
      if Found then
         --  Nothing to align
         return Ops;
      end if;

      Round_Loop :
      for D in 0 .. Max_Alignment_Cost loop
         for K in -D .. D loop
            --  K and D have the same parity, as in the Myers algorithm
            if K mod 2 = D mod 2 then
               declare
                  X, Y : Natural;
               begin
                  if K = -D
                    or else (K /= D
                             and then V (Diagonal (K - 1)) < V (Diagonal (K + 1)))
                  then
                     X := V (Diagonal (K + 1));
                  else
                     X := V (Diagonal (K - 1)) + 1;
                  end if;
                  Y := X - K;
                  while X < N and then Y < M
                    and then Is_Equal (T1 (Line_Index (X + 1)),
                                       T2 (Line_Index (Y + 1)),
                                       Case_Insensitive => Case_Insensitive,
                                       Ignore_Blanks    => Ignore_Whitespaces)
                  loop
                     X := X + 1;
                     Y := Y + 1;
                  end loop;
                  V (Diagonal (K)) := X;
                  if X >= N and then Y >= M then
                     Found   := True;
                     Found_D := D;
                  end if;
               end;
            end if;
            exit Round_Loop when Found;
         end loop;
         Snapshot (D);
      end loop Round_Loop;

      if not Found then
         --  The alignment is too costly: pair the lines positionally,
         --  as the display did before the alignment was introduced
         declare
            Common : constant Natural := Natural'Min (N, M);
         begin
            for I in 1 .. Common loop
               if Is_Equal (T1 (Line_Index (I)), T2 (Line_Index (I)),
                            Case_Insensitive => Case_Insensitive,
                            Ignore_Blanks    => Ignore_Whitespaces)
               then
                  Ops.Append (Diff_Op'(Match, Line_Index (I), Line_Index (I)));
               else
                  Ops.Append (Diff_Op'(Delete, Line_Index (I), 0));
                  Ops.Append (Diff_Op'(Insert, 0, Line_Index (I)));
               end if;
            end loop;
            for I in Common + 1 .. N loop
               Ops.Append (Diff_Op'(Delete, Line_Index (I), 0));
            end loop;
            for J in Common + 1 .. M loop
               Ops.Append (Diff_Op'(Insert, 0, Line_Index (J)));
            end loop;
            return Ops;
         end;
      end if;

      --  Backtracking from the last line down to the first: the
      --  operations are collected in reverse order
      declare
         X : Natural := N;
         Y : Natural := M;

         function Round_V (Round, K : Integer) return Natural is
         --  The value of V (K) at the end of Round, from the snapshot
           (Trace.Element (Round_Start (Round) + Natural (K + Round) + 1));

      begin
         for D in reverse 0 .. Found_D loop
            if D = 0 then
               --  The initial snake, from (0, 0)
               while X > 0 and then Y > 0 loop
                  Ops.Append (Diff_Op'(Match, Line_Index (X), Line_Index (Y)));
                  X := X - 1;
                  Y := Y - 1;
               end loop;

            else
               declare
                  K      : constant Integer := X - Y;
                  Prev_K : Integer;
               begin
                  if K = -D
                    or else (K /= D
                             and then Round_V (D - 1, K - 1) < Round_V (D - 1, K + 1))
                  then
                     Prev_K := K + 1;
                  else
                     Prev_K := K - 1;
                  end if;
                  declare
                     Prev_X : constant Natural := Round_V (D - 1, Prev_K);
                     Prev_Y : constant Natural := Prev_X - Prev_K;
                  begin
                     if Prev_K = K + 1 then
                        --  Down move: a line of T2 was inserted, then
                        --  a snake up to (X, Y)
                        while X > Prev_X and then Y > Prev_Y + 1 loop
                           Ops.Append (Diff_Op'(Match, Line_Index (X),
                                        Line_Index (Y)));
                           X := X - 1;
                           Y := Y - 1;
                        end loop;
                        Ops.Append (Diff_Op'(Insert, 0, Line_Index (Y)));
                        Y := Y - 1;
                     else
                        --  Right move: a line of T1 was deleted, then
                        --  a snake up to (X, Y)
                        while Y > Prev_Y and then X > Prev_X + 1 loop
                           Ops.Append (Diff_Op'(Match, Line_Index (X),
                                        Line_Index (Y)));
                           X := X - 1;
                           Y := Y - 1;
                        end loop;
                        Ops.Append (Diff_Op'(Delete, Line_Index (X), 0));
                        X := X - 1;
                     end if;
                  end;
               end;
            end if;
         end loop;
      end;

      --  The operations were collected backward: reverse them
      declare
         Forward : Diff_Op_List;
      begin
         if not Ops.Is_Empty then
            for Idx in reverse Ops.First_Index .. Ops.Last_Index loop
               Forward.Append (Ops.Element (Idx));
            end loop;
         end if;
         return Forward;
      end;
   end Align;

   -- --------------------------------------------------------------------------
   function Side_By_Side (T1, T2            : Text;
                          Case_Insensitive   : Boolean := True;
                          Ignore_Whitespaces : Boolean := True) return Text is
      Sep_Diff       : constant String := " | ";
      Sep_Only_Left  : constant String := " < ";
      Sep_Only_Right : constant String := " > ";

      T1_Max_Len : constant Line_Length := Max_Line_Length (T1);
      Sep_Col    : constant Line_Length := T1_Max_Len + 1;

      Result : Text := Empty_Text;

      --  A hunk is a group of consecutive differing lines,
      --  preceded by a git diff style header
      In_Hunk       : Boolean         := False;
      Hunk_Start_T1 : Line_Index'Base := 0;
      Hunk_Start_T2 : Line_Index'Base := 0;
      Exp_Count     : Natural         := 0;
      Act_Count     : Natural         := 0;
      Hunk_Lines    : Text            := Empty_Text;

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
            Result.Append (String'("@@ -" &
                                     Img (Natural (Hunk_Start_T1)) &
                                     Count_Image (Exp_Count) &
                                     " +" &
                                     Img (Natural (Hunk_Start_T2)) &
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

      procedure Start_Hunk (T1_Start, T2_Start : Line_Index'Base) is
      begin
         if not In_Hunk then
            In_Hunk       := True;
            Hunk_Start_T1 := T1_Start;
            Hunk_Start_T2 := T2_Start;
            Exp_Count     := 0;
            Act_Count     := 0;
            Hunk_Lines    := Leading_Buffer;
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
      declare
         Ops : constant Diff_Op_List :=
           Align (T1, T2, Case_Insensitive, Ignore_Whitespaces);
         --  Index of the left and right lines of the last Match
         --  operation seen: the insertion point of the lines that
         --  are in only one of the texts
         Last_Match_T1 : Line_Index'Base := 0;
         Last_Match_T2 : Line_Index'Base := 0;
         Idx           : Positive := 1;

      begin
         while Idx <= Natural (Ops.Length) loop
            declare
               Op : constant Diff_Op := Ops (Idx);
            begin
               if Op.Kind = Match then
                  Last_Match_T1 := Op.I;
                  Last_Match_T2 := Op.J;
                  if In_Hunk then
                     --  Potential inner or trailing context
                     Pending_Ctx.Append (T1 (Op.I));
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
                     Roll_Leading_Buffer (T1 (Op.I));
                  end if;
                  Idx := Idx + 1;

               else
                  --  A run of Delete and Insert operations: the lines
                  --  present in only one of the texts, that occur at
                  --  the same place
                  declare
                     Left_Lines  : Text := Empty_Text;
                     Right_Lines : Text := Empty_Text;
                     First_Left  : Line_Index'Base := 0;
                     First_Right : Line_Index'Base := 0;

                  begin
                     while Idx <= Natural (Ops.Length)
                       and then Ops (Idx).Kind /= Match
                     loop
                        declare
                           Group_Op : constant Diff_Op := Ops (Idx);
                        begin
                           if Group_Op.Kind = Delete then
                              if First_Left = 0 then
                                 First_Left := Group_Op.I;
                              end if;
                              Left_Lines.Append (T1 (Group_Op.I));
                           else
                              if First_Right = 0 then
                                 First_Right := Group_Op.J;
                              end if;
                              Right_Lines.Append (T2 (Group_Op.J));
                           end if;
                        end;
                        Idx := Idx + 1;
                     end loop;

                     --  As sdiff does, the lines only in one text that
                     --  occur at the same place are paired
                     Start_Hunk ((if First_Left = 0 then Last_Match_T1
                                  else First_Left),
                                 (if First_Right = 0 then Last_Match_T2
                                  else First_Right));
                     declare
                        Pairs : constant Natural := Natural'Min
                          (Natural (Left_Lines.Length),
                           Natural (Right_Lines.Length));
                     begin
                        for K in 1 .. Pairs loop
                           Exp_Count := @ + 1;
                           Act_Count := @ + 1;
                           Hunk_Lines.Append (String'
                             (Pad_To (Left_Lines (Line_Index (K)), Sep_Col) &
                                Sep_Diff & Right_Lines (Line_Index (K))));
                        end loop;
                        for K in Pairs + 1 .. Natural (Left_Lines.Length) loop
                           Exp_Count := @ + 1;
                           Hunk_Lines.Append (String'
                             (Pad_To (Left_Lines (Line_Index (K)), Sep_Col) &
                                Sep_Only_Left));
                        end loop;
                        for K in Pairs + 1 .. Natural (Right_Lines.Length) loop
                           Act_Count := @ + 1;
                           Hunk_Lines.Append (String'
                             (Pad_To (" ", Sep_Col) & Sep_Only_Right &
                                Right_Lines (Line_Index (K))));
                        end loop;
                     end;
                  end;
               end if;
            end;
         end loop;
      end;
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

   function Version_Start_OK (S : String; From : Positive) return Boolean is
   -- A version must not be the tail of a longer number or version
     (From = S'First
        or else (not Is_Digit (S (From - 1)) and then S (From - 1) /= '.'));

   function Version_End_OK (S : String; Last : Natural) return Boolean is
   -- A version must not be the head of a longer number or version:
   -- the next character is rejected if it is a digit, or a dot
   -- followed by a digit
     (Last = S'Last
        or else (not Is_Digit (S (Last + 1))
                   and then (S (Last + 1) /= '.'
                               or else Last + 2 > S'Last
                               or else not Is_Digit (S (Last + 2)))));

   function Match_Version_At (S : String; From : Positive) return Natural is
   -- Returns the last index of a version number starting at From,
   -- 0 if the characters at From do not form one. A version is:
   --   D+ '.' D+ ('.' D+)? optionally followed by a '-' or '+'
   --   pre-release or build suffix (a run of letters, digits, dots
   --   and hyphens), as in 1.0, 0.5.1, 1.2.3-dev, 2.0.0-rc.1+meta
   begin
      if not Version_Start_OK (S, From) then
         return 0;
      end if;

      declare
         I : Natural := From;
      begin
         -- major
         while I < S'Last and then Is_Digit (S (I + 1)) loop
            I := @ + 1;
         end loop;
         -- minor
         if I + 2 > S'Last or else S (I + 1) /= '.' then
            return 0;
         end if;
         I := I + 1; -- on the dot
         if not Is_Digit (S (I + 1)) then
            return 0;
         end if;
         while I < S'Last and then Is_Digit (S (I + 1)) loop
            I := @ + 1;
         end loop;
         -- optional patch
         if I + 2 <= S'Last and then S (I + 1) = '.'
           and then Is_Digit (S (I + 2))
         then
            I := I + 2;
            while I < S'Last and then Is_Digit (S (I + 1)) loop
               I := @ + 1;
            end loop;
         end if;
         -- optional pre-release or build suffix
         if I + 1 <= S'Last
           and then (S (I + 1) = '-' or else S (I + 1) = '+')
         then
            declare
               Suffix_Last : Natural := I + 1;
            begin
               while Suffix_Last < S'Last
                 and then (Is_Digit (S (Suffix_Last + 1))
                             or else S (Suffix_Last + 1) in 'a' .. 'z'
                             or else S (Suffix_Last + 1) in 'A' .. 'Z'
                             or else S (Suffix_Last + 1) = '.'
                             or else S (Suffix_Last + 1) = '-'
                             or else S (Suffix_Last + 1) = '+')
               loop
                  Suffix_Last := @ + 1;
               end loop;
               if Suffix_Last > I + 1 then
                  -- a trailing dot is punctuation, not a suffix
                  while Suffix_Last > I + 1
                    and then S (Suffix_Last) = '.'
                  loop
                     Suffix_Last := @ - 1;
                  end loop;
                  if Suffix_Last > I + 1 then
                     I := Suffix_Last;
                  end if;
               end if;
            end;
         end if;
         if Version_End_OK (S, I) then
            return I;
         else
            return 0;
         end if;
      end;
   end Match_Version_At;

   procedure Mask_Version (S     : in out Unbounded_String;
                           Count : in out Natural) is
   -- Replaces every version number in S by Ignore_Marker
      Src : constant String := To_String (S);
      R   : Unbounded_String;
      I   : Positive := Src'First;
   begin
      while I <= Src'Last loop
         declare
            Last : constant Natural := Match_Version_At (Src, I);
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
   end Mask_Version;

   -- -----------------------------------------------------------------------
   function Image (P : Ignore_Pattern) return String is
     (case P is
         when No_Pattern => "",
         when Time       => "time",
         when Dir        => "dir",
         when Version    => "version");

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
               if Patterns (Version) then
                  Mask_Version (L, Counts (Version));
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

   -- -----------------------------------------------------------------------
   function Mask (T      : Text;
                 Regexp : String;
                 Count  : out Natural) return Text is
      use GNAT.Regpat;
      Matcher : constant Pattern_Matcher := Compile (Regexp);
      R       : Text := Empty_Text;
   begin
      Count := 0;
      if T.Is_Empty then
         return R;
      end if;

      for I in T.First_Index .. T.Last_Index loop
         declare
            S       : constant String := T (I);
            Line    : Unbounded_String;
            Pos     : Natural := S'First;
            Matches : Match_Array (0 .. 0);
         begin
            while Pos <= S'Last loop
               Match (Matcher, S (Pos .. S'Last), Matches);
               exit when Matches (0) = No_Match;
               if Matches (0).First > Matches (0).Last then
                  -- a zero-width match masks nothing: skip one character
                  Pos := @ + 1;
               else
                  Append (Line, S (Pos .. Matches (0).First - 1));
                  Append (Line, Ignore_Marker);
                  Count := @ + 1;
                  Pos   := Matches (0).Last + 1;
               end if;
            end loop;
            Append (Line, S (Pos .. S'Last));
            R.Append (To_String (Line));
         end;
      end loop;
      return R;
   end Mask;

   -- -----------------------------------------------------------------------
   function Mask (T      : Text;
                  Regexp : String) return Text is
      Count : Natural;
   begin
      return Mask (T, Regexp, Count);
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
