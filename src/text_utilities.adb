-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;

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
   -- Fixme: blanks in the middle not taken into account
   begin
      if Ignore_Blanks then
         declare
            -- I1   : Natural := S1'First;
            -- I2   : Natural := S2'First;
            use Ada.Strings;
            use Ada.Strings.Fixed;
            Tmp1 : constant String := Trim (S1, Side => Both);
            Tmp2 : constant String := Trim (S2, Side => Both);
         begin
            return Is_Eq (Tmp1, Tmp2, Case_Insensitive);
         end;

      else
         return Is_Eq (S1, S2, Case_Insensitive);
         --  while I1 /= Tmp1'Last and I2 /= Tmp2'Last loop
         --
         --     while Index_Non_Blank (Tmp1, From => I1) > I1 loop
         --        I1 := @ + 1;
         --     end loop;
         --     while Index_Non_Blank (Tmp2, From => I2) > I2 loop
         --        I2 := @ + 1;
         --     end loop;
         --
         --     if not Is_Eq (Tmp1 (I1), Tmp2 (I2), Case_Insensitive) then
         --        return False;
         --     end if;
         --
         --  end loop;
         --  return True;

      end if;
   end Is_Equal;

   -- --------------------------------------------------------------------------
   procedure Create_File (File_Name   : String;
                         With_Content : Text) is
      Output : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (Output,
                          Ada.Text_IO.Out_File,
                          File_Name);
      for L of With_Content loop
         Ada.Text_IO.Put_Line (Output, Item => L);
      end loop;
      Ada.Text_IO.Close (Output);
   end Create_File;

   -- -----------------------------------------------------------------------
   function Create_File (File_Name    : Unbounded_String;
                         With_Content : Text) return Boolean is
   begin
      Create_File (To_String (File_Name), With_Content);
      return True;
   exception
      when others => return False;
   end Create_File;

   -- --------------------------------------------------------------------------
   procedure Put_Text
     (File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Item : Text) is
   begin
      for L of Item loop
         Put_Line (File, L);
      end loop;
   end Put_Text;

   -- --------------------------------------------------------------------------
   procedure Put_Text (Item      : Text;
                       File_Name : String) is
      File : File_Type;
      pragma Warnings (Off, File);
   begin
      Open (File, Name => File_Name, Mode => In_File);
      Put_Text (File, Item);
      Close (File);
   end Put_Text;

   -- --------------------------------------------------------------------------
   procedure Put_Text_Head
     (Item       : Text;
      File       : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Line_Count : Positive)
   is
      I : Positive := 1;
   begin
      for L of Item loop
         exit when I = Line_Count;
         Put_Line (File, L);
         I := @ + I;
      end loop;
   end Put_Text_Head;

   -- --------------------------------------------------------------------------
   procedure Put_Text_Head (Item       : Text;
                            File_Name  : String;
                            Line_Count : Positive) is
      File : File_Type;
      pragma Warnings (Off, File);
   begin
      Open (File, Name => File_Name, Mode => In_File);
      Put_Text_Head (Item, File, Line_Count);
      Close (File);
   end Put_Text_Head;

   -- --------------------------------------------------------------------------
   procedure Put_Text_Tail
     (Item       : Text;
      File       : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Line_Count : Positive)
   is
      I : Positive := 1;
   begin
      for L of reverse Item loop
         exit when I = Line_Count;
         Put_Line (File, L);
         I := @ + I;
      end loop;
   end Put_Text_Tail;

   -- --------------------------------------------------------------------------
   procedure Put_Text_Tail (Item       : Text;
                            File_Name  : String;
                            Line_Count : Positive) is
      File : File_Type;
      pragma Warnings (Off, File);
   begin
      Open (File, Name => File_Name, Mode => In_File);
      Put_Text_Tail (Item, File, Line_Count);
      Close (File);
   end Put_Text_Tail;

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
      T := Get_Text (File);
      while not End_Of_File (File) loop
         T.Append (Get_Line (File));
      end loop;
      Close (File);
      return T;
   end Get_Text;

   -- --------------------------------------------------------------------------
   function Get_Text (File_Name : Unbounded_String) return Text is
     (Get_Text (To_String (File_Name)));

   -- --------------------------------------------------------------------------
   function Get_Text_Head (From       : Text;
                           Line_Count : Positive) return Text is
      use Ada.Containers;
   begin
      if From = Empty_Text then
         return Empty_Text;
      elsif From.Length < Count_Type (Line_Count) then
         return From;
      else
         declare
            Tmp : Text;
         begin
            for I in From.First_Index .. From.First_Index + Line_Count - 1 loop
               Tmp.Append (From (From.First_Index + I - 1));
            end loop;
            return Tmp;
         end;
      end if;
   end Get_Text_Head;

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

   -- --------------------------------------------------------------------------
   procedure Compare (Text1, Text2     : Text;
                      Ignore_Blanks    : Boolean := True;
                      Case_Insensitive : Boolean := True;
                      Identical        : out Boolean;
                      Diff_Index       : out Natural) is
      Idx1, Idx2 : Natural := 1;

      -- Fixme: uggly code!!
   begin
      Identical := False;

      --  Put_Line ("Text1 = " & Text1'Image);
      --  Put_Line ("Text2 = " & Text2'Image);

      -- If Test1 = Text2, return Identical = True and Diff_Index = 0
      -- Otherwise, return False and Index of the first different line in Text2
      if Text1 = Text2 then
         Identical := True;
         Diff_Index := 0;
         -- Put_Line ("Text1 = Text2");

      elsif Ignore_Blanks then

         Idx1 := First_Non_Blank_Line (Text1, @);
         Idx2 := First_Non_Blank_Line (Text2, @);

         if Idx1 = 0 and Idx2 = 0 then
            -- Put_Line ("Idx1 = Idx2 = 0");
            -- both file are blank
            Identical  := True;
            Diff_Index := 0;
            return;
         end if;

         --  Put_Line ("Init Idx1 = " & Idx1'Image & "/" & Text1.Last_Index'Image);
         --  Put_Line ("Init Idx2 = " & Idx2'Image & "/" & Text2.Last_Index'Image);

         loop
            if Idx1 = 0 xor Idx2 = 0 then
               -- no more line to compare for one of the files
               if Idx2 /= 0 then
                  Diff_Index := Idx2;
                  -- otherwise, Diff_Index is already set to the last non blank
                  --  Put_Line ("Idx1 = 0");
                  --  else
                  --     Put_Line ("Idx2 = 0");
               end if;
               Identical := False;
               return;
            end if;

            if not Is_Equal (Text1 (Idx1), Text2 (Idx2),
                             Case_Insensitive => Case_Insensitive,
                             Ignore_Blanks    => Ignore_Blanks)
            then
               -- Put_Line ("Text1 /= Text2");
               Identical := False;
               Diff_Index := Idx2;
               return;
               --  else
               --     Put_Line (" Text1 (" & Idx1'Image
               --               & ") " & String'(Text1 (Idx1))'Image
               --               & " = Text2 (" & Idx2'Image & ") "
               --              & String'(Text2 (Idx2))'Image);
               -- Put_Line (String'(Text1 (Idx1))'Image & " = " & String'(Text2 (Idx2))'Image);

            end if;

            Diff_Index := Idx2; -- store the last non blank index

            -- Put_Line ("Idx1 = " & Idx1'Image & "/" & Text1.Last_Index'Image);
            -- Put_Line ("Idx2 = " & Idx2'Image & "/" & Text2.Last_Index'Image);
            exit when Idx1 = Text1.Last_Index and Idx2 = Text2.Last_Index;

            declare
               I1 : Natural := Text1.Last_Index;
               I2 : Natural := Text2.Last_Index;
            begin
               if Idx1 /= Text1.Last_Index then
                  I1 := First_Non_Blank_Line (Text1, From => Idx1 + 1);
               end if;
               if Idx2 /= Text2.Last_Index then
                  I2 := First_Non_Blank_Line (Text2, From => Idx2 + 1);
               end if;
               -- Idx should not return to zero, so let's use tmp index :
               -- Put_Line ("I1 = " & I1'Image);
               -- Put_Line ("I2 = " & I2'Image);
               if I1 /= 0 then
                  Idx1 := I1;
               end if;
               if I2 /= 0 then
                  Idx2 := I2;
               end if;
            end;

            --  Put_Line ("Idx1 = " & Idx1'Image & "/" & Text1.Last_Index'Image);
            --  Put_Line ("Idx2 = " & Idx2'Image & "/" & Text2.Last_Index'Image);

         end loop;

         -- Put_Line ("Text1 = Text2");
         Identical := True;
         Diff_Index := 0;

      else
         -- Brut compare
         for Diff_Index in Text1.Iterate loop
            exit when Texts."/=" (Text1 (Diff_Index), Text2 (Diff_Index));
         end loop;
      end if;

   end Compare;

   -- --------------------------------------------------------------------------
   function Is_Equal (Text1, Text2       : Text;
                      Ignore_Blank_Lines : Boolean := True;
                      Case_Insensitive   : Boolean := True) return Boolean
   is
      Identical  : Boolean;
      Diff_Index : Natural; pragma Unreferenced (Diff_Index);
   begin
      Compare (Text1, Text2,
               Ignore_Blanks      => Ignore_Blank_Lines,
               Case_Insensitive   => Case_Insensitive,
               Identical          => Identical,
               Diff_Index         => Diff_Index);
      return Identical;
   end Is_Equal;

   -- --------------------------------------------------------------------------
   function Search (String1,
                    String2          : String;
                    Case_Insensitive : Boolean := True) return Boolean is
   begin
      return (Ada.Strings.Fixed.Index (Source  => String1,
                                       Pattern => String2,
                                       From    => String1'First) /= 0)
        or else (Case_Insensitive and Ada.Strings.Fixed.Index
                 (Source  => Ada.Characters.Handling.To_Lower (String1),
                  Pattern => Ada.Characters.Handling.To_Lower (String2),
                  From    => String1'First) /= 0);
   end Search;

   -- --------------------------------------------------------------------------
   function Contains (Text1, Text2     : Text;
                      Case_Insensitive : Boolean := True) return Boolean is
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

   begin
      if Text1.Length < Text2.Length then
         return False;

      elsif Text1 = Text2 then
         return True;

      else
         declare
            Last_I1 : constant Positive
              := Text1.Last_Index - Positive (Text2.Length) + 1;
            I1      : Positive;

         begin
            for Start in Text1.First_Index .. Last_I1 loop
               I1 := Start;
               Inner : for I2 in Text2.First_Index .. Text2.Last_Index loop
                  -- We look for a first match between texts.
                  if Search (Text1 (I1), Text2 (I2), Case_Insensitive) then
                     -- Lines match
                     if I2 = Text2.Last_Index then
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
                           Case_Insensitive : Boolean := True) return Boolean is
   begin
      for L of The_Text loop
         if Is_Equal (L, The_Line, Case_Insensitive) then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Line;

   -- --------------------------------------------------------------------------
   function Contains_String
     (The_Text         : Text;
      The_String       : String;
      Case_Insensitive : Boolean := True) return Boolean is
   begin
      for L of The_Text loop
         if Search (L, The_String, Case_Insensitive) then
            return True;
         end if;
      end loop;
      return False;
   end Contains_String;

   -- --------------------------------------------------------------------------
   function Contains_Line
     (File_Name        : String;
      The_Line         : String;
      Case_Insensitive : Boolean := True) return Boolean is
   begin
      return Contains_Line (Get_Text (File_Name),
                            The_Line,
                            Case_Insensitive);
   end Contains_Line;

   -- --------------------------------------------------------------------------
   function Contains_String
     (File_Name          : String;
      The_String         : String;
      Case_Insensitive   : Boolean := True) return Boolean is
   begin
      return Contains_String (Get_Text (File_Name),
                              The_String,
                              Case_Insensitive);
   end Contains_String;

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

end Text_Utilities;
