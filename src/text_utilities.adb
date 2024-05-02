package body Text_Utilities is

   -- -----------------------------------------------------------------------
   function Create_File (File_Name    : String;
                         With_Content : Text) return Boolean is
      -- return true if the file is created as expected,
      -- false otherwise
      Output : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Create (Output,
                          Ada.Text_IO.Out_File,
                          File_Name);
      for L of With_Content loop
         Ada.Text_IO.Put_Line (Output, Item => L);
      end loop;
      Ada.Text_IO.Close (Output);
      return True;

      --  exception
      --     when E : others =>
      --        IO.Put_Line (Ada.Exceptions.Exception_Message (E),
      --                     Level => IO.Quiet);
      --        IO.Put_Line ("Unable to create file "
      --                     & File_Name'Image,
      --                     Level => IO.Quiet);
      --        return False;
      --
   end Create_File;

   -- -----------------------------------------------------------------------
   function Create_File (File_Name    : Unbounded_String;
                         With_Content : Text) return Boolean is
     (Create_File (To_String (File_Name), With_Content));

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
   procedure Put_Text_Head
     (File  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Item  : Text;
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
   procedure Put_Text_Tail
     (File  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Item  : Text;
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
   procedure Compare (Text1, Text2 : Text;
                      Identical    : out Boolean;
                      Diff_Index   : out Natural) is
      use type Text;
   begin
      -- If Test1 = Text2, return Identical = True and Diff_Index = 0
      -- Otherwise, return False and Index of the first different line in Text2
      if Text1 = Text2 then
         Identical := True;
         Diff_Index := 0;
      else
         Identical := False;
         for Diff_Index in Text1.Iterate loop
            exit when Text1 (Diff_Index) /= Text2 (Diff_Index);
         end loop;
      end if;
   end Compare;

   -- --------------------------------------------------------------------------
   function Contains (Text1, Text2 : Text) return Boolean is
      use type Text;
      use type Texts.Cursor;
      use type Ada.Containers.Count_Type;

      --  function Is_Equal (T1, T2 : Text) return Boolean is
      --    (T1.Length = T2.Length and then (T1 = T2));

   begin
      if Text1.Length < Text2.Length then
         return False;
      elsif Text1.Length = Text2.Length then
         return Text1 = Text2;
      else
         declare
            Last_I1 : constant Positive := Text1.Last_Index -
                        Positive (Text2.Length) + 1;
            I1      : Positive;
         begin
            for Start in Text1.First_Index .. Last_I1 loop
               I1 := Start;
               Inner : for I2 in Text2.Iterate loop
                  if Text1 (I1) = Text2 (I2) then
                     if I2 = Text2.Last then
                        return True;
                     else
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

end Text_Utilities;
