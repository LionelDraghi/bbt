-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Strings.Fixed;

package body TodoMVC_Model is

   use Todo_Vectors;

   function Trimmed (Text : String) return String is
     (Ada.Strings.Fixed.Trim (Text, Ada.Strings.Both));

   function Image (Value : Natural) return String is
     (Ada.Strings.Fixed.Trim (Natural'Image (Value), Ada.Strings.Left));

   function Length (List : Todo_List) return Natural is
     (Natural (List.Todos.Length));

   function Todo_At (List : Todo_List; Index : Positive) return Todo is
   begin
      if Index > Length (List) then
         return (Title => Null_Unbounded_String, Done => False);
      end if;
      return List.Todos.Element (Index);
   end Todo_At;

   function Active_Count (List : Todo_List) return Natural is
      Count : Natural := 0;
   begin
      for I in 1 .. Length (List) loop
         if not List.Todos.Element (I).Done then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Count;

   function All_Done (List : Todo_List) return Boolean is
     (Length (List) > 0 and then Active_Count (List) = 0);

   function Counter_Text (List : Todo_List) return String is
      Active : constant Natural := Active_Count (List);
   begin
      if Active = 1 then
         return "1 item left";
      end if;
      return Image (Active) & " items left";
   end Counter_Text;

   function Visible (List : Todo_List; Index : Positive) return Boolean is
   begin
      if Index > Length (List) then
         return False;
      end if;
      case List.Active is
         when All_Filter =>
            return True;
         when Active_Filter =>
            return not List.Todos.Element (Index).Done;
         when Completed_Filter =>
            return List.Todos.Element (Index).Done;
      end case;
   end Visible;

   function Filter (List : Todo_List) return Filter_Kind is
     (List.Active);

   procedure Add (List : in out Todo_List; Title : String) is
      Text : constant String := Trimmed (Title);
   begin
      if Text'Length = 0 then
         return;
      end if;
      List.Todos.Append
        (Todo'(Title => To_Unbounded_String (Text), Done => False));
   end Add;

   procedure Delete (List : in out Todo_List; Index : Positive) is
   begin
      if Index <= Length (List) then
         List.Todos.Delete (Index);
      end if;
   end Delete;

   procedure Set_Done
     (List : in out Todo_List; Index : Positive; Done : Boolean) is
   begin
      if Index <= Length (List) then
         declare
            Item : Todo := List.Todos.Element (Index);
         begin
            Item.Done := Done;
            List.Todos.Replace_Element (Index, Item);
         end;
      end if;
   end Set_Done;

   procedure Toggle (List : in out Todo_List; Index : Positive) is
   begin
      if Index <= Length (List) then
         Set_Done (List, Index, not List.Todos.Element (Index).Done);
      end if;
   end Toggle;

   procedure Set_Title
     (List : in out Todo_List; Index : Positive; Title : String) is
      Text : constant String := Trimmed (Title);
   begin
      if Index > Length (List) then
         return;
      end if;
      if Text'Length = 0 then
         Delete (List, Index);
         return;
      end if;
      declare
         Item : Todo := List.Todos.Element (Index);
      begin
         Item.Title := To_Unbounded_String (Text);
         List.Todos.Replace_Element (Index, Item);
      end;
   end Set_Title;

   procedure Clear_Completed (List : in out Todo_List) is
   begin
      for I in reverse 1 .. Length (List) loop
         if List.Todos.Element (I).Done then
            Delete (List, I);
         end if;
      end loop;
   end Clear_Completed;

   procedure Mark_All (List : in out Todo_List) is
      Target : constant Boolean := not All_Done (List);
   begin
      for I in 1 .. Length (List) loop
         Set_Done (List, I, Target);
      end loop;
   end Mark_All;

   procedure Set_Filter (List : in out Todo_List; Kind : Filter_Kind) is
   begin
      List.Active := Kind;
   end Set_Filter;

   procedure Clear (List : in out Todo_List) is
   begin
      List.Todos.Clear;
      List.Active := All_Filter;
   end Clear;

end TodoMVC_Model;
