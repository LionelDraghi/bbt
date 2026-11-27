-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BBT_Backends;
with BBT_Backends.Polling;       use BBT_Backends.Polling;
with BBT.Backends.Adi2.Bridge;   use BBT.Backends.Adi2.Bridge;

package body BBT.Backends.Adi2 is

   Last_Snapshot : Unbounded_String := Null_Unbounded_String;

   function Snapshot return Node_Vectors.Vector;
   --  A fresh widget tree, remembered for the diagnostics

   function Snapshot return Node_Vectors.Vector is
      Response : constant String := Widget_Tree;
   begin
      Last_Snapshot := To_Unbounded_String (Response);
      return Parse (Response);
   end Snapshot;

   --  Role and name ------------------------------------------------------

   function Kind_Of
     (Nodes : Node_Vectors.Vector; Index : Natural) return String
   is
      --  The bridge serializes the Ada external tag of the widget
      --  class, which for a generic instantiation reads as "internal
      --  tag at 16#..#: unit.type" — only the last segment names the
      --  widget kind, and the unit part would otherwise steer the
      --  family matches (todomvc.label_list.list_box_widget carries
      --  "label", which is not what it is)
      Kind : constant String := To_String (Nodes.Element (Index).Kind);
      Dot  : constant Natural :=
        Ada.Strings.Fixed.Index (Kind, ".", Ada.Strings.Backward);
   begin
      return (if Dot = 0 then Kind else Kind (Dot + 1 .. Kind'Last));
   end Kind_Of;

   function Role_Matches (Kind, Role : String) return Boolean is
      use Ada.Characters.Handling;
      K : constant String := To_Lower (Kind);
      R : constant String := To_Lower (Role);
   begin
      if R = "listitem" then
         --  A list box's rows are plain label widgets: the structure
         --  carries the relation the bridge does not expose
         return Ada.Strings.Fixed.Index (K, "label") /= 0;
      elsif R = "button" then
         return Ada.Strings.Fixed.Index (K, "button") /= 0
           and then Ada.Strings.Fixed.Index (K, "switch") = 0;
      elsif R = "toggle button" then
         return Ada.Strings.Fixed.Index (K, "switch") /= 0;
      elsif R = "textbox" then
         return Ada.Strings.Fixed.Index (K, "text_input") /= 0;
      elsif R = "list" then
         return Ada.Strings.Fixed.Index (K, "list_box") /= 0;
      else
         return Ada.Strings.Fixed.Index (K, R) /= 0;
      end if;
   end Role_Matches;

   function Inside_A_List
     (Nodes : Node_Vectors.Vector; Index : Natural) return Boolean is
      Path : Unbounded_String := Nodes.Element (Index).Path;
   begin
      loop
         Path := To_Unbounded_String (Parent_Path (To_String (Path)));
         exit when Path = Null_Unbounded_String;
         declare
            Parent : constant Natural :=
              Find_By_Path (Nodes, To_String (Path));
         begin
            exit when Parent = 0;
            if Role_Matches (Kind_Of (Nodes, Parent), "list") then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Inside_A_List;

   function Following_Path (Path : String) return String;
   --  The path of the next tree sibling

   function Following_Path (Path : String) return String is
      Dot : constant Natural :=
        Ada.Strings.Fixed.Index (Path, ".", Ada.Strings.Backward);
      First_Of_Last : constant Positive :=
        (if Dot = 0 then Path'First else Dot + 1);
      Index : constant Positive :=
        Positive'Value (Path (First_Of_Last .. Path'Last)) + 1;
      Image : constant String :=
        Ada.Strings.Fixed.Trim (Positive'Image (Index), Ada.Strings.Both);
   begin
      return (if Dot = 0 then Image else Path (Path'First .. Dot) & Image);
   exception
      when Constraint_Error =>
         return "";
   end Following_Path;

   function Preceding_Label_Names
     (Nodes : Node_Vectors.Vector; Index : Natural; Name : String)
      return Boolean;
   --  Whether the label that immediately precedes Nodes (Index) in tree
   --  order carries Name

   function Preceding_Label_Names
     (Nodes : Node_Vectors.Vector; Index : Natural; Name : String)
      return Boolean is
   begin
      for J in Nodes.First_Index .. Nodes.Last_Index loop
         if Shown (Nodes, J)
           and then Role_Matches (Kind_Of (Nodes, J), "label")
           and then To_String (Nodes.Element (J).Text) = Name
           and then Following_Path (To_String (Nodes.Element (J).Path))
                    = To_String (Nodes.Element (Index).Path)
         then
            return True;
         end if;
      end loop;
      return False;
   end Preceding_Label_Names;

   function Named_Node
     (Nodes : Node_Vectors.Vector; Role, Name : String) return Natural is
   begin
      for I in Nodes.First_Index .. Nodes.Last_Index loop
         if Shown (Nodes, I) and then Role_Matches (Kind_Of (Nodes, I), Role)
         then
            if Role_Matches (Kind_Of (Nodes, I), "textbox")
              or else Role_Matches (Kind_Of (Nodes, I), "list")
            then
               --  A widget carrying no text of its own -- a field, a
               --  list -- is named by the label that precedes it: the
               --  label's next sibling is the widget itself
               if Preceding_Label_Names (Nodes, I, Name) then
                  return I;
               end if;
            elsif Role_Matches (Kind_Of (Nodes, I), "listitem") then
               if Inside_A_List (Nodes, I)
                 and then To_String (Nodes.Element (I).Text) = Name
               then
                  return I;
               end if;
            elsif To_String (Nodes.Element (I).Text) = Name then
               return I;
            end if;
         end if;
      end loop;
      return 0;
   end Named_Node;

   function Named_Node (Role, Name : String) return Natural is
     (Named_Node (Snapshot, Role, Name));

   --  Operations ---------------------------------------------------------

   overriding procedure Launch
     (The_Backend : in out Adi2_Backend; Binary_Path : String) is
   begin
      Bridge.Launch (Binary_Path);
   end Launch;

   overriding procedure Shutdown (The_Backend : in out Adi2_Backend) is
   begin
      Bridge.Shutdown;
   end Shutdown;

   overriding function Window_Appears
     (The_Backend : Adi2_Backend; Expected_Title : String := "")
      return Boolean
   is
      function Try return Boolean is
         Nodes : constant Node_Vectors.Vector := Snapshot;
         Root  : constant Natural := Find_By_Path (Nodes, "");
      begin
         return Root /= 0
           and then Shown (Nodes, Root)
           and then (Expected_Title = ""
                     or else To_String (Nodes.Element (Root).Text)
                             = Expected_Title);
      end Try;
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Window_Appears;

   overriding function Observed_Title
     (The_Backend : Adi2_Backend) return String
   is
      Nodes : constant Node_Vectors.Vector := Parse (To_String (Last_Snapshot));
      Root  : constant Natural := Find_By_Path (Nodes, "");
   begin
      if Root = 0 then
         return "";
      end if;
      return To_String (Nodes.Element (Root).Text);
   end Observed_Title;

   overriding function Window_Contains
     (The_Backend : Adi2_Backend; Expected : String) return Boolean
   is
      function Try return Boolean is
         Nodes : constant Node_Vectors.Vector := Snapshot;
      begin
         for I in Nodes.First_Index .. Nodes.Last_Index loop
            if Shown (Nodes, I)
              and then Contains (To_String (Nodes.Element (I).Text), Expected)
            then
               return True;
            end if;
         end loop;
         return False;
      end Try;
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Window_Contains;

   overriding function Widget_Named
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean
   is
      function Try return Boolean is
        (Named_Node (Role, Name) /= 0);
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Widget_Named;

   overriding function Widget_Enabled
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean
   is
      function Try return Boolean is
         Nodes  : constant Node_Vectors.Vector := Snapshot;
         Target : constant Natural := Named_Node (Nodes, Role, Name);
      begin
         --  Not found reads False, never silently enabled: the
         --  existence check is the preceding scenario step
         return Target /= 0 and then not Nodes.Element (Target).Disabled;
      end Try;
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Widget_Enabled;

   overriding function Press_Widget
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean
   is
      function Present return Boolean is
        (Named_Node (Role, Name) /= 0);
      --  The press is not replayed on retry: it is sent once the
      --  widget is there, and its own outcome is reported
      function Press_Once return Boolean is
         Nodes  : constant Node_Vectors.Vector := Snapshot;
         Target : constant Natural := Named_Node (Nodes, Role, Name);
      begin
         if Target = 0 then
            return False;
         end if;
         declare
            Id : constant String :=
              Ada.Strings.Fixed.Trim
                (Natural'Image (Nodes.Element (Target).Id), Ada.Strings.Both);
         begin
            return Ok (Run_Command
                         ("click_widget", '"' & "id" & '"' & ":" & Id));
         end;
      end Press_Once;
      function Poll is new Eventually_Get (Present  => Present,
                                          Observed => Press_Once);
   begin
      return Poll;
   end Press_Widget;

   overriding function Widget_Contains
     (The_Backend : Adi2_Backend; Role, Name, Expected : String)
      return Boolean
   is
      function Try return Boolean is
         Nodes    : constant Node_Vectors.Vector := Snapshot;
         Target   : constant Natural := Named_Node (Nodes, Role, Name);
         Sub_Path : Unbounded_String;
      begin
         if Target = 0 then
            return False;
         end if;
         Sub_Path := Nodes.Element (Target).Path;
         for I in Nodes.First_Index .. Nodes.Last_Index loop
            if Is_Descendant (To_String (Nodes.Element (I).Path),
                              To_String (Sub_Path))
              and then Shown (Nodes, I)
              and then Contains (To_String (Nodes.Element (I).Text), Expected)
            then
               return True;
            end if;
         end loop;
         return False;
      end Try;
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Widget_Contains;

   overriding function Widget_Is_Empty
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean
   is
      function Try return Boolean is
         Nodes  : constant Node_Vectors.Vector := Snapshot;
         Target : constant Natural := Named_Node (Nodes, Role, Name);
      begin
         return Target /= 0 and then Nodes.Element (Target).Child_Count = 0;
      end Try;
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Widget_Is_Empty;

   overriding function Widget_Item_Count
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Natural
   is
      Result : Natural := 0;
      function Try return Boolean is
         Nodes  : constant Node_Vectors.Vector := Snapshot;
         Target : constant Natural := Named_Node (Nodes, Role, Name);
      begin
         Result :=
           (if Target = 0 then 0 else Nodes.Element (Target).Child_Count);
         return Target /= 0;
      end Try;
      function Poll is new Eventually (Try);
      Found : constant Boolean := Poll;
      pragma Unreferenced (Found);
   begin
      return Result;
   end Widget_Item_Count;

   overriding procedure Type_Into
     (The_Backend : in out Adi2_Backend;
      Role        : String;
      Name        : String;
      Text        : String)
   is
      Nodes  : constant Node_Vectors.Vector := Snapshot;
      Target : constant Natural := Named_Node (Nodes, Role, Name);
   begin
      if Target = 0 then
         raise No_Backend_Available
           with "no " & Role & " named " & Name & " in the Adi2 widget tree";
      end if;
      declare
         Id : constant String :=
           Ada.Strings.Fixed.Trim
             (Natural'Image (Nodes.Element (Target).Id), Ada.Strings.Both);
         Answer : constant String :=
           Run_Command
             ("set_text",
              '"' & "id" & '"' & ":" & Id & ","
              & '"' & "text" & '"' & ":" & '"' & Text & '"');
      begin
         if not Ok (Answer) then
            raise No_Backend_Available
              with "the Adi2 bridge refused to set the value of " & Name;
         end if;
      end;
   end Type_Into;

   overriding procedure Enter_Into
     (The_Backend : in out Adi2_Backend;
      Role        : String;
      Name        : String;
      Text        : String)
   is
      Nodes  : constant Node_Vectors.Vector := Snapshot;
      Target : constant Natural := Named_Node (Nodes, Role, Name);
   begin
      if Target = 0 then
         raise No_Backend_Available
           with "no " & Role & " named " & Name & " in the Adi2 widget tree";
      end if;
      declare
         Id : constant String :=
           Ada.Strings.Fixed.Trim
             (Natural'Image (Nodes.Element (Target).Id), Ada.Strings.Both);
      begin
         Type_Into (The_Backend, Role, Name, Text);
         if not Ok (Run_Command
                      ("set_focus", '"' & "id" & '"' & ":" & Id))
           or else not Ok (Run_Command
                             ("send_keys",
                              '"' & "keys" & '"' & ":" & '"' & "{Return}" & '"'))
         then
            raise No_Backend_Available
              with "the Adi2 bridge could not commit the field " & Name;
         end if;
      end;
   end Enter_Into;

   overriding function Close_Window
     (The_Backend : Adi2_Backend; Title : String := "")
      return Boolean
   is
      pragma Unreferenced (Title);
   begin
      if Bridge.Quit (BBT_Backends.Harness_Deadline) then
         return True;
      end if;
      --  The application's own close path refused or never answered:
      --  the process-level close is the documented fallback
      Bridge.Shutdown;
      return True;
   end Close_Window;

end BBT.Backends.Adi2;
