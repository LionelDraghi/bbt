-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

--  The ToDoMVC model of the Adi2 witness: the todos in creation order,
--  their completed state, and the active filter (cf. ../ToDoMVC.md).
--  No widget and no file here: the widget wiring lives in todomvc.adb,
--  the persistence in todomvc_store.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package TodoMVC_Model is

   type Filter_Kind is (All_Filter, Active_Filter, Completed_Filter);

   type Todo is record
      Title : Unbounded_String := Null_Unbounded_String;
      Done  : Boolean := False;
   end record;

   package Todo_Vectors is new Ada.Containers.Vectors (Positive, Todo);

   type Todo_List is private;

   Empty_List : constant Todo_List;

   --  Queries ---------------------------------------------------------------

   function Length (List : Todo_List) return Natural;
   --  The number of todos, whatever the filter

   function Todo_At (List : Todo_List; Index : Positive) return Todo;
   --  The todo at Index in creation order

   function Active_Count (List : Todo_List) return Natural;
   --  The todos still to be done

   function All_Done (List : Todo_List) return Boolean;
   --  True when there is at least one todo and every one of them is done
   --  (the mark-all control's own state, cf. ToDoMVC.md)

   function Counter_Text (List : Todo_List) return String;
   --  The footer counter, pluralized: "0 items left", "1 item left"...

   function Visible (List : Todo_List; Index : Positive) return Boolean;
   --  Whether the todo at Index passes the active filter

   function Filter (List : Todo_List) return Filter_Kind;

   --  Commands --------------------------------------------------------------

   procedure Add (List : in out Todo_List; Title : String);
   --  Appends a todo, the title trimmed: an empty title adds nothing

   procedure Set_Done
     (List : in out Todo_List; Index : Positive; Done : Boolean);
   procedure Toggle (List : in out Todo_List; Index : Positive);
   procedure Set_Title (List : in out Todo_List; Index : Positive; Title : String);
   --  Replaces the title, trimmed: an empty title destroys the todo
   procedure Delete (List : in out Todo_List; Index : Positive);
   procedure Clear_Completed (List : in out Todo_List);
   procedure Mark_All (List : in out Todo_List);
   --  Completes every todo, or uncompletes them all when they all are
   procedure Set_Filter (List : in out Todo_List; Kind : Filter_Kind);
   procedure Clear (List : in out Todo_List);

private

   type Todo_List is record
      Todos  : Todo_Vectors.Vector;
      Active : Filter_Kind := All_Filter;
   end record;

   Empty_List : constant Todo_List :=
     (Todos => Todo_Vectors.Empty_Vector, Active => All_Filter);

end TodoMVC_Model;
