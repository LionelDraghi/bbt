-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-- -----------------------------------------------------------------------------
--  GTK 4 implementation of the ToDoMVC witness app (cf. the behavior
--  spec in Apps/ToDoMVC/ToDoMVC.md). Same structure and idioms as the
--  GuiVerb apps/witness reference: Gtk.Main.Init first, an explicit
--  window presentation, and the event loop pumped with
--  Main_Context_Iteration.
--
--  Widget naming contract (cf. the spec's widget table): the visible
--  text and the accessible name are the same, and both are set
--  explicitly, so that scenarios resolve every control by role +
--  accessible name, independently from the GTK name propagation
--  defaults:
--
--    entry  "What needs to be done?"        new todo input
--    button "Create"                        todo creation (single)
--    button "Mark all as complete"          mark-all toggle
--    button "Complete"                      item completion toggle (one per row)
--    button "Edit"                          edition trigger (single, on the
--                                           selected todo)
--    button "Delete"                        delete control (single, on the
--                                           selected todo)
--    entry  "Edit todo"                     item editing input
--    label  "N items left" / "1 item left"  active counter
--    button "Clear completed"               completed purge
--    button "All" / "Active" / "Completed"  filters
--
--  Selection: activating (clicking) a todo's title selects it; the
--  selected todo is highlighted, and the unique "Edit" and "Delete"
--  buttons apply to it. They are insensitive while no todo is
--  selected.
--
--  Persistence: 'todos.state' in the current directory, with one
--  'U <title>' (undone) or 'D <title>' (done) line per todo, plus a
--  'F <filter>' line for the active filter. It is written on every
--  model change, so that closing the window never loses an
--  acknowledged change. An edition in progress is never persisted
--  (per spec, a restart discards it).
--
--  Note on editing: the reference app edits on double click and
--  cancels with Escape; the semantic vocabulary has no key events nor
--  double clicks, so the spec's equivalent explicit controls are used
--  instead, and pressing the "Edit" button while the selected row's
--  edition is open cancels it (the previous title is restored).
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings;   --  used by Trimmed
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Interfaces.C;
with Interfaces.C.Strings;
with System;

with Glib.Main;
with Glib.Object;
with Gtk.Box;
with Gtk.Button;
with Gtk.Enums;
with Gtk.Event_Controller_Focus;
with Gtk.Frame;
with Gtk.GEntry;
with Gtk.Gesture_Click;
with Gtk.Label;
with Gtk.Main;
with Gtk.Toggle_Button;
with Gtk.Widget;
with Gtk.Window;

procedure TodoMvc is

   package SU renames Ada.Strings.Unbounded;
   use Gtk.Enums;

   --  Model ----------------------------------------------------------------

   Max_Todos : constant := 100;

   type Todo_Array is array (1 .. Max_Todos) of SU.Unbounded_String;
   type Done_Array  is array (1 .. Max_Todos) of Boolean;

   type Filter_Kind is (All_Filter, Active_Filter, Completed_Filter);

   Todos   : Todo_Array;
   Done    : Done_Array;
   Last    : Natural := 0;
   Filter  : Filter_Kind := All_Filter;
   Editing : Natural := 0;
   --  Index of the todo currently in the edit input, 0 when none.

   Selected : Natural := 0;
   --  Index of the todo selected by activating its title, 0 when none:
   --  the unique Edit and Delete controls are insensitive then.

   State_File_Name : constant String := "todos.state";

   --  View -------------------------------------------------------------------

   Win           : Gtk.Window.Gtk_Window;
   Main_Box      : Gtk.Box.Gtk_Box;
   Top_Bar       : Gtk.Box.Gtk_Box;
   List_Frame    : Gtk.Frame.Gtk_Frame;
   List_Box      : Gtk.Box.Gtk_Box;
   Filter_Box    : Gtk.Box.Gtk_Box;
   New_Todo      : Gtk.GEntry.Gtk_Entry;
   Create_Button : Gtk.Button.Gtk_Button;
   Complete_Button : Gtk.Button.Gtk_Button;
   Edit_Button   : Gtk.Button.Gtk_Button;
   Delete_Button : Gtk.Button.Gtk_Button;
   Counter_Label : Gtk.Label.Gtk_Label;
   Mark_All      : Gtk.Button.Gtk_Button;
   Clear_Button  : Gtk.Button.Gtk_Button;
   Footer_Box    : Gtk.Box.Gtk_Box;
   Selection_Bar : Gtk.Box.Gtk_Box;

   type Filter_Buttons_Type is
     array (Filter_Kind) of Gtk.Toggle_Button.Gtk_Toggle_Button;
   Filter_Buttons : Filter_Buttons_Type;

   Current_Editor : Gtk.GEntry.Gtk_Entry;
   --  The edit input of the row being edited, null when no edition.

   Guard : Boolean := False;
   --  True while the view reflects a programmatic change: editors do
   --  not commit, and the signals of synchronized widgets do not
   --  trigger new model changes.

   Quit_Requested : Boolean := False;

   --  Forward declarations ---------------------------------------------------

   procedure Update_View;
   procedure Set_Accessible_Name
     (Wg   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Name : String);
   function Title_Of (Index : Natural) return String;
   function Trimmed  (Text : String) return String;
   function Img      (Index : Natural) return String;
   function Index_Of (Named : String) return Natural;
   procedure Add_Todo    (Title : String);
   procedure Toggle_Todo (Index : Natural);
   procedure Rename_Todo (Index : Natural; Title : String);
   procedure Remove_Todo (Index : Natural);
   procedure Set_All     (State : Boolean);
   procedure Clear_Done;
   procedure Set_Filter  (Kind : Filter_Kind);
   procedure Commit_Edition;
   procedure Handle_New_Todo;
   procedure Save_Todos;
   function  All_Done    return Boolean;
   procedure Load_Todos;

   --  Helpers ---------------------------------------------------------------

   function Trimmed (Text : String) return String is
      First : Natural := Text'First;
      Last  : Natural := Text'Last;
   begin
      while First <= Last and then Text (First) in ' ' | ASCII.HT loop
         First := First + 1;
      end loop;
      while Last >= First and then Text (Last) in ' ' | ASCII.HT loop
         Last := Last - 1;
      end loop;
      return Text (First .. Last);
   end Trimmed;

   function Img (Index : Natural) return String is
      S : constant String := Natural'Image (Index);
   begin
      return S (S'First + 1 .. S'Last);  --  strip the leading blank
   end Img;

   function Title_Of (Index : Natural) return String is
     (SU.To_String (Todos (Index)));

   --  The row index is carried in the widget name ("delete:5"), and
   --  parsed back in the handlers: closures are not available on
   --  rebuilt rows.

   function Index_Of (Named : String) return Natural is
      Col : Natural := 0;
   begin
      for I in reverse Named'Range loop
         if Named (I) = ':' then
            Col := I;
            exit;
         end if;
      end loop;
      if Col = 0 or else Col = Named'Last then
         return 0;
      end if;
      return Natural'Value (Named (Col + 1 .. Named'Last));
   exception
      when Constraint_Error => return 0;
   end Index_Of;

   --  Accessible names -------------------------------------------------------
   --  GTK 4 does not propagate a widget's visible text as the
   --  accessible name of every control (notably entries), so the
   --  naming contract is set explicitly, with
   --  gtk_accessible_update_property. That C function is variadic;
   --  GNAT accepts to import it with its fixed prefix: the string
   --  value, then the -1 list terminator.

   procedure Set_Accessible_Name
     (Wg   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Name : String)
   is
      Prop_Label : constant Interfaces.C.int := 4;
      --  GTK_ACCESSIBLE_PROPERTY_LABEL (enum order checked in the
      --  GtkAda binding).

      procedure G_Accessible_Update_Property
        (Self     : System.Address;
         Property : Interfaces.C.int;
         Text     : Interfaces.C.Strings.chars_ptr;
         List_End : Interfaces.C.int);
      pragma Import (C, G_Accessible_Update_Property,
                        "gtk_accessible_update_property");

   begin
      G_Accessible_Update_Property
        (Glib.Object.Get_Object (Wg.all'Access),
         Prop_Label,
         Interfaces.C.Strings.New_String (Name),
         Interfaces.C.int (-1));
   end Set_Accessible_Name;

   --  Actions ----------------------------------------------------------------
   --  Each model change is saved and reflected immediately: the
   --  storage is always up to date when the window goes away.

   procedure Add_Todo (Title : String) is
   begin
      if Title'Length = 0 or else Last = Max_Todos then
         return;
      end if;
      Last        := Last + 1;
      Todos (Last) := SU.To_Unbounded_String (Title);
      Done (Last)  := False;
      Save_Todos;
      Update_View;
   end Add_Todo;

   procedure Toggle_Todo (Index : Natural) is
   begin
      if Index in 1 .. Last then
         Done (Index) := not Done (Index);
         Save_Todos;
         Update_View;
      end if;
   end Toggle_Todo;

   procedure Rename_Todo (Index : Natural; Title : String) is
   begin
      if Index in 1 .. Last and then Title'Length > 0 then
         Todos (Index) := SU.To_Unbounded_String (Title);
         Save_Todos;
         Update_View;
      end if;
   end Rename_Todo;

   procedure Remove_Todo (Index : Natural) is
   begin
      if Index in 1 .. Last then
         if Editing = Index then
            Editing := 0;
         elsif Editing > Index then
            Editing := Editing - 1;
         end if;
         if Selected = Index then
            Selected := 0;
         elsif Selected > Index then
            Selected := Selected - 1;
         end if;
         for J in Index .. Last - 1 loop
            Todos (J) := Todos (J + 1);
            Done (J)  := Done (J + 1);
         end loop;
         Last := Last - 1;
         Save_Todos;
         Update_View;
      end if;
   end Remove_Todo;

   procedure Set_All (State : Boolean) is
   begin
      for I in 1 .. Last loop
         Done (I) := State;
      end loop;
      Save_Todos;
      Update_View;
   end Set_All;

   procedure Clear_Done is
   begin
      for I in reverse 1 .. Last loop
         if Done (I) then
            --  Each removal rebuilds the view, so the indexes below
            --  stay valid.
            Remove_Todo (I);
         end if;
      end loop;
   end Clear_Done;

   procedure Set_Filter (Kind : Filter_Kind) is
   begin
      Filter := Kind;
      Save_Todos;
      Update_View;
   end Set_Filter;

   procedure Handle_New_Todo is
      Title   : constant String := Trimmed (New_Todo.Get_Text);
      Ignored : Boolean;
   begin
      if Title'Length = 0 then
         --  Empty text is not accepted, from the entry's Enter key or
         --  from the Create button alike: nothing is cleared, nothing
         --  is added.
         return;
      end if;
      New_Todo.Set_Text ("");
      Ignored := New_Todo.Grab_Focus;
      Add_Todo (Title);
      pragma Unreferenced (Ignored);
   end Handle_New_Todo;

   --  Handlers ---------------------------------------------------------------
   --  They receive the widget only, hence the widget-name based
   --  dispatch above.

   procedure On_Complete_Clicked
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Toggle_Todo (Selected);
   end On_Complete_Clicked;

   procedure On_Edit_Clicked
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
      Idx : constant Natural := Selected;
   begin
      if Idx not in 1 .. Last then
         return;
      end if;
      if Editing = Idx then
         --  Second press on the Edit button while the selected row's
         --  edition is open: cancel it, the previous title is
         --  restored untouched.
         Current_Editor := null;
         Editing := 0;
         Update_View;
      else
         --  Starting another edition commits the current one first,
         --  as leaving the field would.
         Commit_Edition;
         Editing := Idx;
         Update_View;
      end if;
   end On_Edit_Clicked;

   procedure On_Delete_Clicked
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Remove_Todo (Selected);
   end On_Delete_Clicked;

   procedure On_Row_Clicked
     (Self    : access Gtk.Gesture_Click.Gtk_Gesture_Click_Record'Class;
      N_Press : Glib.Gint;
      X, Y    : Glib.Gdouble)
   is
      pragma Unreferenced (N_Press, X, Y);
   begin
      Selected := Index_Of (Self.Get_Widget.Get_Name);
      Update_View;
   end On_Row_Clicked;

   procedure On_Edit_Activate
     (Self : access Gtk.GEntry.Gtk_Entry_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Commit_Edition;
   end On_Edit_Activate;

   procedure On_Edit_Focus_Out
     (Self : access Gtk.Event_Controller_Focus.
                     Gtk_Event_Controller_Focus_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      if not Guard then
         Commit_Edition;
      end if;
   end On_Edit_Focus_Out;

   procedure On_New_Todo_Activate
     (Self : access Gtk.GEntry.Gtk_Entry_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Handle_New_Todo;
   end On_New_Todo_Activate;

   procedure On_Create_Clicked
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Handle_New_Todo;
   end On_Create_Clicked;

   procedure On_Mark_All_Clicked
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      if not Guard then
         --  The mark-all control applies its inferred state to all
         --  the todos: everything is checked when it is pressed on a
         --  not-all-checked state, and everything is unchecked
         --  otherwise.
         Set_All (not All_Done);
      end if;
   end On_Mark_All_Clicked;

   procedure On_Clear_Clicked
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Clear_Done;
   end On_Clear_Clicked;

   procedure On_Filter_Clicked
     (Self : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Name : constant String := Self.Get_Name;
      Kind : constant Filter_Kind :=
               (if    Name = "filter:All"       then All_Filter
                elsif Name = "filter:Active"    then Active_Filter
                else  Completed_Filter);
   begin
      if not Guard and then Kind /= Filter then
         Set_Filter (Kind);
      end if;
   end On_Filter_Clicked;

   function On_Close_Request
     (Self : access Gtk.Window.Gtk_Window_Record'Class) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Quit_Requested := True;
      return True;
   end On_Close_Request;

   procedure On_Window_Destroy
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Quit_Requested := True;
   end On_Window_Destroy;

   --  State queries ----------------------------------------------------------

   function Active_Count return Natural is
      N : Natural := 0;
   begin
      for I in 1 .. Last loop
         if not Done (I) then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Active_Count;

   function Any_Done return Boolean is
     (for some I in 1 .. Last => Done (I));

   function All_Done return Boolean is
     (Last = 0 or else (for all I in 1 .. Last => Done (I)));

   function Counter_Text return String is
      N : constant Natural := Active_Count;
      S : constant String  := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last)
        & (if N = 1 then " item left" else " items left");
   end Counter_Text;

   --  View update --------------------------------------------------------------
   --  The todo rows are rebuilt on every model change: it keeps the
   --  visible state and the accessible names trivially synchronised
   --  with the model, and the widget-name based handlers stateless.

   procedure Remove_Children (Box : not null access Gtk.Box.Gtk_Box_Record'Class) is
      use type Gtk.Widget.Gtk_Widget;
      Child : Gtk.Widget.Gtk_Widget;
   begin
      loop
         Child := Box.Get_First_Child;
         exit when Child = null;
         Box.Remove (Child);
      end loop;
   end Remove_Children;

   procedure Update_View is

      procedure Build_Row (Index : Positive) is
         Title  : constant String := Title_Of (Index);
         Row    : Gtk.Box.Gtk_Box;
         Row_Label : Gtk.Label.Gtk_Label;
         Complete_Btn : Gtk.Button.Gtk_Button;
         Row_Editor   : Gtk.GEntry.Gtk_Entry;
         Gesture      : Gtk.Gesture_Click.Gtk_Gesture_Click;
         Ignored      : Boolean;

         procedure New_Button
           (Ctrl : out Gtk.Button.Gtk_Button;
            Kind : String;
            Text : String) is
         begin
            Gtk.Button.Gtk_New (Ctrl, Text);
            Ctrl.Set_Name (Kind & ":" & Img (Index));
            Ctrl.Set_Halign (Gtk.Enums.Align_End);
            Set_Accessible_Name (Ctrl, Text);
         end New_Button;

      begin
         Gtk.Box.Gtk_New (Row, Gtk.Enums.Orientation_Horizontal, 6);

         Gtk.Label.Gtk_New (Row_Label, Title);
         Row_Label.Set_Hexpand (True);
         Row_Label.Set_Xalign (0.0);
         Row_Label.Set_Name ("row:" & Img (Index));
         --  Per spec: a completed todo is green (keeping the
         --  strikethrough), a todo still to be done is blue; the
         --  selection adds the bold aspect on top.
         declare
            Color     : constant String :=
                          (if Done (Index) then "#2e7d32"
                           else "#1565c0");
            Body_Text : constant String :=
                          (if Done (Index)
                           then "<s>" & Title & "</s>"
                           else Title);
         begin
            if Selected = Index then
               Row_Label.Set_Markup
                 ("<b><span foreground=""" & Color & """>"
                  & Body_Text & "</span></b>");
            else
               Row_Label.Set_Markup
                 ("<span foreground=""" & Color & """>"
                  & Body_Text & "</span>");
            end if;
         end;
         Gtk.Gesture_Click.Gtk_New (Gesture);
         Gesture.On_Pressed (On_Row_Clicked'Unrestricted_Access);
         Row_Label.Add_Controller (Gesture);
         Row.Append (Row_Label);

         Gtk.GEntry.Gtk_New (Row_Editor);
         Row_Editor.Set_Text (Title);
         Set_Accessible_Name (Row_Editor, "Edit todo");
         Row_Editor.Set_Name ("editor:" & Img (Index));
         Row_Editor.On_Activate (On_Edit_Activate'Unrestricted_Access);
         Row.Append (Row_Editor);

         if Editing = Index then
            --  The edition replaces the item rendering by the edit
            --  input, focused, holding the current title.
            Row_Label.Set_Visible (False);
            Row_Editor.Set_Visible (True);

            Current_Editor := Row_Editor;
            declare
               Focus_Ctl : Gtk.Event_Controller_Focus.
                             Gtk_Event_Controller_Focus;
            begin
               Gtk.Event_Controller_Focus.Gtk_New (Focus_Ctl);
               Focus_Ctl.On_Leave (On_Edit_Focus_Out'Unrestricted_Access);
               Row_Editor.Add_Controller (Focus_Ctl);
            end;
            Ignored := Row_Editor.Grab_Focus;
         else
            Row_Editor.Set_Visible (False);
         end if;

         List_Box.Append (Row);
         Row.Set_Visible (True);
         case Filter is
            when All_Filter       => null;
            when Active_Filter    =>
               Row.Set_Visible (not Done (Index));
            when Completed_Filter =>
               Row.Set_Visible (Done (Index));
         end case;
      end Build_Row;

   begin
      Guard := True;

      Remove_Children (List_Box);
      --  Todos appear in their creation order, whatever their
      --  completion state (per spec).
      for I in 1 .. Last loop
         Build_Row (I);
      end loop;

      Mark_All.Set_Visible (Last > 0);
      Footer_Box.Set_Visible (Last > 0);
      Selection_Bar.Set_Visible (Last > 0);
      Counter_Label.Set_Visible (Last > 0);
      Filter_Box.Set_Visible (Last > 0);
      Clear_Button.Set_Visible (Any_Done);

      Edit_Button.Set_Sensitive (Selected in 1 .. Last);
      Delete_Button.Set_Sensitive (Selected in 1 .. Last);
      Complete_Button.Set_Sensitive (Selected in 1 .. Last);

      if Last > 0 then
         Counter_Label.Set_Text (Counter_Text);
      end if;

      for F in Filter_Kind loop
         Filter_Buttons (F).Set_Active (F = Filter);
      end loop;

      Guard := False;
   end Update_View;

   --  Persistence ---------------------------------------------------------------

   procedure Save_Todos is
      File : Ada.Text_IO.File_Type;
   begin
      if Last = 0 then
         --  An empty todo list means no leftovers: a fresh app on the
         --  next start shows no list and no footer.
         if Ada.Directories.Exists (State_File_Name) then
            Ada.Directories.Delete_File (State_File_Name);
         end if;
         return;
      end if;

      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, State_File_Name);
      Ada.Text_IO.Put_Line
        (File, "F " & (case Filter is
                          when All_Filter       => "All",
                          when Active_Filter    => "Active",
                          when Completed_Filter => "Completed"));
      for I in 1 .. Last loop
         Ada.Text_IO.Put_Line
           (File, (if Done (I) then "D " else "U ")
                  & SU.To_String (Todos (I)));
      end loop;
      Ada.Text_IO.Close (File);
   end Save_Todos;

   procedure Load_Todos is
      use Ada.Text_IO;
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 1024);
      Len  : Natural;
   begin
      Last   := 0;
      Filter := Filter_Kind'First;
      if not Ada.Directories.Exists (State_File_Name) then
         return;
      end if;

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, State_File_Name);
      while not End_Of_File (File) loop
         Get_Line (File, Line, Len);
         if Len > 2 then
            if Line (1 .. 2) = "F " then
               Filter :=
                 (if    Line (3 .. Len) = "Active"    then Active_Filter
                  elsif Line (3 .. Len) = "Completed" then Completed_Filter
                  else All_Filter);
            else
               Last         := Last + 1;
               Todos (Last) := SU.To_Unbounded_String (Line (3 .. Len));
               Done (Last)  := (Line (1) = 'D');
            end if;
         end if;
      end loop;
      Ada.Text_IO.Close (File);
   exception
      when Ada.Text_IO.Name_Error =>
         null;  --  no readable state file: fresh app
   end Load_Todos;

   --  Edition ------------------------------------------------------------------

   procedure Commit_Edition is
      use type Gtk.GEntry.Gtk_Entry;
      Idx  : constant Natural := Editing;
      Text : constant String :=
               (if Current_Editor /= null
                then Trimmed (Current_Editor.Get_Text)
                else "");
   begin
      if Idx = 0 then
         return;
      end if;
      --  Detach first: the model update below rebuilds the view, and
      --  destroys the editor whose signals are being processed.
      Current_Editor := null;
      Editing := 0;
      if Text'Length = 0 then
         --  An edition saved as empty destroys the todo.
         Remove_Todo (Idx);
      elsif Text /= Title_Of (Idx) then
         Rename_Todo (Idx, Text);
      else
         Update_View;
      end if;
   end Commit_Edition;

   --  Main ----------------------------------------------------------------------

   Ignored : Boolean;

begin
   Gtk.Main.Init;
   Load_Todos;

   Gtk.Window.Gtk_New (Win);
   Win.Set_Title ("ToDoMVC");

   Gtk.Box.Gtk_New (Main_Box, Gtk.Enums.Orientation_Vertical, 6);
   Win.Set_Child (Main_Box);

   Gtk.Box.Gtk_New (Top_Bar, Gtk.Enums.Orientation_Horizontal, 6);
   Main_Box.Append (Top_Bar);

   Gtk.GEntry.Gtk_New (New_Todo);
   New_Todo.Set_Placeholder_Text ("What needs to be done?");
   Set_Accessible_Name (New_Todo, "What needs to be done?");
   New_Todo.Set_Name ("new-todo");
   New_Todo.Set_Hexpand (True);
   Top_Bar.Append (New_Todo);
   New_Todo.On_Activate (On_New_Todo_Activate'Unrestricted_Access);

   Gtk.Button.Gtk_New (Create_Button, "Create");
   Set_Accessible_Name (Create_Button, "Create");
   Create_Button.Set_Halign (Gtk.Enums.Align_End);
   Top_Bar.Append (Create_Button);
   Create_Button.On_Clicked (On_Create_Clicked'Unrestricted_Access);

   Gtk.Box.Gtk_New (Filter_Box, Gtk.Enums.Orientation_Horizontal, 6);
   Main_Box.Append (Filter_Box);

   for F in Filter_Kind loop
      declare
         Label : constant String :=
                   (case F is
                       when All_Filter       => "All",
                       when Active_Filter    => "Active",
                       when Completed_Filter => "Completed");
      begin
         Gtk.Toggle_Button.Gtk_New_With_Label (Filter_Buttons (F), Label);
         Set_Accessible_Name (Filter_Buttons (F), Label);
         Filter_Buttons (F).Set_Name ("filter:" & Label);
         Filter_Buttons (F).On_Clicked
           (On_Filter_Clicked'Unrestricted_Access);
         Gtk.Box.Append (Filter_Box, Filter_Buttons (F));
      end;
   end loop;

   Gtk.Box.Gtk_New (List_Box, Gtk.Enums.Orientation_Vertical, 6);

   --  The list is set off from the surrounding window content: the
   --  frame (a hollow box) wraps it, per spec.

   Gtk.Frame.Gtk_New (List_Frame);
   List_Frame.Set_Child (List_Box);
   Main_Box.Append (List_Frame);

   --  The selection-scoped controls: unique Edit and Delete buttons,
   --  applying to the selected todo, insensitive while the selection
   --  is empty (per spec).

   Gtk.Box.Gtk_New (Selection_Bar, Gtk.Enums.Orientation_Horizontal, 6);
   Main_Box.Append (Selection_Bar);

   Gtk.Button.Gtk_New (Edit_Button, "Edit");
   Set_Accessible_Name (Edit_Button, "Edit");
   Edit_Button.Set_Name ("edit");
   Edit_Button.Set_Sensitive (False);
   Selection_Bar.Append (Edit_Button);
   Edit_Button.On_Clicked (On_Edit_Clicked'Unrestricted_Access);

   Gtk.Button.Gtk_New (Delete_Button, "Delete");
   Set_Accessible_Name (Delete_Button, "Delete");
   Delete_Button.Set_Name ("delete");
   Delete_Button.Set_Sensitive (False);
   Selection_Bar.Append (Delete_Button);
   Delete_Button.On_Clicked (On_Delete_Clicked'Unrestricted_Access);

   Gtk.Button.Gtk_New (Complete_Button, "Complete");
   Set_Accessible_Name (Complete_Button, "Complete");
   Complete_Button.Set_Name ("complete");
   Complete_Button.Set_Sensitive (False);
   Selection_Bar.Append (Complete_Button);
   Complete_Button.On_Clicked (On_Complete_Clicked'Unrestricted_Access);

   Win.On_Close_Request (On_Close_Request'Unrestricted_Access);
   Win.On_Destroy (On_Window_Destroy'Unrestricted_Access);

   --  The footer, at the bottom: the mark-all and purge controls
   --  together, side by side, on the same line, with the counter,
   --  per spec.

   Gtk.Box.Gtk_New (Footer_Box, Gtk.Enums.Orientation_Horizontal, 6);
   Main_Box.Append (Footer_Box);

   Gtk.Label.Gtk_New (Counter_Label, "");
   Counter_Label.Set_Hexpand (True);
   Footer_Box.Append (Counter_Label);

   Gtk.Button.Gtk_New (Mark_All, "Mark all as complete");
   Set_Accessible_Name (Mark_All, "Mark all as complete");
   Mark_All.Set_Halign (Gtk.Enums.Align_End);
   Footer_Box.Append (Mark_All);
   Mark_All.On_Clicked (On_Mark_All_Clicked'Unrestricted_Access);

   Gtk.Button.Gtk_New (Clear_Button, "Clear completed");
   Set_Accessible_Name (Clear_Button, "Clear completed");
   Clear_Button.Set_Halign (Gtk.Enums.Align_End);
   Footer_Box.Append (Clear_Button);
   Clear_Button.On_Clicked (On_Clear_Clicked'Unrestricted_Access);

   Win.Set_Visible (True);
   Update_View;
   Win.Present;
   Ignored := New_Todo.Grab_Focus;

   while not Quit_Requested loop
      declare
         Pending : constant Boolean :=
           Glib.Main.Main_Context_Iteration (May_Block => False);
         pragma Unreferenced (Pending);
      begin
         delay 0.02;
      end;
   end loop;
end TodoMvc;
