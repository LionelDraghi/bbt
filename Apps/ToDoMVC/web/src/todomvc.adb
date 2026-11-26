-----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026 Lionel Draghi
-----------------------------------------------------------------------------
--  Web implementation of the ToDoMVC witness app (cf. the behavior
--  spec in Apps/ToDoMVC/ToDoMVC.md), symmetric to the GTK 4 one
--  (../gtk4/src/todomvc.adb): same model, same widget naming
--  contract, same storage format, driven by the webdriver GUI backend
--  instead of AT-SPI.
--
--  The application is a gnoga server: the model lives in this
--  process, the browser page is the spec's "window" (G005: the page
--  and its document), and every connection builds its own view of the
--  shared model — a model change refreshes the view of each open page.
--
--  Widget naming contract (cf. the spec's widget table): role and
--  accessible name are exposed through ARIA, set explicitly, so that
--  scenarios resolve every control by role + accessible name:
--
--    textbox        "New task"                 new todo input
--                                              (placeholder:
--                                              "What needs to be done?")
--    button         "Create"                   todo creation (single)
--    list           "To do"                    todos still to be done
--    list           "Done"                     completed todos (G080)
--                                              (each list is a visible
--                                              zone: colored border and
--                                              header label, visual only)
--    button         "Mark all as complete"     mark-all
--    button         "Complete"                 completion toggle, on the
--                                              selected todo
--    button         "Edit"                     edition trigger, on the
--                                              selected todo
--    button         "Delete"                   delete control, on the
--                                              selected todo
--    textbox        "Edit todo"                edition input
--    text           "N items left"/"1 item left" active counter
--    button         "Clear completed"          completed purge
--    toggle button  "All"/"Active"/"Completed" filters (aria-pressed)
--
--  Each todo is a listitem named with the todo title: activating one
--  selects the todo, exactly like a click on its title in the GTK app.
--  The selected todo is highlighted (bold title), and the unique
--  "Edit", "Delete" and "Complete" buttons apply to it, disabled
--  while the selection is empty.
--
--  The rows are rebuilt on every model change, like the GTK app does:
--  the visible state and the accessible names stay trivially
--  synchronized with the model, and the handlers stay stateless — the
--  todo index is carried by the row's DOM id ("row-<n>"), the same way
--  the GTK app carries it in the widget name.
--
--  Persistence: 'todos.state' in the current directory, with one
--  'U <title>' (undone) or 'D <title>' (done) line per todo, plus a
--  'F <filter>' line for the active filter. It is written on every
--  model change, so that closing the page never loses an acknowledged
--  change. An edition in progress is never persisted (per spec, a
--  restart discards it).
--
--  Note on editing: the reference app edits on double click and
--  cancels with Escape; the semantic vocabulary has no key events nor
--  double clicks, so the spec's equivalent explicit controls are used
--  instead. Validating the edit input (Enter) and leaving it both
--  commit the edition, as does selecting another todo.
--
--  Lifetime: the server serves pages until the runner ends the
--  process — closing the page does not stop the application (the spec
--  leaves windowless applications out of the close step's scope, cf.
--  G095).
--
--  Port: 8080 by default, overridden by gnoga's own
--  '--gnoga-port=<n>' switch. The serving URL is printed on the error
--  output before serving, so that the backend can find the port.
-----------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with UXStrings;

with Gnoga.Application;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.List;
with Gnoga.Types;

procedure TodoMvc is

   package SU     renames Ada.Strings.Unbounded;
   package UX     renames UXStrings;
   package Common renames Gnoga.Gui.Element.Common;
   package Forms  renames Gnoga.Gui.Element.Form;
   package Lists  renames Gnoga.Gui.Element.List;
   package Base   renames Gnoga.Gui.Base;
   use Forms;
   --  The edit input is reached through an access value: gnoga's
   --  implicit dereference of such a value is only legal when its
   --  access type is directly visible, hence this use clause.

   --  Model ------------------------------------------------------------------
   --  One model for the process, exactly like the GTK app's single
   --  window: the pages are views of it.

   Max_Todos : constant := 100;

   type Todo_Array is array (1 .. Max_Todos) of SU.Unbounded_String;
   type Done_Array  is array (1 .. Max_Todos) of Boolean;

   type Filter_Kind is (All_Filter, Active_Filter, Completed_Filter);

   Todos   : Todo_Array;
   Done    : Done_Array;
   Last    : Natural     := 0;
   Filter  : Filter_Kind := All_Filter;
   Editing : Natural     := 0;
   --  Index of the todo currently in its edit input, 0 when none.

   Selected : Natural := 0;
   --  Index of the todo selected by activating it, 0 when none: the
   --  unique Edit, Delete and Complete controls are disabled then.

   Guard : Boolean := False;
   --  True while the views are being rebuilt: the events a rebuild
   --  may provoke do not start new model changes.

   State_File_Name : constant String := "todos.state";

   --  Views ------------------------------------------------------------------
   --  One view per connection. The elements belong to the connection:
   --  each page shows the shared model with its own widgets.

   type View_Data_Type;
   type View_Data_Access is access all View_Data_Type;

   type Filter_Buttons is array (Filter_Kind) of Common.Button_Type;

   type View_Data_Type is new Gnoga.Types.Connection_Data_Type with record
      Conn         : Gnoga.Types.Connection_ID := Gnoga.Types.No_Connection;
      Live         : Boolean := True;

      Root         : Gnoga.Gui.View.View_Type;

      Creation     : Forms.Form_Type;
      New_Todo     : Forms.Text_Type;
      Create_Btn   : Common.Button_Type;

      Filters_Box  : Common.DIV_Type;
      Filters      : Filter_Buttons;

      Slots        : Common.DIV_Type;
      Todo_Zone    : Common.DIV_Type;
      Done_Zone    : Common.DIV_Type;
      Todo_List    : Lists.Unordered_List_Type;
      Done_List    : Lists.Unordered_List_Type;

      Selection    : Common.DIV_Type;
      Edit_Btn     : Common.Button_Type;
      Delete_Btn   : Common.Button_Type;
      Complete_Btn : Common.Button_Type;

      Footer       : Common.DIV_Type;
      Counter      : Common.Span_Type;
      Mark_All     : Common.Button_Type;
      Clear_Btn    : Common.Button_Type;

      Editor       : Forms.Text_Access := null;
      --  The edit input of this page, while an edition is open on it.
   end record;

   Max_Views : constant := 4;
   type Views_Array is array (1 .. Max_Views) of View_Data_Access;
   Views : Views_Array := [others => null];

   Current_Editor : Forms.Text_Access := null;
   --  The edit input the open edition writes to. The edition is a
   --  shared model state (like the selection): a rebuild of any page
   --  rebuilds the editing row's input, and this is the last one.

   --  Helpers -----------------------------------------------------------------

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

   function Filter_Label (Kind : Filter_Kind) return String is
     (case Kind is
         when All_Filter       => "All",
         when Active_Filter    => "Active",
         when Completed_Filter => "Completed");

   function Filter_Id (Kind : Filter_Kind) return String is
     (case Kind is
         when All_Filter       => "filter-all",
         when Active_Filter    => "filter-active",
         when Completed_Filter => "filter-completed");

   function Index_Of (Id : String) return Natural is
     --  The todo index is carried by the DOM id ("row-5") and parsed
     --  back in the handlers: the rows are rebuilt on every change.
      Dash : Natural := 0;
   begin
      for I in reverse Id'Range loop
         if Id (I) = '-' then
            Dash := I;
            exit;
         end if;
      end loop;
      if Dash = 0 or else Dash = Id'Last then
         return 0;
      end if;
      return Natural'Value (Id (Dash + 1 .. Id'Last));
   exception
      when Constraint_Error => return 0;
   end Index_Of;

   function UTF (Text : String) return UX.UXString is (UX.From_UTF_8 (Text));
   function Raw (Text : UX.UXString) return String is (UX.To_UTF_8 (Text));
   --  The gnoga API speaks UXString, the model speaks plain String.

   function Element_Id (Object : Base.Base_Type'Class) return String is
     (Raw (Base.ID (Base.Base_Type (Object))));

   function Connection_Of
     (Object : Base.Base_Type'Class)
      return Gnoga.Types.Connection_ID is
     (Base.Connection_ID (Base.Base_Type (Object)));

   --  State queries -----------------------------------------------------------

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
   begin
      return Img (N) & (if N = 1 then " item left" else " items left");
   end Counter_Text;

   --  Forward declarations ----------------------------------------------------

   procedure Update_View (D : View_Data_Access);
   procedure Refresh_All;
   procedure Build_Row_In
     (D     : View_Data_Access;
      Index : Positive;
      L     : in out Lists.Ordered_List_Type'Class);
   procedure Add_Todo    (Title : String);
   procedure Toggle_Todo (Index : Natural);
   procedure Rename_Todo (Index : Natural; Title : String);
   procedure Remove_Todo (Index : Natural);
   procedure Set_All     (State : Boolean);
   procedure Clear_Done;
   procedure Set_Filter  (Kind : Filter_Kind);
   procedure Commit_Edition;
   procedure Handle_New_Todo (D : View_Data_Access);
   procedure Save_Todos;
   procedure Load_Todos;

   function View_Of
     (Object : Base.Base_Type'Class)
      return View_Data_Access;

   --  Handlers -----------------------------------------------------------------
   --  They receive the event's object only: the view comes from its
   --  connection, the todo index from the object's DOM id.

   procedure On_Row_Clicked (Object : in out Base.Base_Type'Class) is
      D   : constant View_Data_Access := View_Of (Object);
      Idx : constant Natural := Index_Of (Element_Id (Object));
   begin
      if D = null or else Guard then
         return;
      end if;
      if Editing /= 0 then
         if Editing = Idx then
            --  A click inside the edition's own row (the edit input
            --  itself): the field is not left, the edition goes on.
            return;
         end if;
         --  Selecting another todo leaves the field: the edition
         --  commits first, like the blur of a real user.
         Commit_Edition;
      end if;
      if Idx in 1 .. Last then
         Selected := Idx;
         Refresh_All;
      end if;
   end On_Row_Clicked;

   procedure On_Editor_Blur (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      if not Guard and then Editing /= 0 then
         Commit_Edition;
      end if;
   end On_Editor_Blur;

   procedure On_Editor_Submit (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      --  Validating the edit input (Enter): gnoga's submit handler
      --  binding suppresses the page reload the browser would do.
      if not Guard and then Editing /= 0 then
         Commit_Edition;
      end if;
   end On_Editor_Submit;

   procedure On_New_Todo_Submit (Object : in out Base.Base_Type'Class) is
      D : constant View_Data_Access := View_Of (Object);
   begin
      if D /= null and then not Guard then
         Handle_New_Todo (D);
      end if;
   end On_New_Todo_Submit;

   procedure On_Create_Clicked (Object : in out Base.Base_Type'Class) is
      D : constant View_Data_Access := View_Of (Object);
   begin
      if D /= null and then not Guard then
         Handle_New_Todo (D);
      end if;
   end On_Create_Clicked;

   procedure On_Complete_Clicked (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      if not Guard then
         --  The completion toggle applies to the selected todo, done
         --  or not: pressing it again uncompletes it.
         Toggle_Todo (Selected);
      end if;
   end On_Complete_Clicked;

   procedure On_Edit_Clicked (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
      Idx : constant Natural := Selected;
   begin
      if Guard or else Idx not in 1 .. Last then
         return;
      end if;
      if Editing = Idx then
         --  Second press while the selected row's edition is open:
         --  cancel it, the previous title is restored untouched.
         Current_Editor := null;
         Editing := 0;
         Refresh_All;
      else
         Editing := Idx;
         Refresh_All;
      end if;
   end On_Edit_Clicked;

   procedure On_Delete_Clicked (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      if not Guard then
         Remove_Todo (Selected);
      end if;
   end On_Delete_Clicked;

   procedure On_Mark_All_Clicked (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      if not Guard then
         --  The mark-all control applies the inferred state to all the
         --  todos: everything is completed when nothing is, and
         --  everything is uncompleted otherwise.
         Set_All (not All_Done);
      end if;
   end On_Mark_All_Clicked;

   procedure On_Clear_Clicked (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      if not Guard then
         Clear_Done;
      end if;
   end On_Clear_Clicked;

   procedure On_Filter_Clicked (Object : in out Base.Base_Type'Class) is
      Id   : constant String := Element_Id (Object);
      Kind : constant Filter_Kind :=
               (if    Id = "filter-all"    then All_Filter
                elsif Id = "filter-active" then Active_Filter
                else  Completed_Filter);
   begin
      if not Guard and then Kind /= Filter then
         Set_Filter (Kind);
      end if;
   end On_Filter_Clicked;

   --  View resolution ---------------------------------------------------------

   function View_Of
     (Object : Base.Base_Type'Class)
      return View_Data_Access
   is
      Id : constant Gnoga.Types.Connection_ID := Connection_Of (Object);
   begin
      for I in Views'Range loop
         if Views (I) /= null and then Views (I).Conn = Id then
            return Views (I);
         end if;
      end loop;
      return null;
   end View_Of;

   --  Actions -----------------------------------------------------------------
   --  Each model change is saved and reflected immediately: the
   --  storage is always up to date when the page goes away.

   procedure Add_Todo (Title : String) is
   begin
      if Title'Length = 0 or else Last = Max_Todos then
         return;
      end if;
      Last        := Last + 1;
      Todos (Last) := SU.To_Unbounded_String (Title);
      Done (Last)  := False;
      Save_Todos;
      Refresh_All;
   end Add_Todo;

   procedure Toggle_Todo (Index : Natural) is
   begin
      if Index in 1 .. Last then
         Done (Index) := not Done (Index);
         Save_Todos;
         Refresh_All;
      end if;
   end Toggle_Todo;

   procedure Rename_Todo (Index : Natural; Title : String) is
   begin
      if Index in 1 .. Last and then Title'Length > 0 then
         Todos (Index) := SU.To_Unbounded_String (Title);
         Save_Todos;
         Refresh_All;
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
         Refresh_All;
      end if;
   end Remove_Todo;

   procedure Set_All (State : Boolean) is
   begin
      for I in 1 .. Last loop
         Done (I) := State;
      end loop;
      Save_Todos;
      Refresh_All;
   end Set_All;

   procedure Clear_Done is
   begin
      for I in reverse 1 .. Last loop
         if Done (I) then
            --  Each removal refreshes the views, so the indexes below
            --  stay valid.
            Remove_Todo (I);
         end if;
      end loop;
   end Clear_Done;

   procedure Set_Filter (Kind : Filter_Kind) is
   begin
      Filter := Kind;
      Save_Todos;
      Refresh_All;
   end Set_Filter;

   procedure Handle_New_Todo (D : View_Data_Access) is
      Title : constant String := Trimmed (Raw (D.New_Todo.Value));
   begin
      if Title'Length = 0 then
         --  Empty text is not accepted, from the input's Enter key or
         --  from the Create button alike: nothing is cleared, nothing
         --  is added.
         return;
      end if;
      D.New_Todo.Value ("");
      --  The refresh rebuilds the rows only: the creation line keeps
      --  its focus, an added todo does not move the user's cursor.
      Add_Todo (Title);
   end Handle_New_Todo;

   --  Persistence -------------------------------------------------------------

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
      Ada.Text_IO.Put_Line (File, "F " & Filter_Label (Filter));
      for I in 1 .. Last loop
         Ada.Text_IO.Put_Line
           (File, (if Done (I) then "D " else "U ") & SU.To_String (Todos (I)));
      end loop;
      Ada.Text_IO.Close (File);
   end Save_Todos;

   procedure Load_Todos is
      use Ada.Text_IO;
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 1024);
      Len  : Natural;
   begin
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
            elsif Last < Max_Todos then
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
      Idx  : constant Natural := Editing;
      Text : constant String :=
               (if Current_Editor /= null
                then Trimmed (Raw (Current_Editor.all.Value))
                else "");
   begin
      if Idx = 0 then
         return;
      end if;
      --  Detach first: the model update below rebuilds the views, and
      --  drops the edit input whose value is being read above.
      Current_Editor := null;
      Editing := 0;
      if Text'Length = 0 then
         --  An edition saved as empty destroys the todo.
         Remove_Todo (Idx);
      elsif Text /= Title_Of (Idx) then
         Rename_Todo (Idx, Text);
      else
         Refresh_All;
      end if;
   end Commit_Edition;

   --  View --------------------------------------------------------------------

   function Is_Shown (Index : Natural) return Boolean is
     (Filter = All_Filter
      or else (Filter = Active_Filter and then not Done (Index))
      or else (Filter = Completed_Filter and then Done (Index)));
   --  Per spec, the active filter shows its subset: a todo updated so
   --  that it no longer matches disappears from the list.

   procedure Build_Row_In
     (D     : View_Data_Access;
      Index : Positive;
      L     : in out Lists.Ordered_List_Type'Class)
   is
      Title : constant String := Title_Of (Index);
      Row   : constant Lists.List_Item_Access := new Lists.List_Item_Type;
      Title_Span : Common.Span_Type;
   begin
      --  The row is the semantic list item: named with the todo title,
      --  and the activation target (selecting the todo). Marked
      --  dynamic before being parented: the list frees it with the
      --  connection, once its node is gone.
      Row.all.Dynamic;
      Row.Create (L, "", UTF ("row-" & Img (Index)));
      Row.Attribute ("aria-label", UTF (Title));
      Row.On_Click_Handler (On_Row_Clicked'Unrestricted_Access);

      Title_Span.Create (Row.all, "", "");
      --  A row is an element, not a view: gnoga only auto-places the
      --  children of views, so the title is placed explicitly.
      Title_Span.Place_Inside_Bottom_Of (Row.all);
      Title_Span.Add_Class ("title");
      if Done (Index) then
         Title_Span.Add_Class ("done");
      end if;
      if Selected = Index then
         Title_Span.Add_Class ("selected");
      end if;
      Title_Span.Text (UTF (Title));

      if Editing = Index then
         --  The edition replaces the item rendering by the edit input,
         --  focused, holding the current title: it only exists while
         --  the edition is open. Both are created per edition, and
         --  their DOM parent is the row (not a view), so gnoga does
         --  not take their ownership: they stay allocated for the
         --  connection's lifetime, an edition being a rare event.
         declare
            Editor_Form : constant Forms.Form_Access := new Forms.Form_Type;
            Editor      : constant Forms.Text_Access := new Forms.Text_Type;
         begin
            Editor_Form.all.Dynamic;
            Editor_Form.all.Create (Row.all, Action => "",
                                    ID => UTF ("edit-form-" & Img (Index)));
            Editor_Form.all.Place_Inside_Bottom_Of (Row.all);
            Editor_Form.all.On_Submit_Handler
              (On_Editor_Submit'Unrestricted_Access);

            Editor.all.Dynamic;
            Editor.all.Create
              (Editor_Form.all, Size => 40, Value => UTF (Title),
               Name => "edit-todo", ID => UTF ("editor-" & Img (Index)));
            Editor.all.Attribute ("aria-label", UTF ("Edit todo"));
            Editor.all.On_Blur_Handler
              (On_Editor_Blur'Unrestricted_Access);

            D.Editor := Editor;
            Current_Editor := Editor;
         end;
      end if;
   end Build_Row_In;

   procedure Update_View (D : View_Data_Access) is
   begin
      Guard := True;

      D.Editor := null;
      --  Clear before rebuilding: the rows are recreated from the
      --  model, their former nodes and bindings go away with them.
      D.Todo_List.Inner_HTML ("");
      D.Done_List.Inner_HTML ("");

      --  Todos appear in their creation order, whatever their
      --  completion state: a done todo sits in the Done list, an
      --  undone one in the To do list (per spec).
      for I in 1 .. Last loop
         if Is_Shown (I) then
            if Done (I) then
               Build_Row_In (D, I, D.Done_List);
            else
               Build_Row_In (D, I, D.Todo_List);
            end if;
         end if;
      end loop;

      --  Per spec: no todo, no filters and no footer; the creation
      --  line stays (the app must stay usable to add the first one).
      D.Filters_Box.Hidden (Last = 0);
      D.Selection.Hidden (Last = 0);
      D.Footer.Hidden (Last = 0);
      D.Clear_Btn.Hidden (not Any_Done);

      D.Edit_Btn.Disabled (Selected not in 1 .. Last);
      D.Delete_Btn.Disabled (Selected not in 1 .. Last);
      D.Complete_Btn.Disabled (Selected not in 1 .. Last);

      if Last > 0 then
         D.Counter.Text (UTF (Counter_Text));
      end if;

      for F in Filter_Kind loop
         D.Filters (F).Attribute
           ("aria-pressed", UTF (if F = Filter then "true" else "false"));
      end loop;

      if D.Editor /= null then
         D.Editor.all.Focus;
      end if;

      Guard := False;
   end Update_View;

   procedure Refresh_All is
   begin
      for I in Views'Range loop
         if Views (I) /= null and then Views (I).Live then
            begin
               Update_View (Views (I));
            exception
               when others =>
                  --  A page that went away between the check and the
                  --  update: drop its view, the others go on.
                  Views (I).Live := False;
            end;
         end if;
      end loop;
   end Refresh_All;

   --  Connection ---------------------------------------------------------------

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      D            : constant View_Data_Access := new View_Data_Type;
      Header_Todo  : Common.Span_Type;
      Header_Done  : Common.Span_Type;
   begin
      D.Conn := Connection_Of (Main_Window);
      Main_Window.Connection_Data (D, Dynamic => True);
      --  The window owns the view data: it is freed with the page.

      for I in Views'Range loop
         if Views (I) = null then
            Views (I) := D;
            exit;
         end if;
      end loop;
      --  Registered before building: the widgets' handlers resolve
      --  their view by connection. A page beyond Max_Views is not
      --  registered, and its widgets would stay inert: say so instead
      --  of failing silently.
      if View_Of (Main_Window) = null then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "todomvc (web): too many open pages, this one is not tracked");
      end if;

      D.Root.Create (Main_Window, "app");

      --  Creation line ---------------------------------------------------
      D.Creation.Create (D.Root, Action => "", ID => "creation");
      --  The submit handler keeps Enter in the input from reloading
      --  the page: it adds the todo, like the Create button.
      D.Creation.On_Submit_Handler (On_New_Todo_Submit'Unrestricted_Access);

      D.New_Todo.Create (D.Creation, Size => 40, Value => "",
                         Name => "new-todo", ID => "new-todo");
      D.New_Todo.Place_Holder (UTF ("What needs to be done?"));
      D.New_Todo.Attribute ("aria-label", UTF ("New task"));
      D.New_Todo.Attribute ("autofocus", UTF ("autofocus"));

      D.Create_Btn.Create (D.Creation, "Create", "create");
      D.Create_Btn.On_Click_Handler (On_Create_Clicked'Unrestricted_Access);

      --  Filters, above the lists, right under the creation line --------
      D.Filters_Box.Create (D.Root, "", "filters");
      for F in Filter_Kind loop
         D.Filters (F).Create (D.Filters_Box, UTF (Filter_Label (F)),
                               UTF (Filter_Id (F)));
         D.Filters (F).Attribute ("aria-pressed", "false");
         D.Filters (F).On_Click_Handler
           (On_Filter_Clicked'Unrestricted_Access);
      end loop;

      --  The two lists, each in its visible display zone ---------------
      D.Slots.Create (D.Root, "", "slots");

      D.Todo_Zone.Create (D.Slots, "", "todo-zone");
      D.Todo_Zone.Add_Class ("zone");
      Header_Todo.Create (D.Todo_Zone, "", "todo-zone-name");
      Header_Todo.Add_Class ("zone-name");
      Header_Todo.Text ("To do");
      D.Todo_List.Create (D.Todo_Zone, "todo-list");
      D.Todo_List.Add_Class ("list");
      D.Todo_List.Attribute ("aria-label", "To do");

      D.Done_Zone.Create (D.Slots, "", "done-zone");
      D.Done_Zone.Add_Class ("zone");
      Header_Done.Create (D.Done_Zone, "", "done-zone-name");
      Header_Done.Add_Class ("zone-name");
      Header_Done.Text ("Done");
      D.Done_List.Create (D.Done_Zone, "done-list");
      D.Done_List.Add_Class ("list");
      D.Done_List.Attribute ("aria-label", "Done");

      --  The selection-scoped controls: unique Edit, Delete and
      --  Complete buttons, disabled while the selection is empty.
      D.Selection.Create (D.Root, "", "selection");

      D.Edit_Btn.Create (D.Selection, "Edit", "edit");
      D.Edit_Btn.Disabled (True);
      D.Edit_Btn.On_Click_Handler (On_Edit_Clicked'Unrestricted_Access);

      D.Delete_Btn.Create (D.Selection, "Delete", "delete");
      D.Delete_Btn.Disabled (True);
      D.Delete_Btn.On_Click_Handler (On_Delete_Clicked'Unrestricted_Access);

      D.Complete_Btn.Create (D.Selection, "Complete", "complete");
      D.Complete_Btn.Disabled (True);
      D.Complete_Btn.On_Click_Handler
        (On_Complete_Clicked'Unrestricted_Access);

      --  The footer, at the bottom: the mark-all and purge controls
      --  together, side by side, on the same line, with the counter.
      D.Footer.Create (D.Root, "", "footer");

      D.Counter.Create (D.Footer, "", "counter");
      D.Counter.Add_Class ("counter");

      D.Mark_All.Create (D.Footer, "Mark all as complete", "mark-all");
      D.Mark_All.On_Click_Handler (On_Mark_All_Clicked'Unrestricted_Access);

      D.Clear_Btn.Create (D.Footer, "Clear completed", "clear-completed");
      D.Clear_Btn.On_Click_Handler (On_Clear_Clicked'Unrestricted_Access);

      Update_View (D);
      D.New_Todo.Focus;

      Connection.Hold;
      --  The page is gone: stop refreshing it, and let the next
      --  connection take its slot.
      D.Live := False;
      for I in Views'Range loop
         if Views (I) = D then
            Views (I) := null;
         end if;
      end loop;
   end On_Connect;

   --  Serving ------------------------------------------------------------------

   function Server_Port return String is
      --  gnoga reads the same switch (and the same default) itself:
      --  the banner below only reports what will be served.
      use Ada.Command_Line;
      Prefix : constant String := "--gnoga-port=";
   begin
      for I in 1 .. Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg'Length > Prefix'Length
              and then Arg
                (Arg'First .. Arg'First + Prefix'Length - 1) = Prefix
            then
               return Ada.Strings.Fixed.Trim
                 (Arg (Arg'First + Prefix'Length .. Arg'Last),
                  Ada.Strings.Both);
            end if;
         end;
      end loop;
      return "8080";
   end Server_Port;

begin
   Load_Todos;

   Gnoga.Application.Title ("ToDoMVC");
   --  The page title: the spec's window name (G005 maps the window to
   --  the page and its document for the Web backend).

   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "todomvc (web): serving on http://127.0.0.1:" & Server_Port);
   --  On the error output: the serving url is a status line, not a
   --  result — a scenario's captured standard output stays clean.

   Gnoga.Application.Multi_Connect.Initialize
     (Event   => On_Connect'Unrestricted_Access,
      Host    => "127.0.0.1",
      Port    => 8080,
      Verbose => False);
   Gnoga.Application.Multi_Connect.Message_Loop;
end TodoMvc;
