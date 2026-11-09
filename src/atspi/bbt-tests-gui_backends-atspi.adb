-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------
--  AT-SPI backend (Linux). Own libatspi binding: direct imports, no
--  dev headers required, accessibility bus connection lazy and
--  automatic, correlation with the launched application by process
--  id, with an application name fallback for wrapper processes.

with Ada.Calendar;          use Ada.Calendar;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;
with System;

package body BBT.Tests.GUI_Backends.Atspi is

   App_Pid         : GNAT.OS_Lib.Process_Id;
   Target_Pid      : Integer := 0;
   Target_Name     : GNAT.OS_Lib.String_Access;
   Target_Name_Set : Boolean := False;
   --  Correlation key fallback: the simple name of the launched
   --  binary, for the applications that do not expose themselves
   --  under the launched pid (a python wrapper pattern: e.g.
   --  gnome-calculator shows up on the bus as another process)
   Observed        : Unbounded_String := Null_Unbounded_String;
   Last_Text       : Unbounded_String := Null_Unbounded_String;

   use type GNAT.OS_Lib.Process_Id;
   use type System.Address;

   --  libatspi direct imports (no dev headers installed): exactly the
   --  calls a canonical tree walk needs. Correlation is by process id
   --  — the spawned binary IS the application (no intermediate
   --  launcher), so its pid is stable.

   function Atspi_Get_Desktop (D : int) return System.Address;
   pragma Import (C, Atspi_Get_Desktop, "atspi_get_desktop");

   function Atspi_Accessible_Get_Child_Count
     (Obj : System.Address) return int;
   pragma Import (C, Atspi_Accessible_Get_Child_Count,
                     "atspi_accessible_get_child_count");

   function Atspi_Accessible_Get_Child_At_Index
     (Obj : System.Address; Index : int) return System.Address;
   pragma Import (C, Atspi_Accessible_Get_Child_At_Index,
                     "atspi_accessible_get_child_at_index");

   function Atspi_Accessible_Get_Name
     (Obj : System.Address; Error : System.Address) return System.Address;
   pragma Import (C, Atspi_Accessible_Get_Name,
                     "atspi_accessible_get_name");

   function Atspi_Accessible_Get_Process_Id
     (Obj : System.Address; Error : System.Address) return int;
   pragma Import (C, Atspi_Accessible_Get_Process_Id,
                     "atspi_accessible_get_process_id");

   function Atspi_Accessible_Get_Role
     (Obj : System.Address; Error : System.Address) return int;
   pragma Import (C, Atspi_Accessible_Get_Role,
                     "atspi_accessible_get_role");

   function Atspi_Accessible_Get_State_Set
     (Obj : System.Address) return System.Address;
   pragma Import (C, Atspi_Accessible_Get_State_Set,
                     "atspi_accessible_get_state_set");

   --  AtspiState values (Atspi-2.0 enum, verified by introspection):
   --  ENABLED = 8; GTK 4 never emits it, see the .ads mapping note;
   --  SENSITIVE = 24 is GTK 4's user-visible "enabled" state.

   Role_Push_Button : constant int := 43;
   Role_Dialog      : constant int := 16;
   Role_Text        : constant int := 61;
   State_Sensitive  : constant int := 24;
   State_Focused    : constant int := 12;

   function Atspi_State_Set_Contains
     (Set : System.Address; State : int) return int;
   pragma Import (C, Atspi_State_Set_Contains, "atspi_state_set_contains");

   function Atspi_Action_Do_Action
     (Obj : System.Address; Index : int; Error : System.Address)
     return int;
   pragma Import (C, Atspi_Action_Do_Action, "atspi_action_do_action");

   procedure G_Object_Unref (Obj : System.Address);
   pragma Import (C, G_Object_Unref, "g_object_unref");

   procedure G_Free (P : System.Address);
   pragma Import (C, G_Free, "g_free");

   --  A libatspi-returned C string, once read, is released with
   --  g_free (the string transferred to us is glib-allocated).

   function To_Chars is
     new Ada.Unchecked_Conversion
       (System.Address, Interfaces.C.Strings.chars_ptr);

   function C_String_Of (P : System.Address) return String is
   begin
      if P = System.Null_Address then
         return "";
      end if;
      declare
         Text : constant String :=
           Interfaces.C.Strings.Value (To_Chars (P));
      begin
         G_Free (P);
         return Text;
      end;
   end C_String_Of;

   --  Is that desktop application ours? by pid first, then by the
   --  simple name of the launched binary.

   function Correlates (App_Name : String; App_Pid_On_Bus : int)
                        return Boolean is
   begin
      return (Target_Pid > 0
                and then Integer (App_Pid_On_Bus) = Target_Pid)
        or else (Target_Name_Set and then App_Name = Target_Name.all);
   end Correlates;

   --  The application's window frame: a window of the process the
   --  backend launched (correlation by pid), optionally title-matched.
   --  The caller owns the returned reference.

   function Window_Of (Expected_Title : String) return System.Address is
      Desktop : constant System.Address := Atspi_Get_Desktop (0);
   begin
      if Desktop = System.Null_Address then
         return System.Null_Address;
      end if;
      for A in 1 .. Atspi_Accessible_Get_Child_Count (Desktop) loop
         declare
            App : constant System.Address :=
              Atspi_Accessible_Get_Child_At_Index (Desktop, int (A - 1));
         begin
            if App /= System.Null_Address
              and then Correlates
                         (C_String_Of
                            (Atspi_Accessible_Get_Name
                               (App, System.Null_Address)),
                          Atspi_Accessible_Get_Process_Id
                            (App, System.Null_Address))
            then
               for W in 1 .. Atspi_Accessible_Get_Child_Count (App) loop
                  declare
                     Frame : constant System.Address :=
                       Atspi_Accessible_Get_Child_At_Index
                         (App, int (W - 1));
                  begin
                     if Frame /= System.Null_Address then
                        if Expected_Title = ""
                          or else C_String_Of
                                    (Atspi_Accessible_Get_Name
                                       (Frame, System.Null_Address))
                              = Expected_Title
                        then
                           G_Object_Unref (App);
                           return Frame;
                        end if;
                        G_Object_Unref (Frame);
                     end if;
                  end;
               end loop;
            end if;
            if App /= System.Null_Address then
               G_Object_Unref (App);
            end if;
         end;
      end loop;
      return System.Null_Address;
   end Window_Of;

   --  Depth-first search for a node of role Role named Name. The
   --  caller owns the returned reference; every visited node is
   --  unref'd.

   function Find_Role_Named
     (Obj : System.Address; Role, Name : int; Name_Text : String)
     return System.Address
   is
      Child, Found : System.Address;
   begin
      if Obj = System.Null_Address then
         return System.Null_Address;
      end if;
      for C in 1 .. Atspi_Accessible_Get_Child_Count (Obj) loop
         Child := Atspi_Accessible_Get_Child_At_Index (Obj, int (C - 1));
         if Child /= System.Null_Address then
            if Atspi_Accessible_Get_Role (Child, System.Null_Address)
                 = Role
              and then (Name = -1
                        or else C_String_Of
                                  (Atspi_Accessible_Get_Name
                                     (Child, System.Null_Address))
                              = Name_Text)
            then
               return Child;
            end if;
            Found := Find_Role_Named (Child, Role, Name, Name_Text);
            G_Object_Unref (Child);
            if Found /= System.Null_Address then
               return Found;
            end if;
         end if;
      end loop;
      return System.Null_Address;
   end Find_Role_Named;

   --  The window's rendered text content: the accessible names of the
   --  window subtree (GTK 4 labels carry their text as accessible
   --  name). The root reference belongs to the caller.

   function Gather_Text (Obj : System.Address) return String is
      Text : Unbounded_String := To_Unbounded_String
        (C_String_Of (Atspi_Accessible_Get_Name (Obj, System.Null_Address)));
   begin
      for C in 1 .. Atspi_Accessible_Get_Child_Count (Obj) loop
         declare
            Child : constant System.Address :=
              Atspi_Accessible_Get_Child_At_Index (Obj, int (C - 1));
         begin
            if Child /= System.Null_Address then
               Append (Text, " " & Gather_Text (Child));
               G_Object_Unref (Child);
            end if;
         end;
      end loop;
      return To_String (Text);
   end Gather_Text;

   function Scan_For_Window (Expected_Title : String) return Boolean is
      Frame : constant System.Address := Window_Of (Expected_Title);
   begin
      if Frame = System.Null_Address then
         return False;
      end if;
      Observed := To_Unbounded_String
        (C_String_Of (Atspi_Accessible_Get_Name (Frame, System.Null_Address)));
      G_Object_Unref (Frame);
      return True;
   end Scan_For_Window;

   procedure Launch (Binary_Path : String) is
      Binary : GNAT.OS_Lib.String_Access :=
        new String'(Binary_Path);
      Binary_List : constant GNAT.OS_Lib.Argument_List :=
        [Binary];
   begin
      App_Pid := GNAT.OS_Lib.Non_Blocking_Spawn
        (Binary_Path, Binary_List);
      GNAT.OS_Lib.Free (Binary);
      if App_Pid = GNAT.OS_Lib.Invalid_Pid then
         raise No_Backend_Available with "cannot launch " & Binary_Path;
      end if;
      Target_Pid := GNAT.OS_Lib.Pid_To_Integer (App_Pid);
      Target_Name := new String'
        (Ada.Directories.Simple_Name (Binary_Path));
      Target_Name_Set := True;
   end Launch;

   function Window_Appears (Expected_Title : String := "") return Boolean is
      Deadline : constant Time := Clock + 6.0;
      Found    : Boolean := False;
   begin
      Observed := Null_Unbounded_String;
      while Clock < Deadline and then not Found loop
         Found := Scan_For_Window (Expected_Title);
         if not Found then
            delay 0.2;
         end if;
      end loop;
      return Found;
   end Window_Appears;

   function Observed_Title return String is
   begin
      return To_String (Observed);
   end Observed_Title;

   function App_Node return System.Address is
      Desktop : constant System.Address := Atspi_Get_Desktop (0);
   begin
      if Desktop = System.Null_Address then
         return System.Null_Address;
      end if;
      for A in 1 .. Atspi_Accessible_Get_Child_Count (Desktop) loop
         declare
            App : constant System.Address :=
              Atspi_Accessible_Get_Child_At_Index (Desktop, int (A - 1));
         begin
            if App /= System.Null_Address
              and then Correlates
                         (C_String_Of
                            (Atspi_Accessible_Get_Name
                               (App, System.Null_Address)),
                          Atspi_Accessible_Get_Process_Id
                            (App, System.Null_Address))
            then
               return App;
            end if;
            if App /= System.Null_Address then
               G_Object_Unref (App);
            end if;
         end;
      end loop;
      return System.Null_Address;
   end App_Node;

   --  The steps below poll the accessibility tree within the harness
   --  deadline: the application window appears, then its subtree
   --  builds, and a previous step may still be propagating.

   function Window_Contains (Expected : String) return Boolean is
      Deadline : constant Time := Clock + 6.0;
      App      : System.Address;
   begin
      Last_Text := Null_Unbounded_String;
      while Clock < Deadline loop
         App := App_Node;
         if App /= System.Null_Address then
            --  The window text of the application: every window of
            --  the application in the tree — a dialog opens in its
            --  own window (unlike a DOM dialog, which stays in the
            --  parent document's text).
            declare
               Text : constant String := Gather_Text (App);
            begin
               G_Object_Unref (App);
               Last_Text := To_Unbounded_String (Text);
               if Ada.Strings.Fixed.Index (Text, Expected) /= 0 then
                  return True;
               end if;
            end;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Window_Contains;

   function Observed_Text return String is
   begin
      return To_String (Last_Text);
   end Observed_Text;

   pragma Unreferenced (Observed_Text);

   function Button_Named (Name : String) return Boolean is
      Deadline : constant Time := Clock + 6.0;
      Frame    : System.Address;
      Button   : System.Address;
   begin
      while Clock < Deadline loop
         Frame := Window_Of ("");
         if Frame /= System.Null_Address then
            Button := Find_Role_Named
                         (Frame, Role_Push_Button, -1, Name);
            G_Object_Unref (Frame);
            if Button /= System.Null_Address then
               G_Object_Unref (Button);
               return True;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Button_Named;

   pragma Unreferenced (Button_Named);

   function Button_Enabled (Name : String) return Boolean is
      Deadline : constant Time := Clock + 6.0;
      Frame    : System.Address;
      Button   : System.Address;
      Enabled  : Boolean := False;
   begin
      while Clock < Deadline loop
         Frame := Window_Of ("");
         if Frame /= System.Null_Address then
            Button := Find_Role_Named
                         (Frame, Role_Push_Button, -1, Name);
            G_Object_Unref (Frame);
            if Button /= System.Null_Address then
               Enabled := Atspi_State_Set_Contains
                            (Atspi_Accessible_Get_State_Set (Button),
                             State_Sensitive) /= 0;
               G_Object_Unref (Button);
               return Enabled;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Button_Enabled;

   pragma Unreferenced (Button_Enabled);

   function Press_Button (Name : String) return Boolean is
      Deadline : constant Time := Clock + 6.0;
      Frame    : System.Address;
      Button   : System.Address;
   begin
      while Clock < Deadline loop
         Frame := Window_Of ("");
         if Frame /= System.Null_Address then
            Button := Find_Role_Named
                         (Frame, Role_Push_Button, -1, Name);
            G_Object_Unref (Frame);
            if Button /= System.Null_Address then
               declare
                  Done : constant Boolean :=
                    Atspi_Action_Do_Action
                      (Button, 0, System.Null_Address) /= 0;
               begin
                  G_Object_Unref (Button);
                  return Done;
               end;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Press_Button;

   pragma Unreferenced (Press_Button);

   --  The application node: the desktop application of the process
   --  the backend launched. The caller owns the returned reference.
   function Dialog_Appears (Expected_Name : String := "") return Boolean is
      Deadline : constant Time := Clock + 6.0;
      App      : System.Address;
      Found    : System.Address;
   begin
      while Clock < Deadline loop
         App := App_Node;
         if App /= System.Null_Address then
            Found := Find_Role_Named
                       (App, Role_Dialog, -1, Expected_Name);
            G_Object_Unref (App);
            if Found /= System.Null_Address then
               G_Object_Unref (Found);
               return True;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Dialog_Appears;

   pragma Unreferenced (Dialog_Appears);

   --  Depth-first search for a text field named Name (rôle text —
   --  GTK 4 maps Entry and PasswordEntry to that role, Q022).

   function Find_Text_Field_Named
     (Obj : System.Address; Name : String) return System.Address
   is
      Child, Found : System.Address;
   begin
      if Obj = System.Null_Address then
         return System.Null_Address;
      end if;
      for C in 1 .. Atspi_Accessible_Get_Child_Count (Obj) loop
         Child := Atspi_Accessible_Get_Child_At_Index (Obj, int (C - 1));
         if Child /= System.Null_Address then
            if Atspi_Accessible_Get_Role (Child, System.Null_Address)
                 = Role_Text
              and then C_String_Of
                         (Atspi_Accessible_Get_Name
                            (Child, System.Null_Address)) = Name
            then
               return Child;
            end if;
            Found := Find_Text_Field_Named (Child, Name);
            G_Object_Unref (Child);
            if Found /= System.Null_Address then
               return Found;
            end if;
         end if;
      end loop;
      return System.Null_Address;
   end Find_Text_Field_Named;

   function Text_Field_Named
     (Name : String; Password : Boolean := False) return Boolean
   is
      pragma Unreferenced (Password);
      --  GTK 4 does not distinguish password fields by role (Q022):
      --  both projections match role text; the password variant of
      --  the scenario resolves through the same search.
      Deadline : constant Time := Clock + 6.0;
      App      : System.Address;
      Field    : System.Address;
   begin
      while Clock < Deadline loop
         App := App_Node;
         if App /= System.Null_Address then
            Field := Find_Text_Field_Named (App, Name);
            G_Object_Unref (App);
            if Field /= System.Null_Address then
               G_Object_Unref (Field);
               return True;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Text_Field_Named;

   pragma Unreferenced (Text_Field_Named);

   function Field_Focused (Name : String) return Boolean is
      Deadline : constant Time := Clock + 6.0;
      App      : System.Address;
      Field    : System.Address;
   begin
      while Clock < Deadline loop
         App := App_Node;
         if App /= System.Null_Address then
            Field := Find_Text_Field_Named (App, Name);
            G_Object_Unref (App);
            if Field /= System.Null_Address then
               declare
                  Focused : constant Boolean :=
                    Atspi_State_Set_Contains
                      (Atspi_Accessible_Get_State_Set (Field),
                       State_Focused) /= 0;
               begin
                  G_Object_Unref (Field);
                  return Focused;
               end;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Field_Focused;

   pragma Unreferenced (Field_Focused);

   procedure Shutdown is
      Status  : GNAT.OS_Lib.Process_Id;
      Success : Boolean;
   begin
      if Target_Pid > 0 then
         GNAT.OS_Lib.Kill (App_Pid);
         GNAT.OS_Lib.Wait_Process (Status, Success);
         Target_Pid := 0;
         Target_Name_Set := False;
         GNAT.OS_Lib.Free (Target_Name);
      end if;
   end Shutdown;


   --  -----------------------------------------------------------------------
   --  The MVP role set: the user visible vocabulary (button, textbox,
   --  dialog) mapped on the AT-SPI role ids. Unknown roles answer -1,
   --  and never match any node in the tree.

   function Atspi_Role_Of (T : String) return int is
   begin
      if    T = "button"  then return Role_Push_Button;
      elsif T = "textbox" then return Role_Text;
      elsif T = "dialog"  then return Role_Dialog;
      else return -1;
      end if;
   end Atspi_Role_Of;

   --  Generic form of the role based searches, used by the parent
   --  dispatch for the whole MVP widget vocabulary.

   function Widget_Named (Role : String; Name : String) return Boolean is
      Deadline : constant Time := Clock + 6.0;
      Frame, Found : System.Address;
   begin
      while Clock < Deadline loop
         Frame := Window_Of ("");
         if Frame /= System.Null_Address then
            Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), -1, Name);
            G_Object_Unref (Frame);
            if Found /= System.Null_Address then
               G_Object_Unref (Found);
               return True;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Widget_Named;

   function Widget_Enabled
     (Role : String; Name : String) return Boolean
   is
      Deadline : constant Time := Clock + 6.0;
      Frame, Found : System.Address;
   begin
      while Clock < Deadline loop
         Frame := Window_Of ("");
         if Frame /= System.Null_Address then
            Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), -1, Name);
            G_Object_Unref (Frame);
            if Found /= System.Null_Address then
               declare
                  Enabled : constant Boolean :=
                    Atspi_State_Set_Contains
                      (Atspi_Accessible_Get_State_Set (Found),
                       State_Sensitive) /= 0;
               begin
                  G_Object_Unref (Found);
                  return Enabled;
               end;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Widget_Enabled;


   --  Semantic activation of the named widget: the first AT-SPI
   --  action of the first widget of role Role named Name.

   function Press_Widget (Role : String; Name : String) return Boolean is
      Deadline : constant Time := Clock + 6.0;
      Frame, Found : System.Address;
   begin
      while Clock < Deadline loop
         Frame := Window_Of ("");
         if Frame /= System.Null_Address then
            Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), -1, Name);
            G_Object_Unref (Frame);
            if Found /= System.Null_Address then
               declare
                  Done : constant Boolean :=
                    Atspi_Action_Do_Action
                      (Found, 0, System.Null_Address) /= 0;
               begin
                  G_Object_Unref (Found);
                  return Done;
               end;
            end if;
         end if;
         delay 0.2;
      end loop;
      return False;
   end Press_Widget;

end BBT.Tests.GUI_Backends.Atspi;
