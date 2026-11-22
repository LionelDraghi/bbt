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
with BBT_Backends.Polling; use BBT_Backends.Polling;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;
with System;
with GNAT.OS_Lib;

package body BBT.Backends.Gtk4 is

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

   procedure Atspi_Shutdown;
   --  Kills and reaps the currently launched application, if any.
   --  Forward declared for Atspi_Launch, which disposes of the
   --  previous application before spawning a new one: a scenario may
   --  spawn the app several times, and each launch must not leak the
   --  previous process (the runner only calls Shutdown at scenario
   --  boundaries).

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
   Role_List        : constant int := 31;
   Role_List_Item   : constant int := 32;
   State_Sensitive  : constant int := 24;
   State_Focused    : constant int := 12;

   function Atspi_State_Set_Contains
     (Set : System.Address; State : int) return int;
   pragma Import (C, Atspi_State_Set_Contains, "atspi_state_set_contains");

   function Atspi_Action_Do_Action
     (Obj : System.Address; Index : int; Error : System.Address)
     return int;
   pragma Import (C, Atspi_Action_Do_Action, "atspi_action_do_action");

   --  The action list of an object's AT-SPI Action interface, to
   --  resolve an action by name (window.close on frames, cf. G095).
   --  Never a fixed index: the reachable action set depends on the
   --  toolkit version.
   function Atspi_Action_Get_N_Actions
     (Obj : System.Address; Error : System.Address) return int;
   pragma Import (C, Atspi_Action_Get_N_Actions,
                     "atspi_action_get_n_actions");

   function Atspi_Action_Get_Action_Name
     (Obj   : System.Address;
      Index : int;
      Error : System.Address) return System.Address;
   pragma Import (C, Atspi_Action_Get_Action_Name,
                     "atspi_action_get_action_name");

   function Atspi_Accessible_Get_Editable_Text
     (Obj : System.Address; Error : System.Address) return System.Address;
   pragma Import (C, Atspi_Accessible_Get_Editable_Text,
                     "atspi_accessible_get_editable_text");

   --  Set the whole contents of an editable field: the backend side of
   --  the GUI "I type `text` into the `name` textbox" action (G035),
   --  i.e. a semantic value set, not key injection. The returned
   --  interface reference is owned by the caller (unref it).
   function Atspi_Editable_Text_Set_Text_Contents
     (Obj   : System.Address;
      Text  : Interfaces.C.Strings.chars_ptr;
      Error : System.Address) return int;
   pragma Import (C, Atspi_Editable_Text_Set_Text_Contents,
                     "atspi_editable_text_set_text_contents");

   --  The keyboard synthesis of the accessibility stack itself
   --  (atspi_generate_keyboard_event, the virtual keyboards API of
   --  the AT-SPI device event controller): the submission key of an
   --  entry, pressed and released. The synthesis is delegated to
   --  the accessibility daemon, so no windowing call is made here;
   --  on a stack where the daemon cannot inject (e.g. Wayland without
   --  compositor support), the call is accepted but has no effect.
   --  ATSPI_KEY_PRESSRELEASE = 2 (AtspiKeySynthType, verified on
   --  docs.gtk.org/atspi2: PRESS = 0, RELEASE = 1, PRESSRELEASE = 2,
   --  SYM = 3, STRING = 4). An earlier version passed 3 — the SYM
   --  form — which reads the keycode argument as an X keysym: 36 is
   --  the dollar sign, so the daemon synthesized "$" instead of
   --  Return, and no submission ever happened.
   Atspi_Key_Pressrelease : constant int := 2;
   --  The X keycode of Return on the evdev layout, the keycode
   --  argument of the PRESSRELEASE form (the SYM form takes a
   --  keysym — 65293 for Return — but the daemon's on-the-fly keysym
   --  generation has limited reliability per the AT-SPI doc, so the
   --  hardware keycode form is used).
   Return_Keycode        : constant long := 36;

   --  C prototype (docs.gtk.org/atspi2): gboolean
   --  atspi_generate_keyboard_event (glong keyval,
   --  const gchar *keystring, AtspiKeySynthType synth_type,
   --  GError **error). The keystring argument is only read by the
   --  STRING form; keyval is a glong, and the error location is
   --  part of the profile, so it must be passed (NULL here).
   function Atspi_Generate_Keyboard_Event
     (Keycode   : long;
      Keystring : Interfaces.C.Strings.chars_ptr;
      Synth     : int;
      Error     : System.Address) return int;
   pragma Import (C, Atspi_Generate_Keyboard_Event,
                     "atspi_generate_keyboard_event");

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

   --  Depth-first search for a node of role Role. When Name_Text is
   --  empty, any node of the role matches: the caller owns the
   --  returned reference, every visited node is unref'd.
   --
   --  Note: the lookup key is the accessible name, always compared
   --  when provided. An earlier version toked a -1 "no name" flag,
   --  and every named lookup silently degraded to the first node of
   --  the role in tree order — it went unnoticed as long as the
   --  suite widgets were unique per role (two same-role lists
   --  exposed it, cf. G080).

   function Find_Role_Named
     (Obj : System.Address; Role : int; Name_Text : String)
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
              and then (Name_Text = ""
                        or else C_String_Of
                                  (Atspi_Accessible_Get_Name
                                     (Child, System.Null_Address))
                              = Name_Text)
            then
               return Child;
            end if;
            Found := Find_Role_Named (Child, Role, Name_Text);
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

   procedure Atspi_Launch (Binary_Path : String) is
      Binary : GNAT.OS_Lib.String_Access :=
        new String'(Binary_Path);
      Binary_List : constant GNAT.OS_Lib.Argument_List :=
        [Binary];
   begin
      --  A still running application from an earlier run step is
      --  killed first: two applications would compete for the
      --  accessibility bus (and the same todos.state).
      Atspi_Shutdown;

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
   end Atspi_Launch;

   function Atspi_Window_Appears (Expected_Title : String := "") return Boolean is
      function Try return Boolean is
        (Scan_For_Window (Expected_Title));
      function Poll is new Eventually (Try);
   begin
      Observed := Null_Unbounded_String;
      return Poll;
   end Atspi_Window_Appears;

   function Atspi_Observed_Title return String is
   begin
      return To_String (Observed);
   end Atspi_Observed_Title;

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

   function Atspi_Window_Contains (Expected : String) return Boolean is
      function Try return Boolean is
         App : constant System.Address := App_Node;
      begin
         if App = System.Null_Address then
            return False;
         end if;
         --  The window text of the application: every window of
         --  the application in the tree — a dialog opens in its
         --  own window (unlike a DOM dialog, which stays in the
         --  parent document's text).
         declare
            Text : constant String := Gather_Text (App);
         begin
            G_Object_Unref (App);
            Last_Text := To_Unbounded_String (Text);
            return Ada.Strings.Fixed.Index (Text, Expected) /= 0;
         end;
      end Try;
      function Poll is new Eventually (Try);
   begin
      Last_Text := Null_Unbounded_String;
      return Poll;
   end Atspi_Window_Contains;

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
                         (Frame, Role_Push_Button, Name);
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
                         (Frame, Role_Push_Button, Name);
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
                         (Frame, Role_Push_Button, Name);
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
                       (App, Role_Dialog, Expected_Name);
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

   procedure Atspi_Shutdown is
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
   end Atspi_Shutdown;

   --  The index of the action named Name in the AT-SPI Action
   --  interface of Obj, -1 when none is exposed. The search is by
   --  name: the reachable action set depends on the toolkit version
   --  (cf. G095), so no fixed index.
   function Atspi_Action_Named (Obj : System.Address; Name : String)
                                return int
   is
   begin
      for A in 0 .. Atspi_Action_Get_N_Actions
                      (Obj, System.Null_Address) - 1
      loop
         if C_String_Of (Atspi_Action_Get_Action_Name
                           (Obj, A, System.Null_Address)) = Name
         then
            return A;
         end if;
      end loop;
      return -1;
   end Atspi_Action_Named;

   --  The G095 window close: the window.close action of the frame
   --  accessible — graceful, the application's own close path, its
   --  persistence hooks included — then the bounded wait for the
   --  window to leave the accessibility tree, and the process reaped
   --  when the application ended with its window. A stack that does
   --  not expose a close action gets the process-level close (the
   --  spec's fallback): the window is gone either way. The unnamed
   --  form closes the application's only window, and refuses (False)
   --  when the application exposes several: the name disambiguates.

   function Atspi_Close_Window
     (Title : String) return Boolean
   is
      App     : System.Address;
      Frame   : System.Address;
      Windows : int;

      function Gone return Boolean is
         Left : constant System.Address := Window_Of (Title);
      begin
         if Left /= System.Null_Address then
            G_Object_Unref (Left);
            return False;
         end if;
         return True;
      end Gone;
      function Poll is new Eventually (Gone);

   begin
      if Title = "" then
         --  The unnamed form requires a single target window
         App := App_Node;
         if App = System.Null_Address then
            return False;
         end if;
         Windows := Atspi_Accessible_Get_Child_Count (App);
         G_Object_Unref (App);
         if Windows > 1 then
            return False;
         end if;
      end if;

      Frame := Window_Of (Title);
      if Frame = System.Null_Address then
         return False;
      end if;

      declare
         Idx : constant int :=
                 Atspi_Action_Named (Frame, "window.close");
         Done : constant Boolean :=
           Idx >= 0
             and then Atspi_Action_Do_Action
                        (Frame, Idx, System.Null_Address) /= 0;
      begin
         G_Object_Unref (Frame);
         if not Done then
            --  No close action on this stack, or the action was
            --  refused: the process-level close of the spec fallback
            Atspi_Shutdown;
            return True;
         end if;
      end;

      if not Poll then
         return False;
      end if;

      --  Reap the application when it ended with its window; an
      --  application with other windows left open survives the step
      declare
         Left_App : constant System.Address := App_Node;
      begin
         if Left_App = System.Null_Address then
            Atspi_Shutdown;
         else
            G_Object_Unref (Left_App);
         end if;
      end;
      return True;
   end Atspi_Close_Window;


   --  -----------------------------------------------------------------------
   --  The MVP role set: the user visible vocabulary (button, textbox,
   --  dialog) mapped on the AT-SPI role ids. Unknown roles answer -1,
   --  and never match any node in the tree.

   function Atspi_Role_Of (T : String) return int is
   begin
      if    T = "button"  then return Role_Push_Button;
      elsif T = "textbox" then return Role_Text;
      elsif T = "dialog"  then return Role_Dialog;
      elsif T = "list"    then return Role_List;
      elsif T = "listitem" then return Role_List_Item;
      else return -1;
      end if;
   end Atspi_Role_Of;

   --  Generic form of the role based searches, used by the parent
   --  dispatch for the whole MVP widget vocabulary.

   function Atspi_Widget_Named (Role : String; Name : String) return Boolean is
      function Try return Boolean is
         Frame : constant System.Address := Window_Of ("");
         Found : System.Address;
      begin
         if Frame = System.Null_Address then
            return False;
         end if;
         Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), Name);
         G_Object_Unref (Frame);
         if Found /= System.Null_Address then
            G_Object_Unref (Found);
            return True;
         end if;
         return False;
      end Try;
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Atspi_Widget_Named;

   function Atspi_Widget_Enabled
     (Role : String; Name : String) return Boolean
   is
      Widget : System.Address := System.Null_Address;

      function Present return Boolean is
         Frame : constant System.Address := Window_Of ("");
         Found : System.Address;
      begin
         if Frame /= System.Null_Address then
            Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), Name);
            G_Object_Unref (Frame);
            Widget := Found;
         end if;
         return Widget /= System.Null_Address;
      end Present;

      function Enabled return Boolean is
         In_State : constant Boolean :=
           Atspi_State_Set_Contains
             (Atspi_Accessible_Get_State_Set (Widget),
              State_Sensitive) /= 0;
      begin
         G_Object_Unref (Widget);
         Widget := System.Null_Address;
         return In_State;
      end Enabled;

      function Poll is new Eventually_Get (Present   => Present,
                                           Observed  => Enabled);
   begin
      return Poll;
   end Atspi_Widget_Enabled;


   --  Semantic activation of the named widget: the first AT-SPI
   --  action of the first widget of role Role named Name.

   function Atspi_Press_Widget (Role : String; Name : String) return Boolean is
      Widget : System.Address := System.Null_Address;

      function Present return Boolean is
         Frame : constant System.Address := Window_Of ("");
         Found : System.Address;
      begin
         if Frame /= System.Null_Address then
            Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), Name);
            G_Object_Unref (Frame);
            Widget := Found;
         end if;
         return Widget /= System.Null_Address;
      end Present;

      function Pressed return Boolean is
         Done : constant Boolean :=
           Atspi_Action_Do_Action (Widget, 0, System.Null_Address) /= 0;
      begin
         G_Object_Unref (Widget);
         Widget := System.Null_Address;
         return Done;
      end Pressed;

      function Poll is new Eventually_Get (Present   => Present,
                                           Observed  => Pressed);
   begin
      return Poll;
   end Atspi_Press_Widget;

   --  Type the entry text into the named text field: find the field
   --  (role text, the GTK 4 mapping of entries and password entries),
   --  then set its contents through the EditableText interface — a
   --  semantic value set, never an injected key sequence. The field
   --  needs an (empty) text contents set even to clear it.

   function Atspi_Type_Into
     (Role : String; Name : String; Text : String) return Boolean
   is
      pragma Unreferenced (Role);
      --  The search is by accessible name on the role text mapping;
      --  the role word of the step is conformance only (cf. Q011).
      Widget : System.Address := System.Null_Address;

      function Present return Boolean is
         App   : constant System.Address := App_Node;
         Field : System.Address;
      begin
         if App /= System.Null_Address then
            Field := Find_Text_Field_Named (App, Name);
            G_Object_Unref (App);
            Widget := Field;
         end if;
         return Widget /= System.Null_Address;
      end Present;

      function Typed return Boolean is
         Editable : constant System.Address :=
           Atspi_Accessible_Get_Editable_Text (Widget, System.Null_Address);
         C_Text   : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.New_String (Text);
         Done     : constant Boolean :=
           Atspi_Editable_Text_Set_Text_Contents
             (Obj   => Editable,
              Text  => C_Text,
              Error => System.Null_Address) /= 0;
      begin
         Interfaces.C.Strings.Free (C_Text);
         G_Object_Unref (Editable);
         G_Object_Unref (Widget);
         Widget := System.Null_Address;
         return Done;
      end Typed;

      function Poll is new Eventually_Get (Present   => Present,
                                           Observed  => Typed);
   begin
      return Poll;
   end Atspi_Type_Into;

   --  The Enter of "I enter `text` into the `name` textbox" (G035):
   --  the value set of I type, then the submission of the value, as
   --  when a user presses Return in the field. The value set alone
   --  is not a submission, and no accessibility API activates a
   --  GTK 4 entry (no exposed action, GrabFocus refused, a newline
   --  inserted through the editable text interface is filtered out):
   --  the only external path to the entry's Return handling is the
   --  keyboard synthesis of the accessibility stack itself
   --  (atspi_generate_keyboard_event, the virtual keyboards API —
   --  the same call onboard makes). The synthesis is delegated to
   --  the accessibility daemon, so no windowing call is made here;
   --  the daemon injects by its own environment: verified on
   --  at-spi2-core 2.62, the registryd takes the XTest route when
   --  WAYLAND_DISPLAY is unset, and the Wayland route otherwise
   --  (which reaches the compositor's focused window, needs
   --  compositor support, and cannot target the application of the
   --  step: synthesizing from a Wayland session is unreliable).

   function Atspi_Enter_Into
     (Role : String; Name : String; Text : String) return Boolean
   is
      pragma Unreferenced (Role);
      --  The search is by accessible name on the role text mapping;
      --  the role word of the step is conformance only (cf. Q011).
      Widget : System.Address := System.Null_Address;

      function Present return Boolean is
         App   : constant System.Address := App_Node;
         Field : System.Address;
      begin
         if App /= System.Null_Address then
            Field := Find_Text_Field_Named (App, Name);
            G_Object_Unref (App);
            Widget := Field;
         end if;
         return Widget /= System.Null_Address;
      end Present;

      function Entered return Boolean is
         Editable : constant System.Address :=
           Atspi_Accessible_Get_Editable_Text (Widget, System.Null_Address);
         C_Text   : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.New_String (Text);
         Set_Done : constant Boolean :=
           Atspi_Editable_Text_Set_Text_Contents
             (Obj   => Editable,
              Text  => C_Text,
              Error => System.Null_Address) /= 0;
         Key_Done : Boolean;
      begin
         --  Let the value set settle in the field before synthesizing
         --  the Return key: the key must be processed by the entry
         --  after its text changed, not concurrently. The keystring
         --  argument is not read by the PRESSRELEASE form (NULL).
         delay 0.2;
         Key_Done :=
           Atspi_Generate_Keyboard_Event
             (Keycode   => Return_Keycode,
              Keystring => Interfaces.C.Strings.Null_Ptr,
              Synth     => Atspi_Key_Pressrelease,
              Error     => System.Null_Address) /= 0;
         Interfaces.C.Strings.Free (C_Text);
         G_Object_Unref (Editable);
         G_Object_Unref (Widget);
         Widget := System.Null_Address;
         return Set_Done and then Key_Done;
      end Entered;

      function Poll is new Eventually_Get (Present   => Present,
                                           Observed  => Entered);
   begin
      return Poll;
   end Atspi_Enter_Into;

   --  The rendered text content of the named widget subtree: the
   --  same gathering as the window content, restricted to the
   --  widget of role Role named Name (list content checks, G080).

   function Atspi_Widget_Contains
     (Role, Name, Expected : String) return Boolean
   is
      function Try return Boolean is
         Frame : constant System.Address := Window_Of ("");
         Found : System.Address;
      begin
         if Frame = System.Null_Address then
            return False;
         end if;
         Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), Name);
         G_Object_Unref (Frame);
         if Found = System.Null_Address then
            return False;
         end if;
         declare
            Text : constant String := Gather_Text (Found);
         begin
            G_Object_Unref (Found);
            Last_Text := To_Unbounded_String (Text);
            return Ada.Strings.Fixed.Index (Text, Expected) /= 0;
         end;
      end Try;
      function Poll is new Eventually (Try);
   begin
      return Poll;
   end Atspi_Widget_Contains;

   --  The named container widget holds no child (empty list checks,
   --  G080). The existence of the widget is polled first: a list
   --  only known to the builder is not an empty list.

   function Atspi_Widget_Is_Empty (Role, Name : String) return Boolean is
      Widget : System.Address := System.Null_Address;

      function Present return Boolean is
         Frame : constant System.Address := Window_Of ("");
         Found : System.Address;
      begin
         if Frame /= System.Null_Address then
            Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), Name);
            G_Object_Unref (Frame);
            Widget := Found;
         end if;
         return Widget /= System.Null_Address;
      end Present;

      function Empty return Boolean is
         Kids : constant int :=
           Atspi_Accessible_Get_Child_Count (Widget);
      begin
         G_Object_Unref (Widget);
         Widget := System.Null_Address;
         return Kids = 0;
      end Empty;

      function Poll is new Eventually_Get (Present   => Present,
                                           Observed  => Empty);
   begin
      return Poll;
   end Atspi_Widget_Is_Empty;

   function Atspi_Widget_Item_Count
     (Role, Name : String) return Natural
   is
      Widget : System.Address := System.Null_Address;

      function Present return Boolean is
         Frame : constant System.Address := Window_Of ("");
         Found : System.Address;
      begin
         if Frame /= System.Null_Address then
            Found := Find_Role_Named (Frame, Atspi_Role_Of (Role), Name);
            G_Object_Unref (Frame);
            Widget := Found;
         end if;
         return Widget /= System.Null_Address;
      end Present;

      function Item_Count return int is
         Kids : constant int :=
                  Atspi_Accessible_Get_Child_Count (Widget);
      begin
         G_Object_Unref (Widget);
         Widget := System.Null_Address;
         return Kids;
      end Item_Count;

      function Poll is new Eventually (Attempt => Present);
      --  The widget presence is waited for; the count is read once
      --  below, at the same moment as the presence holds
   begin
      if Poll then
         return Natural (Item_Count);
      else
         return 0;
      end if;
   end Atspi_Widget_Item_Count;

   --  -----------------------------------------------------------------------
   --  The dispatching primitives, one per root operation: thin
   --  wrappers on the local subprograms above, dispatched from
   --  BBT.Backends.Selected (set to Registered'Access by
   --  the bbt_gtk4 main).

   procedure Launch
     (The_Backend : in out Gtk4_Backend;
      Binary_Path : String)
   is
      pragma Unreferenced (The_Backend);
   begin
      Atspi_Launch (Binary_Path);
   end Launch;

   procedure Shutdown (The_Backend : in out Gtk4_Backend) is
      pragma Unreferenced (The_Backend);
   begin
      Atspi_Shutdown;
   end Shutdown;

   function Window_Appears
     (The_Backend : Gtk4_Backend; Expected_Title : String := "")
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Window_Appears (Expected_Title);
   end Window_Appears;

   function Observed_Title (The_Backend : Gtk4_Backend) return String is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Observed_Title;
   end Observed_Title;

   function Window_Contains
     (The_Backend : Gtk4_Backend; Expected : String) return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Window_Contains (Expected);
   end Window_Contains;

   function Widget_Named
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Widget_Named (Role, Name);
   end Widget_Named;

   function Widget_Enabled
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Widget_Enabled (Role, Name);
   end Widget_Enabled;

   function Press_Widget
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Press_Widget (Role, Name);
   end Press_Widget;

   function Widget_Contains
     (The_Backend : Gtk4_Backend; Role, Name, Expected : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Widget_Contains (Role, Name, Expected);
   end Widget_Contains;

   function Widget_Is_Empty
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Widget_Is_Empty (Role, Name);
   end Widget_Is_Empty;

   function Widget_Item_Count
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Natural
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Widget_Item_Count (Role, Name);
   end Widget_Item_Count;

   procedure Type_Into
     (The_Backend : in out Gtk4_Backend;
      Role        : String;
      Name        : String;
      Text        : String)
   is
      pragma Unreferenced (The_Backend);
   begin
      if not Atspi_Type_Into (Role, Name, Text) then
         raise No_Backend_Available
           with "cannot type into " & Name
              & " (no editable text field of that name)";
      end if;
   end Type_Into;

   procedure Enter_Into
     (The_Backend : in out Gtk4_Backend;
      Role        : String;
      Name        : String;
      Text        : String)
   is
      pragma Unreferenced (The_Backend);
   begin
      if not Atspi_Enter_Into (Role, Name, Text) then
         raise No_Backend_Available
           with "cannot enter into " & Name
              & " (no editable text field of that name, or the"
              & " accessibility daemon did not accept the Return"
              & " key synthesis, cf. G035)";
      end if;
   end Enter_Into;

   function Close_Window
     (The_Backend : Gtk4_Backend; Title : String := "")
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Atspi_Close_Window (Title);
   end Close_Window;

end BBT.Backends.Gtk4;
