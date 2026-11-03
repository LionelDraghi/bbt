-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

--  AT-SPI backend (Linux): the real child of BBT.Tests.GUI_Backends.
--  Compiled only on Linux; on the other platforms the same child unit
--  is provided by src/no_gui as a no-op stub, named identically
--  (BBT.Tests.GUI_Backends.Atspi), so that the parent body always
--  compiles and dispatches at run time on Settings.Selected.

package body BBT.Tests.GUI_Backends.Atspi is

   --  libatspi direct imports, no dev headers required: exactly the
   --  calls a canonical tree walk needs.

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

   --  The application's window frame: a window of the process the
   --  backend launched (correlation by pid), optionally title-matched.

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
              and then Target_Pid > 0
              and then Integer
                         (Atspi_Accessible_Get_Process_Id
                            (App, System.Null_Address)) = Target_Pid
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

   --  The steps below poll the accessibility tree within the harness
   --  deadline: the application window appears, then its subtree
   --  builds, and a previous step may still be propagating.

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

end BBT.Tests.GUI_Backends.Atspi;
