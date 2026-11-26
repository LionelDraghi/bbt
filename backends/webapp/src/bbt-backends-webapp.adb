-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Body of the webapp backend: the vocabulary semantics on top of the
--  WebDriver session (BBT.Backends.Webapp.WebDriver) and of the DOM
--  lookup scripts (BBT.Backends.Webapp.Dom).
--
--  Two things are specific to the web and drive the code below:
--
--  - the page is refreshed by the application's server, asynchronously
--    with respect to the WebDriver call that triggered the change: a
--    single read may catch the previous rendering. The state checks
--    therefore read until two consecutive reads agree, and the
--    existence checks poll within the harness deadline.
--
--  - the page is not a process: the launched application serves it and
--    outlives it. The window close ends the page only (G095), the
--    application being reaped by Shutdown and by the next Launch.

with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with BBT_Backends;
with BBT_Backends.Polling;
with BBT_Backends.Text;

with BBT.Backends.Webapp.Dom;
with BBT.Backends.Webapp.Http;
with BBT.Backends.Webapp.WebDriver;

package body BBT.Backends.Webapp is

   use Ada.Strings.Unbounded;
   use type GNAT.OS_Lib.Process_Id;
   --  Process_Id is private: the invalid-pid comparison needs its
   --  operator directly visible

   Value_Attempts : constant := 4;
   --  A field value set is re-applied this many times at most when the
   --  page does not keep it

   Default_Url : constant String := "http://127.0.0.1:8080";
   --  The url the witness application serves; BBT_WEBAPP_URL overrides
   --  it. The launched application is correlated with its page that
   --  way: the backend waits for that url to answer.

   App_Pid : GNAT.OS_Lib.Process_Id := GNAT.OS_Lib.Invalid_Pid;
   --  The launched application, killed before the next launch

   Driver : WebDriver.Session;
   --  The browser session and the driver process serving it

   App_Url : Unbounded_String := Null_Unbounded_String;
   --  The page the backend drives, as launched

   Observed : Unbounded_String := Null_Unbounded_String;
   --  The last document title read, for the failure diagnostics

   --  Helpers -----------------------------------------------------------------

   function Url_From_Environment return String is
   begin
      if Ada.Environment_Variables.Exists ("BBT_WEBAPP_URL") then
         return Ada.Environment_Variables.Value ("BBT_WEBAPP_URL");
      end if;
      return Default_Url;
   end Url_From_Environment;

   function Url_Port (Url : String) return Natural is
      --  The port of the url: Http.Port_Open is loopback only, the
      --  driver gets the whole url.
      Start : Natural := Url'First;
      Colon : Natural := 0;
      I     : Natural;
   begin
      I := Ada.Strings.Fixed.Index (Url, "://");
      if I /= 0 then
         Start := I + 3;
      end if;
      I := Start;
      while I <= Url'Last and then Url (I) /= '/' loop
         if Url (I) = ':' then
            Colon := I;
         end if;
         I := I + 1;
      end loop;
      if Colon = 0 then
         return 80;
      end if;
      return Natural'Value (Url (Colon + 1 .. I - 1));
   exception
      when Constraint_Error =>
         return 80;
   end Url_Port;

   procedure Wait_Port
     (Port : Natural; Up : Boolean; Reached : out Boolean);
   --  Waits for the page to be served (Up) or for the port to be free
   --  again (not Up), within the harness deadline

   procedure Wait_Port
     (Port : Natural; Up : Boolean; Reached : out Boolean)
   is
      Waited : Duration := 0.0;
   begin
      loop
         if Http.Port_Open (Port) = Up then
            Reached := True;
            return;
         end if;
         exit when Waited >= BBT_Backends.Harness_Deadline;
         delay BBT_Backends.Poll_Interval;
         Waited := Waited + BBT_Backends.Poll_Interval;
      end loop;
      Reached := False;
   end Wait_Port;

   procedure Cleanup;
   --  Kills the launched application and ends the driver session

   procedure Cleanup is
      Status  : GNAT.OS_Lib.Process_Id;
      Success : Boolean;
      Port    : constant Natural :=
                  (if App_Url = Null_Unbounded_String
                   then 0 else Url_Port (To_String (App_Url)));
      Reached : Boolean;
   begin
      if App_Pid /= GNAT.OS_Lib.Invalid_Pid then
         GNAT.OS_Lib.Kill (App_Pid);
         GNAT.OS_Lib.Wait_Process (Status, Success);
         App_Pid := GNAT.OS_Lib.Invalid_Pid;
      end if;
      Driver.Stop;
      if Port /= 0 then
         --  the next launch must find the port free: the application
         --  releases it when it dies.
         Wait_Port (Port, Up => False, Reached => Reached);
      end if;
   end Cleanup;

   function Settled (Script : String) return String is
      --  The script's value, read until two consecutive reads agree:
      --  the page is rendered by the application's server, a single
      --  read may catch the rendering the step meant to change.
      Previous : Unbounded_String := Null_Unbounded_String;
      Current  : Unbounded_String := Null_Unbounded_String;
      Waited   : Duration := 0.0;
      Reads    : Natural := 0;
   begin
      loop
         Current := To_Unbounded_String (Driver.Execute_String (Script));
         exit when Reads > 0 and then Current = Previous;
         exit when Waited >= BBT_Backends.Harness_Deadline;
         Previous := Current;
         Reads := Reads + 1;
         delay BBT_Backends.Poll_Interval;
         Waited := Waited + BBT_Backends.Poll_Interval;
      end loop;
      return To_String (Current);
   end Settled;

   function Mark_Target (Role : String; Name : String) return String is
      --  The driver reference of the element matching the lookup, its
      --  mark set: the way to reach its click and its key handling.
      --  "" when no such element is in the page.
      function Try return Boolean is
      begin
         return Driver.Execute_String (Dom.Mark_Script (Role, Name)) = "found";
      exception
         when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
            return False;
      end Try;
      function Poll is new BBT_Backends.Polling.Eventually (Try);
   begin
      if not Poll then
         return "";
      end if;
      return Driver.Find (Dom.Element_Marker);
   end Mark_Target;

   --  Lifecycle ---------------------------------------------------------------

   overriding procedure Launch
     (The_Backend : in out Webapp_Backend;
      Binary_Path : String)
   is
      pragma Unreferenced (The_Backend);
      No_Args : GNAT.OS_Lib.Argument_List (1 .. 0);
      Reached : Boolean;
      Options : WebDriver.Options;
   begin
      --  An application left by an earlier run step is killed first:
      --  two of them would compete for the same url (and the same
      --  todos.state).
      Cleanup;
      Observed := Null_Unbounded_String;
      App_Url := To_Unbounded_String (Url_From_Environment);

      --  The url must be free before our own application takes it: a
      --  server left behind by another harness would answer instead,
      --  and every step would drive the wrong application — silently,
      --  since the page it serves is a valid page.
      Wait_Port (Url_Port (To_String (App_Url)), Up => False,
                 Reached => Reached);
      if not Reached then
         raise No_Backend_Available
           with "the url " & To_String (App_Url)
             & " is already served: stop the application holding it,"
             & " or point BBT_WEBAPP_URL elsewhere";
      end if;

      App_Pid := GNAT.OS_Lib.Non_Blocking_Spawn (Binary_Path, No_Args);
      if App_Pid = GNAT.OS_Lib.Invalid_Pid then
         raise No_Backend_Available with "cannot launch " & Binary_Path;
      end if;

      Wait_Port (Url_Port (To_String (App_Url)), Up => True,
                 Reached => Reached);
      if not Reached then
         Cleanup;
         raise No_Backend_Available
           with "the application did not serve "
             & To_String (App_Url)
             & " within" & BBT_Backends.Harness_Deadline'Image & " s";
      end if;

      Options := WebDriver.From_Environment;
      begin
         Driver.Start (Options);
         Driver.Navigate (To_String (App_Url));
      exception
         when E : WebDriver.Connection_Error | WebDriver.Protocol_Error
                 | Http.Connection_Error =>
            Cleanup;
            raise No_Backend_Available
              with Ada.Exceptions.Exception_Message (E);
      end;
   end Launch;

   overriding procedure Shutdown (The_Backend : in out Webapp_Backend) is
      pragma Unreferenced (The_Backend);
   begin
      Cleanup;
      App_Url := Null_Unbounded_String;
      Observed := Null_Unbounded_String;
   end Shutdown;

   --  The window (the page) ---------------------------------------------------

   overriding function Window_Appears
     (The_Backend : Webapp_Backend; Expected_Title : String := "")
      return Boolean
   is
      pragma Unreferenced (The_Backend);
      Expected : constant String :=
                   BBT_Backends.Text.Normalized (Expected_Title);
      function Try return Boolean is
      begin
         if not Driver.Active or else not Driver.Ready then
            return False;
         end if;
         Observed := To_Unbounded_String (Driver.Title);
         return Expected = ""
           or else BBT_Backends.Text.Normalized (To_String (Observed))
                   = Expected;
      exception
         when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
            return False;
      end Try;
      function Poll is new BBT_Backends.Polling.Eventually (Try);
   begin
      Observed := Null_Unbounded_String;
      return Poll;
   end Window_Appears;

   overriding function Observed_Title
     (The_Backend : Webapp_Backend) return String
   is
      pragma Unreferenced (The_Backend);
   begin
      return To_String (Observed);
   end Observed_Title;

   overriding function Window_Contains
     (The_Backend : Webapp_Backend; Expected : String) return Boolean
   is
      pragma Unreferenced (The_Backend);
      function Try return Boolean is
      begin
         return BBT_Backends.Text.Contains
                  (Driver.Execute_String (Dom.Window_Text_Script), Expected);
      exception
         when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
            return False;
      end Try;
      function Poll is new BBT_Backends.Polling.Eventually (Try);
   begin
      return Poll;
   end Window_Contains;

   --  Widgets -----------------------------------------------------------------

   overriding function Widget_Named
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
      function Try return Boolean is
      begin
         return Driver.Execute_String (Dom.Exists_Script (Role, Name)) = "true";
      exception
         when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
            return False;
      end Try;
      function Poll is new BBT_Backends.Polling.Eventually (Try);
   begin
      return Poll;
   end Widget_Named;

   overriding function Widget_Enabled
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Settled (Dom.Enabled_Script (Role, Name)) = "true";
   exception
      when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
         return False;
   end Widget_Enabled;

   overriding function Press_Widget
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
      Element : constant String := Mark_Target (Role, Name);
   begin
      if Element = "" then
         return False;
      end if;
      Driver.Click (Element);
      return True;
   exception
      when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
         return False;
   end Press_Widget;

   overriding function Widget_Contains
     (The_Backend : Webapp_Backend; Role, Name, Expected : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return BBT_Backends.Text.Contains
               (Settled (Dom.Text_Script (Role, Name)), Expected);
   exception
      when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
         return False;
   end Widget_Contains;

   overriding function Widget_Is_Empty
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean
   is
      pragma Unreferenced (The_Backend);
   begin
      return Settled (Dom.Child_Count_Script (Role, Name)) = "0";
   exception
      when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
         return False;
   end Widget_Is_Empty;

   overriding function Widget_Item_Count
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Natural
   is
      pragma Unreferenced (The_Backend);
      Value : constant String := Settled (Dom.Child_Count_Script (Role, Name));
   begin
      if Value = "" or else Value = "-1" then
         --  no such container in the page
         return 0;
      end if;
      return Natural'Value (Value);
   exception
      when Constraint_Error =>
         return 0;
      when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
         return 0;
   end Widget_Item_Count;

   overriding procedure Type_Into
     (The_Backend : in out Webapp_Backend;
      Role        : String;
      Name        : String;
      Text        : String)
   is
      pragma Unreferenced (The_Backend);
      Set_Ok : Boolean := False;
   begin
      --  The set value is confirmed once the page has settled, and
      --  re-applied when it did not stay: an application may clear its
      --  field with a queued command, processed by the browser after
      --  the set of the next step, and the text the commit is about to
      --  read would be gone. No harness can assume otherwise: what a
      --  field holds belongs to the application.
      for Attempt in 1 .. Value_Attempts loop
         if Mark_Target (Role, Name) = "" then
            raise No_Backend_Available
              with "no " & Role & " named """ & Name & """ in the page";
         end if;
         Set_Ok :=
           Driver.Execute_String (Dom.Set_Value_Script (Role, Name, Text))
           = "true";
         exit when not Set_Ok;
         delay BBT_Backends.Poll_Interval;
         exit when Driver.Execute_String (Dom.Value_Script (Role, Name)) = Text;
      end loop;
      if not Set_Ok then
         raise No_Backend_Available
           with "cannot set the value of the " & Role & " """ & Name & """";
      end if;
   exception
      when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
         raise No_Backend_Available
           with "cannot type into the " & Role & " """ & Name & """";
   end Type_Into;

   overriding procedure Enter_Into
     (The_Backend : in out Webapp_Backend;
      Role        : String;
      Name        : String;
      Text        : String)
   is
      Element : Unbounded_String;
   begin
      Type_Into (The_Backend, Role, Name, Text);
      --  The marked element is still there with its value set; the
      --  Return goes through the driver's key handling, the only tier
      --  that reaches the browser's own.
      Element := To_Unbounded_String (Mark_Target (Role, Name));
      if Element = Null_Unbounded_String then
         raise No_Backend_Available
           with "cannot commit the " & Role & " """ & Name & """";
      end if;
      Driver.Send_Return (To_String (Element));
   exception
      when WebDriver.Connection_Error | WebDriver.Protocol_Error =>
         raise No_Backend_Available
           with "cannot commit the " & Role & " """ & Name & """";
   end Enter_Into;

   overriding function Close_Window
     (The_Backend : Webapp_Backend; Title : String := "")
      return Boolean
   is
      pragma Unreferenced (The_Backend);
      pragma Unreferenced (Title);
   begin
      if not Driver.Active then
         return False;
      end if;
      Driver.Close_Window;
      Observed := Null_Unbounded_String;
      return True;
   exception
      when WebDriver.Connection_Error =>
         --  the page is already gone: the window semantics (G095)
         return True;
      when WebDriver.Protocol_Error =>
         return False;
   end Close_Window;

end BBT.Backends.Webapp;
