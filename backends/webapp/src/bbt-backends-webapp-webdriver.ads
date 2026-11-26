-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The W3C WebDriver side of the web backend: the driver process, the
--  browser session, and the handful of protocol commands the GUI
--  vocabulary needs (navigate, run a script, find an element, click it,
--  send it a key, close the window).
--
--  The vocabulary semantics stay in BBT.Backends.Webapp: this package
--  knows the protocol, not the ARIA vocabulary.

with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

package BBT.Backends.Webapp.WebDriver is

   Connection_Error : exception;
   --  The driver is unreachable, or the browser session is gone (a
   --  closed page): the backend maps it to the window semantics of
   --  G095 rather than to a failure

   Protocol_Error : exception;
   --  The driver answered an unexpected status; the exception message
   --  carries the driver's own diagnosis

   type Options is record
      Driver_Path  : Ada.Strings.Unbounded.Unbounded_String;
      --  The WebDriver driver executable, chromedriver by default
      Browser_Path : Ada.Strings.Unbounded.Unbounded_String;
      --  The browser binary; the first of chromium, chromium-browser,
      --  chrome, google-chrome found in the PATH by default
      Browser_Args : Ada.Strings.Unbounded.Unbounded_String;
      --  Extra browser arguments, space separated
      Headless     : Boolean := True;
      --  Headless by default: a backend run is not an interactive
      --  session
   end record;

   function From_Environment return Options;
   --  The options read from the environment:
   --  BBT_WEBAPP_DRIVER, BBT_WEBAPP_BROWSER, BBT_WEBAPP_BROWSER_ARGS,
   --  and BBT_WEBAPP_HEADLESS (0 shows the browser)

   type Session is tagged limited private;

   procedure Start (S : in out Session; Opt : Options);
   --  Starts the driver on a free loopback port, then opens the
   --  browser session. Raises Protocol_Error when the browser cannot
   --  be started.

   procedure Stop (S : in out Session);
   --  Ends the session and reaps the driver

   function Active (S : Session) return Boolean;

   procedure Navigate (S : in out Session; URL : String);
   function Title (S : in out Session) return String;
   --  The document title: the page is the spec's window (G005)
   function Ready (S : in out Session) return Boolean;
   --  The document finished loading
   procedure Close_Window (S : in out Session);
   --  Closes the page (the spec's window); the application under test
   --  keeps serving, it is not a window of the browser

   function Execute_String
     (S : in out Session; Script : String) return String;
   function Execute_Bool
     (S : in out Session; Script : String) return Boolean;
   function Execute_Number
     (S : in out Session; Script : String) return Natural;
   --  Runs Script as a JavaScript expression, and returns its value

   function Find (S : in out Session; Selector : String) return String;
   --  The driver's reference of the single element matching the CSS
   --  selector, "" when there is none

   procedure Click (S : in out Session; Element : String);
   --  The element's default action (its semantic activation)

   procedure Send_Return (S : in out Session; Element : String);
   --  A synthesized Return key press on the element: the keyboard tier
   --  of G035, the driver being the only tier that reaches the
   --  browser's key handling

private

   type Session is tagged limited record
      Open       : Boolean := False;
      Port       : Natural := 0;
      Id         : Ada.Strings.Unbounded.Unbounded_String;
      Driver_Pid : GNAT.OS_Lib.Process_Id := GNAT.OS_Lib.Invalid_Pid;
   end record;

end BBT.Backends.Webapp.WebDriver;
