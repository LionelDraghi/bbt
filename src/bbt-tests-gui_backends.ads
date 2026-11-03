-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

--  GUI backend root.
--
--  This is the platform independent face of the GUI execution layer:
--  the runner calls the operations below, and the body of this very
--  package dispatches on the selected backend (--backend=atspi on
--  Linux; No_Backend elsewhere, or when no backend was requested).
--
--  The MVP semantics follow GuiVerb's AT-SPI reference: a backend
--  operation polls the accessibility tree within a bounded deadline
--  and answers a Boolean; the backend owns the accessibility
--  environment setup of the launched applications (GTK_A11Y...).

package BBT.Tests.GUI_Backends is

   No_Backend_Available : exception;

   type Backend_Kind is (No_Backend, Atspi);

   --  The backend selected on the command line (--backend), No_Backend
   --  when no --backend option was given.
   Selected : Backend_Kind := No_Backend;

   --  A launched application is identified by its process id, which
   --  is also the correlation key used to find its windows on the
   --  accessibility bus.
   procedure Launch (Binary_Path : String);
   --  Spawn the application binary, and remember its pid as the
   --  correlation target. Raises No_Backend_Available when no backend
   --  was selected, or launch failure diagnostics terminate the
   --  launch attempt.

   procedure Kill_Last;
   --  Kill and reap the application launched by Launch, if any.

   function Window_Appears (Expected_Title : String := "") return Boolean;
   --  Poll the accessibility bus for a top-level window of the
   --  application identified by the last Launch, within the backend
   --  bounded deadline. Empty Expected_Title accepts the first window
   --  found.

   function Observed_Title return String;
   --  The title of the last window matched by Window_Appears, for the
   --  failure diagnostic; empty when no window matched.

   function Window_Contains (Expected : String) return Boolean;
   --  The rendered text content of the application's windows: the
   --  accessible names and Text interface content of the whole
   --  subtree, dialogs included.

   function Dialog_Appears (Expected_Name : String := "") return Boolean;
   --  Poll for an AT-SPI dialog of the launched application, exact
   --  accessible-name match when Expected_Name is not empty.

   function Widget_Named
     (Role : String; Name : String) return Boolean;
   --  A widget of AT-SPI role Role and accessible name Name exists in
   --  the launched application (MVP roles: button, textbox, dialog).

   function Widget_Enabled
     (Role : String; Name : String) return Boolean;
   --  The user-visible enabled state of the named widget (STATE_
   --  SENSITIVE on GTK 4, which never emits STATE_ENABLED).

end BBT.Tests.GUI_Backends;
