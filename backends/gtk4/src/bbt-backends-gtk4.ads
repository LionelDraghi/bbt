-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  GTK 4 GUI backend (Linux): the AT-SPI accessibility backend,
--  packaged as its own bbt backend crate
--  (cf. docs/proposed/crate_restructuring.md). Own libatspi binding:
--  direct imports, no dev headers required, accessibility bus
--  connection lazy and automatic, correlation with the launched
--  application by process id, with an application name fallback for
--  wrapper processes.

package BBT.Backends.Gtk4 is

   Backend_Kind : constant String := "gtk4";

   type Gtk4_Backend is new Backend with null record;
   --  The concrete backend registered by the bbt_gtk4 main. The
   --  backend state (pid correlation, observed titles, last gathered
   --  text) lives in the body: the backend operates as one instance,
   --  registered once at startup.

   overriding procedure Launch
     (The_Backend : in out Gtk4_Backend;
      Binary_Path : String);
   --  Spawns the application binary, records its pid as the
   --  correlation target for the accessibility tree walk

   overriding procedure Shutdown (The_Backend : in out Gtk4_Backend);
   --  Kills and reaps the last launched application, if any

   overriding function Window_Appears
     (The_Backend : Gtk4_Backend; Expected_Title : String := "")
      return Boolean;
   --  Polls the accessibility bus for a top-level window of the
   --  launched application, within the bounded harness deadline

   overriding function Observed_Title
     (The_Backend : Gtk4_Backend) return String;
   --  The title of the last window matched by Window_Appears, for
   --  the failure diagnostic

   overriding function Window_Contains
     (The_Backend : Gtk4_Backend; Expected : String) return Boolean;
   --  The rendered text content of the application windows: the
   --  accessible names and Text interface content of the whole
   --  subtree, dialogs included

   overriding function Widget_Named
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean;
   --  A widget of role Role and accessible name Name exists in the
   --  launched application

   overriding function Widget_Enabled
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean;
   --  The user-visible enabled state of the named widget (STATE_
   --  SENSITIVE on GTK 4, which never emits STATE_ENABLED)

   overriding function Press_Widget
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean;
   --  Semantic activation of the named widget (the first AT-SPI
   --  action, not injected input)

   Registered : aliased Gtk4_Backend;
   --  The singleton backend instance: the bbt_gtk4 main sets
   --  BBT.Backends.Selected to Registered'Access at
   --  startup, before running the driver

end BBT.Backends.Gtk4;
