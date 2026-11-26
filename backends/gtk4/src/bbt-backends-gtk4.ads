-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  GTK 4 GUI backend (Linux): the AT-SPI accessibility backend,
--  packaged as its own bbt backend crate
--  (cf. docs/dev/design_discussions.md (D16)). Own libatspi binding:
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

   overriding function Widget_Contains
     (The_Backend : Gtk4_Backend; Role, Name, Expected : String)
      return Boolean;
   --  The rendered text of the named widget subtree contains the
   --  expected text (list content checks, G080)

   overriding function Widget_Is_Empty
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Boolean;
   --  The named container widget holds no child

   overriding function Widget_Item_Count
     (The_Backend : Gtk4_Backend; Role : String; Name : String)
      return Natural;
   --  The number of children of the named container widget (G080)

   overriding procedure Type_Into
     (The_Backend : in out Gtk4_Backend;
      Role        : String;
      Name        : String;
      Text        : String);
   --  Types Text into the named text field: sets its contents through
   --  the AT-SPI EditableText interface (a semantic value set, not
   --  injected input), and raises No_Backend_Available when no
   --  editable field of that name is reachable

   overriding procedure Enter_Into
     (The_Backend : in out Gtk4_Backend;
      Role        : String;
      Name        : String;
      Text        : String);
   --  The Type_Into value set, then the field commit: a synthesized
   --  Return key press (X11 XTEST), sent to the focused entry. The
   --  AT-SPI activate action is not usable for that: GTK 4 dispatches
   --  it to gtk_widget_activate and discards the outcome, a silent
   --  no-op for entries (no activation signal on the widget class).
   --  Raises No_Backend_Available when the field is not reachable or
   --  no X11 display is available for the injection

   overriding function Close_Window
     (The_Backend : Gtk4_Backend; Title : String := "")
      return Boolean;
   --  Closes the window named Title — or the application's only
   --  window when no title is given, an error if the application
   --  exposes several — through the window.close AT-SPI action of
   --  the frame accessible (a graceful Gtk close request, the
   --  application's own close path, cf. G095), selected by action
   --  name, never by index: the frame action set is toolkit version
   --  dependent. The answer is True when the window is gone from the
   --  accessibility tree within the harness deadline, dying with the
   --  process reaped (its main loop ended, or the process-level
   --  fallback close); False when no such window exists

   Registered : aliased Gtk4_Backend;
   --  The singleton backend instance: the bbt_gtk4 main sets
   --  BBT.Backends.Selected to Registered'Access at
   --  startup, before running the driver

end BBT.Backends.Gtk4;
