-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Adi2 GUI backend: the witness application is an Ada 2022 program
--  built on the Adi2 toolkit, observed and driven through Adi2's own
--  development bridge (Adi.MCP) over file IPC (cf. the Bridge child,
--  and docs/backends.md).
--
--  Correlation is by process id, and by construction: the runner
--  spawns the application binary itself, so the spawned pid names the
--  bridge session. A session that never appears names a probe-less
--  build (validation and release profiles compile Adi2's no-op stub),
--  reported as such, never as a missing window.
--
--  Mapping conventions — the bridge serializes one text field per
--  widget and no accessible-name channel (the probe audit recorded
--  it), so role and name derive from what it does serialize:
--  - role: the widget kind, by family — a button is one, a text input
--    is a textbox, a list box a list, a row label inside a list box a
--    listitem (structure stands in for the arrow relation AT-SPI
--    gives);
--  - name: the widget's own text when it carries one (a button is what
--    it displays), and otherwise the text of the label that
--    immediately precedes it in tree order — the Adi2 counterpart of
--    the toolkit's mnemonic widget, the convention the prototype
--    measured, which names the fields and the two lists alike (a
--    container's own label is painted inside its clipped content box,
--    so it cannot also serve as a visible header);
--  - window title: the root widget's label, the one name-carrying
--    channel the bridge exposes for a container;
--  - enabled state: the serialized disabled state;
--  - selection: the serialized selected state of the row.
--
--  Injection levels, per the interface contract: Type_Into sets the
--  field value through the bridge's set_text (a semantic value set);
--  Enter_Into adds the field commit, the Return key injected into the
--  focused field; Press_Widget clicks the widget centre through the
--  toolkit's ordinary dispatch pipeline.

package BBT.Backends.Adi2 is

   Backend_Kind : constant String := "adi2";

   type Adi2_Backend is new Backend with null record;
   --  The concrete backend registered by the bbt_adi2 main. Its state
   --  (the spawned process, the session directory, the last snapshot)
   --  lives in the body: the backend operates as one instance,
   --  registered once at startup.

   overriding procedure Launch
     (The_Backend : in out Adi2_Backend; Binary_Path : String);
   --  Spawns the application binary, then waits for its bridge
   --  session ("ready" sentinel) within the harness deadline

   overriding procedure Shutdown (The_Backend : in out Adi2_Backend);
   --  Kills the launched application, if any

   overriding function Window_Appears
     (The_Backend : Adi2_Backend; Expected_Title : String := "")
      return Boolean;
   --  The application's root widget is up and visible; the named form
   --  also requires its serialized text to be the expected title

   overriding function Observed_Title
     (The_Backend : Adi2_Backend) return String;
   --  The text of the last root snapshot, for the failure diagnostic

   overriding function Window_Contains
     (The_Backend : Adi2_Backend; Expected : String) return Boolean;
   --  Some rendered text of the visible widget tree contains Expected
   --  (case insensitive, as the bridge's own non-exact search)

   overriding function Widget_Named
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean;
   --  A visible widget of that role and name exists (the naming
   --  conventions above)

   overriding function Widget_Enabled
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean;
   --  The named widget exists and is not disabled. Not found reads
   --  False: the existence check is the preceding scenario step

   overriding function Press_Widget
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean;
   --  Clicks the named widget's centre through the toolkit's ordinary
   --  dispatch pipeline, then reports whether the bridge accepted it

   overriding function Widget_Contains
     (The_Backend : Adi2_Backend; Role, Name, Expected : String)
      return Boolean;
   --  The rendered text of the named widget subtree contains Expected
   --  (list content checks, G080)

   overriding function Widget_Is_Empty
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Boolean;
   --  The named container widget holds no child

   overriding function Widget_Item_Count
     (The_Backend : Adi2_Backend; Role : String; Name : String)
      return Natural;
   --  The number of children of the named container widget (G080)

   overriding procedure Type_Into
     (The_Backend : in out Adi2_Backend;
      Role        : String;
      Name        : String;
      Text        : String);
   --  Sets the named field's value through the bridge's set_text, a
   --  semantic value set with no validation; raises
   --  No_Backend_Available when the field is not reachable

   overriding procedure Enter_Into
     (The_Backend : in out Adi2_Backend;
      Role        : String;
      Name        : String;
      Text        : String);
   --  The Type_Into value set, then the field commit: the Return key
   --  injected into the focused field. Raises No_Backend_Available
   --  when the field is not reachable or the commit is not accepted

   overriding function Close_Window
     (The_Backend : Adi2_Backend; Title : String := "")
      return Boolean;
   --  Asks the application to close through the bridge's quit command
   --  (its own close path), waits for it to go, and falls back to the
   --  process-level close. The title is not used: the bridge exposes
   --  one window per application, so the named and unnamed forms mean
   --  the same

   Registered : aliased Adi2_Backend;
   --  The singleton backend instance: the bbt_adi2 main sets
   --  BBT.Backends.Selected to Registered'Access at startup, before
   --  running the driver

end BBT.Backends.Adi2;
