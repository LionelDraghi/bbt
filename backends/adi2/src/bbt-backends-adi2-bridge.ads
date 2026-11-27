-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The Adi.MCP side of the adi2 backend: the file-IPC client of Adi2's
--  development bridge, and the widget snapshot it answers with.
--
--  Transport: a per-process session directory <temp>/adi_mcp/<PID>/
--  holding cmd_<id>.json requests, resp_<id>.json answers and a "ready"
--  sentinel rewritten while the application runs. The application
--  polls it once per frame; one command in flight at a time.
--
--  The snapshot reader rests on the bridge's own serialization: one
--  flat object per widget, carrying type, id, path, geometry, text,
--  states, the visible/clickable/focusable flags and its child count,
--  children nested under "children". It walks that answer linearly —
--  every widget object starts with {"type": and carries its own
--  absolute path — which is enough to rebuild the tree the checks
--  need, and keeps a full JSON reader out of the crate.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BBT.Backends.Adi2.Bridge is

   Launch_Failed : exception;
   --  No probe session directory appeared within the bounded deadline:
   --  the binary is missing, or was built without the bridge (the
   --  validation and release profiles compile Adi2's documented no-op
   --  stub), so the scenario cannot run silently un-probed

   function Session_Base_Dir return String;
   --  <system temp>/adi_mcp: the bridge reads TMPDIR, else TEMP, else
   --  TMP, else /tmp, and so does this client

   function Session_Open return Boolean;
   --  Whether a launched application's session is known

   procedure Launch (Binary_Path : String);
   --  Spawns the application binary and waits for its session ("ready"
   --  sentinel) within the bounded deadline, raising Launch_Failed
   --  otherwise

   procedure Shutdown;
   --  Kills the launched application, if any

   function Quit (Deadline : Duration) return Boolean;
   --  Asks the application to exit through its own quit path, and
   --  reports whether it went away within Deadline; the caller falls
   --  back to Shutdown

   function Widget_Tree return String;
   --  A fresh widget_tree answer, "" when the session stopped
   --  answering

   function Run_Command (Command, Parameters : String) return String;
   --  One command to the live session; Parameters is the JSON members
   --  after the command name ("" when none). "" when no answer came in
   --  time

   --  The textual reads of the bridge's serialization --------------------

   function Member_Value (Object, Key : String) return String;
   --  The string member Key of Object, "" when absent
   function Member_Number (Object, Key : String) return Natural;
   --  The numeric member Key, 0 when absent or non numeric
   function Member_Boolean (Object, Key : String) return Natural;
   --  0 = absent (unknown), 1 = false, 2 = true
   function Ok (Response : String) return Boolean;
   --  Whether a response carries the bridge's {"status":"ok"

   --  The widget snapshot -------------------------------------------------

   type Node is record
      Path        : Unbounded_String := Null_Unbounded_String;
      Kind        : Unbounded_String := Null_Unbounded_String;
      Id          : Natural := 0;
      Text        : Unbounded_String := Null_Unbounded_String;
      Visible     : Boolean := False;
      Disabled    : Boolean := False;
      Selected    : Boolean := False;
      Child_Count : Natural := 0;
   end record;

   package Node_Vectors is new Ada.Containers.Vectors (Positive, Node);

   function Parse (Response : String) return Node_Vectors.Vector;
   --  The nodes of a widget_tree answer, in tree order (the root
   --  first), overlays excluded

   function Find_By_Path
     (Nodes : Node_Vectors.Vector; Path : String) return Natural;
   --  The index of the node at Path, 0 when absent

   function Parent_Path (Path : String) return String;
   --  The path of the parent, "" for the root

   function Is_Descendant (Path, Ancestor : String) return Boolean;
   --  Whether Path is below Ancestor (both are dot paths)

   function Shown
     (Nodes : Node_Vectors.Vector; Index : Natural) return Boolean;
   --  Whether the node at Index and its whole ancestor chain are
   --  visible: a hidden row keeps its own visible flag, its container
   --  does not

   function Contains (Text, Pattern : String) return Boolean;
   --  Case insensitive substring test, as the bridge's own
   --  find_by_text non-exact search

end BBT.Backends.Adi2.Bridge;
