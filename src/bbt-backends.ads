-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

--  GUI backends root: the runner only sees this package, and dispatches
--  every GUI operation to the Selected backend access, set at startup by
--  whoever owns a concrete backend (a backend executable crate main,
--  cf. docs/proposed/crate_restructuring.md).
--
--  The core embeds no concrete backend anymore: the default is no
--  backend at all. A document with GUI steps and no registered backend
--  fails eagerly at analysis time (cf. BBT.Main), and each operation
--  below raises No_Backend_Available as a late run time fallback.

package BBT.Backends is

   No_Backend_Available : exception;
   --  Raised by every root operation when no backend was registered

   type Backend is abstract tagged null record;
   --  The abstract backend interface; every concrete backend owns its
   --  accessibility environment and the pid correlation with the
   --  launched application

   procedure Launch
     (The_Backend : in out Backend; Binary_Path : String) is abstract;
   --  Spawns the application binary, records its pid as the
   --  correlation target for the accessibility tree walk

   procedure Shutdown (The_Backend : in out Backend) is abstract;
   --  Kills and reaps the last launched application, if any

   function Window_Appears
     (The_Backend : Backend; Expected_Title : String := "")
      return Boolean
     is abstract;
   --  Polls the accessibility bus for a top-level window of the
   --  launched application, within the bounded harness deadline

   function Observed_Title (The_Backend : Backend) return String
     is abstract;
   --  The title of the last window matched by Window_Appears, for
   --  the failure diagnostic

   function Window_Contains
     (The_Backend : Backend; Expected : String) return Boolean
     is abstract;
   --  The rendered text content of the application windows: the
   --  accessible names and Text interface content of the whole
   --  subtree, dialogs included

   function Widget_Named
     (The_Backend : Backend; Role : String; Name : String)
      return Boolean
     is abstract;
   --  A widget of AT-SPI role Role and accessible name Name exists in
   --  the launched application

   function Widget_Enabled
     (The_Backend : Backend; Role : String; Name : String)
      return Boolean
     is abstract;
   --  The user-visible enabled state of the named widget (STATE_
   --  SENSITIVE on GTK 4, which never emits STATE_ENABLED)

   function Press_Widget
     (The_Backend : Backend; Role : String; Name : String)
      return Boolean
     is abstract;
   --  Semantic activation of the named widget (the first AT-SPI
   --  action, not injected input)

   Selected : access Backend'Class := null;
   --  Set at startup by whoever owns a concrete backend; left null
   --  when bbt runs backend-less

   procedure Launch (Binary_Path : String);
   --  Dispatches to Selected.Launch, or raises the above exception

   procedure Shutdown;
   --  Dispatches to Selected.Shutdown, no-op without backend

   function Window_Appears (Expected_Title : String := "") return Boolean;
   --  Dispatches to Selected.Window_Appears, False without backend

   function Observed_Title return String;
   --  Dispatches to Selected.Observed_Title, "" without backend

   function Window_Contains (Expected : String) return Boolean;
   --  Dispatches to Selected.Window_Contains, False without backend

   function Widget_Named (Role : String; Name : String) return Boolean;
   --  Dispatches to Selected.Widget_Named, False without backend

   function Widget_Enabled
     (Role : String; Name : String) return Boolean;
   --  Dispatches to Selected.Widget_Enabled, False without backend

   function Press_Widget (Role : String; Name : String) return Boolean;
   --  Dispatches to Selected.Press_Widget, False without backend

end BBT.Backends;
