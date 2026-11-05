-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

--  GUI backends root: the runner only sees this package, the body
--  dispatches every GUI operation to the platform backend child
--  (BBT.Tests.GUI_Backends.Atspi on Linux, an all no-op stub on the
--  other platforms).

package BBT.Tests.GUI_Backends is

   No_Backend_Available : exception;
   --  Raised by every operation when the running platform has no GUI
   --  backend, or when none was selected with --backend

   type Backend_Kind is (No_Backend, GUI_Atspi);

   Selected : Backend_Kind := No_Backend;
   --  Set by the --backend command line option

   procedure Launch (Binary_Path : String);
   --  Spawns the application binary, records its pid as the
   --  correlation target for the accessibility tree walk

   procedure Shutdown;
   --  Kills and reaps the last launched application, if any

   function Window_Appears (Expected_Title : String := "") return Boolean;
   --  Polls the accessibility bus for a top-level window of the
   --  launched application, within the bounded harness deadline

   function Observed_Title return String;
   --  The title of the last window matched by Window_Appears, for
   --  the failure diagnostic

   function Window_Contains (Expected : String) return Boolean;
   --  The rendered text content of the application windows: the
   --  accessible names and Text interface content of the whole
   --  subtree, dialogs included

   function Widget_Named (Role : String; Name : String) return Boolean;
   --  A widget of AT-SPI role Role and accessible name Name exists in
   --  the launched application

   function Widget_Enabled
     (Role : String; Name : String) return Boolean;
   --  The user-visible enabled state of the named widget (STATE_
   --  SENSITIVE on GTK 4, which never emits STATE_ENABLED)

   function Press_Widget (Role : String; Name : String) return Boolean;
   --  Semantic activation of the named widget (the first AT-SPI
   --  action, not injected input)

end BBT.Tests.GUI_Backends;
