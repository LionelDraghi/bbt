-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- -----------------------------------------------------------------------------

--  AT-SPI backend child (Linux only): libatspi direct binding, no
--  dev headers required. The same unit exists as a no-op stub in
--  src/no_gui for non Linux platforms, so the parent always compiles.

package BBT.Tests.GUI_Backends.Atspi is

   Backend_Name : constant String := "atspi";

   procedure Launch (Binary_Path : String);
   --  Spawns the application binary, records its pid as the
   --  correlation target for the accessibility tree walk.

   procedure Shutdown;
   --  Kills and reaps the last launched application, if any.

   function Window_Appears (Expected_Title : String := "") return Boolean;
   --  Polls the accessibility bus for a top-level window of the
   --  last launched application, within the bounded harness deadline.

   function Window_Contains (Expected : String) return Boolean;
   --  The rendered text content of the application windows: the
   --  accessible names and Text interface content of the whole
   --  subtree, dialogs included.

   function Observed_Title return String;
   --  The title of the last window matched by Window_Appears, for
   --  the failure diagnostic.

   function Widget_Named (Role : String; Name : String) return Boolean;
   --  A widget of AT-SPI role Role and accessible name Name exists
   --  in the launched application.

   function Widget_Enabled
     (Role : String; Name : String) return Boolean;
   --  The user-visible enabled state of the named widget (STATE_
   --  SENSITIVE on GTK 4, which never emits STATE_ENABLED).

end BBT.Tests.GUI_Backends.Atspi;
