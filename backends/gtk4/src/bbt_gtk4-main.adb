-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The bbt_gtk4 backend main: registers the AT-SPI / GTK 4 backend,
--  then runs the same driver as the bbt CLI
--  (cf. docs/dev/design_discussions.md (D16)). The bbt core CLI
--  delegates to this executable thanks to --backend=gtk4.

with BBT.Driver;
with BBT.Backends;
with BBT.Backends.Gtk4;

procedure BBT_Gtk4.Main is
begin
   BBT.Backends.Selected
     := BBT.Backends.Gtk4.Registered'Access;

   BBT.Driver.Run;
end BBT_Gtk4.Main;
