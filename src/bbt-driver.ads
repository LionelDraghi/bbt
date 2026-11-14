-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The bbt driver: command line analysis, document analysis and
--  command dispatch. Shared by the core CLI main (bbt) and by every
--  backend executable main (bbt_gtk4, ...): a backend main registers
--  its concrete backend in BBT.Backends.Selected, then
--  calls Run
--  (cf. docs/proposed/crate_restructuring.md).

package BBT.Driver is

   procedure Run;
   --  Analyzes the command line, then runs the requested command

end BBT.Driver;
