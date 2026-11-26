-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The bbt_webapp backend main: registers the WebDriver backend, then
--  runs the same driver as the bbt CLI (cf.
--  docs/dev/design_discussions.md (D16)). The bbt core CLI delegates to
--  this executable thanks to --backend=webapp.

with BBT.Driver;
with BBT.Backends;
with BBT.Backends.Webapp;

procedure BBT_Webapp.Main is
begin
   BBT.Backends.Selected
     := BBT.Backends.Webapp.Registered'Access;

   BBT.Driver.Run;
end BBT_Webapp.Main;
