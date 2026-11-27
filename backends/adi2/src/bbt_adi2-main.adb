-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  The bbt_adi2 backend main: registers the Adi2 backend, then runs the
--  same driver as the bbt CLI (cf. docs/dev/design_discussions.md (D16)).
--  The bbt core CLI delegates to this executable thanks to
--  --backend=adi2.

with BBT.Driver;
with BBT.Backends;
with BBT.Backends.Adi2;

procedure BBT_Adi2.Main is
begin
   BBT.Backends.Selected
     := BBT.Backends.Adi2.Registered'Access;

   BBT.Driver.Run;
end BBT_Adi2.Main;
