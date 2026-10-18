-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Terminal is

   --  ---------------------------------------------------------------------------
   procedure Set_Size (Fd : Interfaces.C.int) is
      pragma Unreferenced (Fd);
   begin
      --  No pseudo terminal support on Windows: the commands keep the
      --  pipes behavior, and are never on a terminal there.
      null;
   end Set_Size;

end BBT.Terminal;
