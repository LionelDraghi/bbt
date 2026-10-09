-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
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
