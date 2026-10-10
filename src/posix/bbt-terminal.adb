-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Terminal is

   procedure C_Set_Winsize (Fd   : Interfaces.C.int;
                            Rows : Interfaces.C.int;
                            Cols : Interfaces.C.int);
   pragma Import (C, C_Set_Winsize, "bbt_set_winsize");
   --  The C wrapper on ioctl(TIOCSWINSZ), in src/posix/bbt_terminal.c:
   --  ioctl is variadic, so Ada cannot import it directly, and the
   --  TIOCSWINSZ request constant differs between Linux and macOS,
   --  which only the C preprocessor resolves portably (the same
   --  conclusion termicap reached for TIOCGWINSZ)

   --  ---------------------------------------------------------------------------
   procedure Set_Size (Fd : Interfaces.C.int) is
   begin
      C_Set_Winsize (Fd, Rows, Cols);
   end Set_Size;

end BBT.Terminal;
