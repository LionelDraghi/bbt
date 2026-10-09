-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Interfaces.C;

private package BBT.Terminal is

   Rows : constant := 24;
   Cols : constant := 80;
   --  The size given to the pseudo terminal allocated to the interactive
   --  commands: the simulated terminal must be indistinguishable from
   --  a real one for a program querying its size
   --  (cf. docs/proposed_features/pty.md)

   procedure Set_Size (Fd : Interfaces.C.int);
   --  Fix the window size of the terminal behind the descriptor Fd to
   --  Rows x Cols. On the platforms with no pseudo terminal support
   --  (Windows), this is a no-op: the commands keep the pipes behavior,
   --  and are never on a terminal there.

end BBT.Terminal;
