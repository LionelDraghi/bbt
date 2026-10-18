-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Text_IO;

use Ada;

separate (BBT.Cmd_Line)

-- -----------------------------------------------------------------------------
procedure Put_Trace_Topics is
   use Characters.Handling;
begin
   Text_IO.Put_Line ("Available topics :");
   for T in IO.Topics loop
      Text_IO.Put_Line ("- " & To_Lower (T'Image));
   end loop;
end Put_Trace_Topics;
