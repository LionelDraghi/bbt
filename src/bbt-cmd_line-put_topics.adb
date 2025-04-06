-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Text_IO;

separate (BBT.Cmd_Line)

-- -----------------------------------------------------------------------------
procedure Put_Topics is
   use Ada.Characters.Handling;
begin
   Ada.Text_IO.Put_Line ("Available topics :");
   for T in IO.Topics loop
      Ada.Text_IO.Put_Line ("- " & To_Lower (T'Image));
   end loop;
end Put_Topics;
