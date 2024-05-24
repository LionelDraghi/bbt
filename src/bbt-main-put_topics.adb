-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Text_IO;

separate (BBT.Main)

-- -----------------------------------------------------------------------------
procedure Put_Topics is
   use Ada.Characters.Handling;
begin
   Ada.Text_IO.Put_Line ("Available topics :");
   for T in IO.Topics loop
      Ada.Text_IO.Put_Line ("- " & To_Lower (T'Image));
   end loop;
end Put_Topics;
