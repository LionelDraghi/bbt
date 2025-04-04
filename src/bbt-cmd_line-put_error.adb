-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (BBT.Cmd_Line)

-- -----------------------------------------------------------------------------
procedure Put_Error (Msg       : String  := "";
                     With_Help : Boolean := False) is
begin
   IO.Put_Error (Msg);
   if With_Help then Put_Help; end if;
end Put_Error;
