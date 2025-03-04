-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Output_Formatter.Text is

   -- --------------------------------------------------------------------------
   Processor : aliased Text_Formatter;

   -- --------------------------------------------------------------------------
   procedure Initialize is
   begin
      Register (Formatter  => Processor'Access,
                For_Format => Txt);
   end Initialize;

end BBT.Output_Formatter.Text;
