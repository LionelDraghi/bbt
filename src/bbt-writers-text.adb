-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Writers.Text is

   -- --------------------------------------------------------------------------
   Processor : aliased Text_Writer;

   --  -- --------------------------------------------------------------------------
   --  procedure Initialize is
   --  begin
   --     Register (Writer  => Processor'Access,
   --               For_Format => Txt);
   --  end Initialize;

begin
   Register (Writer     => Processor'Access,
             For_Format => Txt);

end BBT.Writers.Text;
