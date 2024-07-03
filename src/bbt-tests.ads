-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

with BBT.IO;        use BBT.IO;
with BBT.Documents; use BBT.Documents;

private package BBT.Tests is
-- This package (and child packages) is in charge of building the tests list
-- and running it.

   -- --------------------------------------------------------------------------
   procedure Put_Step_Result (Step     : Step_Type;
                              Success  : Boolean;
                              Fail_Msg : String;
                              Loc      : Location_Type);
   -- Common procedure to normalize result output, indented like steps

end BBT.Tests;
