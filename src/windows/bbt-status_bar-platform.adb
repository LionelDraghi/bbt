-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Interfaces.C;
use type Interfaces.C.unsigned;

package body BBT.Status_Bar.Platform is

   --  -------------------------------------------------------------------------
   function GetConsoleOutputCP return Interfaces.C.unsigned;
   pragma Import (Stdcall, GetConsoleOutputCP, "GetConsoleOutputCP");

   --  -------------------------------------------------------------------------
   function UTF8_Output return Boolean is
     (GetConsoleOutputCP = 65001);

end BBT.Status_Bar.Platform;
