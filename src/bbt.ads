-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: BBT specification
--
-- Purpose:
--    This package is empty. Child units do the real job :
--
--    procedure <BBT.Main> - is in charge of
--       controlling the execution flow according to the command line
--    package <BBT.Cmd_Line> - do the command line analysis
--    package <BBT.Settings> - global settings, resulting mainly from
--       cmd line analysis (and env. variables in the future)
--    ...
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

package BBT is

   pragma Pure;

end BBT;
