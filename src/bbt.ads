-- -----------------------------------------------------------------------------
-- bbt, the BlackBox tester (https://github.com/LionelDraghi/bbt)
-- © 2024 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
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
