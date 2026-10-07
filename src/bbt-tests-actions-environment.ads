-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Model,
     BBT.Model.Steps;

use BBT.IO,
    BBT.Model,
    BBT.Model.Steps;

package BBT.Tests.Actions.Environment is

   procedure Set_Env_Var (Step      : Step_Type'Class;
                          Verbosity : Verbosity_Levels);
   procedure Unset_Env_Var (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);
   -- The variable keeps its value for the commands run afterwards, until
   -- Restore_Environment is called.

   procedure Restore_Environment;
   -- Give back to every variable set or unset since the last call the value
   -- (or the absence) it had before. Called after each scenario.

end BBT.Tests.Actions.Environment;
