-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, 2026 Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Steps,
     BBT.IO;

package BBT.Scenarios.Steps is

   function Parse (Line                :        Unbounded_String;
                   Loc                 : in out IO.Location_Type;
                   Code_Block_Expected :    out Boolean)
                   return BBT.Model.Steps.Step_Data;

   procedure Put_Keywords;
   procedure Put_Grammar;

end BBT.Scenarios.Steps;
