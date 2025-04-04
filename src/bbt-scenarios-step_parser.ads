-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Documents; use BBT.Documents;
with BBT.IO;        use BBT.IO;

package BBT.Scenarios.Step_Parser is

   -- --------------------------------------------------------------------------
   function Parse (Line                : Unbounded_String;
                   Loc                 : Location_Type;
                   Code_Block_Expected : out Boolean;
                   Cmd_List            : out Cmd_Lists.Vector) return Step_Type;

   procedure Put_Keywords;
   procedure Put_Grammar;

end BBT.Scenarios.Step_Parser;
