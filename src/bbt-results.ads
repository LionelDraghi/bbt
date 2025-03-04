-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Documents;

private package BBT.Results is

   type Test_Result is (Skipped, Failed, Empty, Successful)
     with Default_Value => Empty;

   type Test_Results_Count is array (Test_Result) of Natural
     with Default_Component_Value => 0;

   function Result (Scenario : Documents.Scenario_Type) return Test_Result;
   -- Compute the Scenario status based on failed and successful step count
   -- stored in the scenario.

   procedure Sum_Results (Docs : access Documents.Documents_Lists.Vector);
   -- Walk through the Document list to count the results

   function Overall_Results return Test_Results_Count;

   procedure Generate_Badge;

end BBT.Results;
