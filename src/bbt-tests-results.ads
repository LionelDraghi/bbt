-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2025, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Documents,
     BBT.Model.Scenarios;

use  BBT.Model;

package BBT.Tests.Results is

   type Test_Result is (Skipped, Failed, Empty, Successful)
     with Default_Value => Empty;

   type Test_Results_Count is private;

   function Result (Scenario : Scenarios.Scenario_Type'Class)
                    return Test_Result;
   -- Compute the Scenario status based on failed and successful step count
   -- stored in the scenario.

   procedure Sum_Results (Docs : Documents.List);
   -- Walk through the Document list to sum all scenarios result's.

   -- function Overall_Results return Test_Results_Count;
   -- function Success (Results : Test_Results_Count) return Boolean;
   function Success return Boolean;
   function No_Fail return Boolean;

   procedure Generate_Badge;

   -- -----------------------------------------------------------------------
   subtype Count_String is String (1 .. 7);

   -- -----------------------------------------------------------------------
   function Count_String_Image (Test : Test_Result) return Count_String;
   -- Warning, image is cut if it does not fit
   function Count (Test : Test_Result) return Natural;

private
   type Test_Results_Count is array (Test_Result) of Natural
     with Default_Component_Value => 0;

end BBT.Tests.Results;
