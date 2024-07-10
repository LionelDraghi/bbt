-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------

package BBT.Tests.Builder is
-- This package is in charge of building the tests list
-- by processing lines extracted from Scenarios files.

   Missing_Scenario : exception;

   -- --------------------------------------------------------------------------
   procedure Add_Document   (Name : String);
   procedure Add_Feature    (Name : String; Loc : Location_Type);
   procedure Add_Scenario   (Name : String; Loc : Location_Type);
   procedure Add_Background (Name : String; Loc : Location_Type);
   procedure Add_Step       (Step     : Step_Type;
                             Cmd_List : Cmd_Lists.Vector);
   procedure Add_Line       (Line : String; Loc : Location_Type);
   procedure Add_Code_Block (Loc : Location_Type);

   -- --------------------------------------------------------------------------
   procedure Duplicate_Multiple_Run;
   -- Search for "When I run X or Y" steps, and duplicate the scenario,
   -- except that one will run x and the other will run Y.

   -- --------------------------------------------------------------------------
   function The_Tests_List return access Documents_Lists.Vector;

end BBT.Tests.Builder;
