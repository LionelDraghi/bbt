-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Steps,
     BBT.IO;

use BBT.Model,
    BBT.IO;

package BBT.Tests.Builder is
-- This package is in charge of building the tests list
-- by processing lines extracted from Scenarios files.

   Missing_Scenario : exception;

   -- --------------------------------------------------------------------------
   procedure Add_Document   (Name : String);
   procedure Add_Feature    (Name : String;
                             Loc  : Location_Type);
   procedure Add_Scenario   (Name : String;
                             Loc  : Location_Type);
   procedure Add_Background (Name : String;
                             Loc  : Location_Type);
   procedure Add_Step       (Step_Info           : in out Steps.Step_Data;
                             Code_Block_Expected :        Boolean;
                             Cmd_List            :        Steps.Cmd_List;
                             Loc                 : Location_Type);
   procedure Add_Line       (Line : String;
                             Loc  : Location_Type);
   procedure Add_Code_Fence (Loc : Location_Type);
   -- function In_File_Content return Boolean;
   -- Return true if we are in a code block (between code fences)

   procedure End_Of_Scenario (Loc : Location_Type);

   -- --------------------------------------------------------------------------
   procedure Duplicate_Multiple_Run;
   -- Search for "When I run X or Y" steps, and duplicate the scenario,
   -- except that one will run X and the other will run Y.

end BBT.Tests.Builder;
