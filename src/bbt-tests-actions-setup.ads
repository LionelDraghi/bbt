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

with Text_Utilities; use Text_Utilities;

package BBT.Tests.Actions.Setup is

   procedure Erase_And_Create (Step      : Step_Type'Class;
                               Verbosity : Verbosity_Levels);
   procedure Create_If_None (Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels);

   procedure Setup_No_File (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels);
   procedure Setup_No_Dir (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);
   -- Clean up, with interactive confirmation by user,
   -- unless Settings.Yes is set.

   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type'Class;
                                   Verbosity : Verbosity_Levels);
   procedure Check_Dir_Existence (Dir_Name : String;
                                  Step     : Step_Type'Class;
                                  Verbosity : Verbosity_Levels);

   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels);
   procedure Check_No_Dir (Dir_Name : String;
                           Step     : Step_Type'Class;
                           Verbosity : Verbosity_Levels);

end BBT.Tests.Actions.Setup;
