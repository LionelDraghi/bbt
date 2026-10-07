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

package BBT.Tests.Actions.Output_Checks is

   procedure Check_No_Output (Output    : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels);

   procedure Output_Is (Output    : Text;
                        Step      : Step_Type'Class;
                        Verbosity : Verbosity_Levels);
   procedure Output_Contains (Output    : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels);
   procedure Output_Does_Not_Contain (Output    : Text;
                                      Step      : Step_Type'Class;
                                      Verbosity : Verbosity_Levels);
   procedure Output_Matches (Output    : Text;
                             Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels);
   procedure Output_Does_Not_Match (Output    : Text;
                                    Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels);

   procedure File_Matches (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);
   procedure File_Does_Not_Match (Step      : Step_Type'Class;
                                   Verbosity : Verbosity_Levels);
   procedure Files_Is (Step      : Step_Type'Class;
                       Verbosity : Verbosity_Levels);
   procedure Files_Is_Not (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);
   procedure File_Contains (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels);
   procedure File_Does_Not_Contain (Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels);

end BBT.Tests.Actions.Output_Checks;
