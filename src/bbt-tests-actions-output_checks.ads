-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
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

   --  All the comparison checks below are stated in their positive form;
   --  they honor the step polarity (Step_Data.Negated): when set, the
   --  comparison result is inverted, and the failure message adapted.

   procedure Output_Not_Empty (Output    : Text;
                               Step      : Step_Type'Class;
                               Verbosity : Verbosity_Levels);
   --  Checks that the output is not empty, or empty when the step is
   --  negated ("there is no output")

   procedure Output_Is (Output    : Text;
                        Step      : Step_Type'Class;
                        Verbosity : Verbosity_Levels);
   procedure Output_Contains (Output    : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels);
   procedure Output_Matches (Output    : Text;
                             Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels);

   procedure File_Matches (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);
   procedure Files_Is (Step      : Step_Type'Class;
                       Verbosity : Verbosity_Levels);
   procedure File_Contains (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels);

end BBT.Tests.Actions.Output_Checks;
