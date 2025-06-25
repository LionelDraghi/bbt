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

private package BBT.Tests.Actions is

   function Is_Success (I : Integer) return Boolean;
   function Is_Failure (I : Integer) return Boolean is
     (not Is_Success (I));

   procedure Run_Cmd (Step         :     Step_Type'Class;
                      Cmd          :     String;
                      Output_Name  :     String;
                      Check_Result :     Boolean;
                      Verbosity    :     Verbosity_Levels;
                      Spawn_OK     : out Boolean;
                      Return_Code  : out Integer);

   procedure Create_If_None (Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels);
   procedure Erase_And_Create (Step      : Step_Type'Class;
                               Verbosity : Verbosity_Levels);

   procedure Setup_No_File (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels);
   procedure Setup_No_Dir (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);
   -- Clean up, with interactive confirmation by user,
   -- unless Settings.Assume_Yes is set.

   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               : Step_Type'Class;
                           Verbosity          : Verbosity_Levels);
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               : Step_Type'Class;
                              Verbosity          : Verbosity_Levels);

   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type'Class;
                                   Verbosity : Verbosity_Levels);
   procedure Check_Dir_Existence (Dir_Name : String;
                                  Step      : Step_Type'Class;
                                  Verbosity : Verbosity_Levels);
   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels);
   procedure Check_No_Dir (Dir_Name : String;
                           Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);

   procedure Check_No_Output (Output : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels);
   procedure Output_Is (Output : Text;
                        Step      : Step_Type'Class;
                        Verbosity : Verbosity_Levels);
   procedure Output_Contains (Output : Text;
                              Step      : Step_Type'Class;
                              Verbosity : Verbosity_Levels);
   procedure Output_Does_Not_Contain (Output : Text;
                                      Step      : Step_Type'Class;
                                      Verbosity : Verbosity_Levels);
   procedure Output_Matches (Output : Text;
                             Step      : Step_Type'Class;
                             Verbosity : Verbosity_Levels);
   procedure Output_Does_Not_Match (Output : Text;
                                    Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels);

   procedure Files_Is (Step      : Step_Type'Class;
                       Verbosity : Verbosity_Levels);
   procedure Files_Is_Not (Step      : Step_Type'Class;
                           Verbosity : Verbosity_Levels);
   procedure File_Contains (Step      : Step_Type'Class;
                            Verbosity : Verbosity_Levels);
   procedure File_Does_Not_Contain (Step      : Step_Type'Class;
                                    Verbosity : Verbosity_Levels);

end BBT.Tests.Actions;
