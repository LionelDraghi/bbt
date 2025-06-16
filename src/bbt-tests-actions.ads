-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model,
     BBT.Model.Steps;

use BBT.Model,
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
                      Spawn_OK     : out Boolean;
                      Return_Code  : out Integer);

   procedure Create_If_None (Step : Step_Type'Class);
   procedure Erase_And_Create (Step : Step_Type'Class);

   procedure Setup_No_File (Step : Step_Type'Class);
   procedure Setup_No_Dir (Step : Step_Type'Class);
   -- Clean up, with interactive confirmation by user,
   -- unless Settings.Assume_Yes is set.

   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               : Step_Type'Class);
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               : Step_Type'Class);

   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type'Class);
   procedure Check_Dir_Existence (Dir_Name : String;
                                  Step     : Step_Type'Class);
   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type'Class);
   procedure Check_No_Dir (Dir_Name : String;
                           Step     : Step_Type'Class);

   procedure Check_No_Output (Output : Text;
                              Step   : Step_Type'Class);
   procedure Output_Is (Output : Text;
                        Step   : Step_Type'Class);
   procedure Output_Contains (Output : Text;
                              Step   : Step_Type'Class);
   procedure Output_Does_Not_Contain (Output : Text;
                                      Step   : Step_Type'Class);
   procedure Output_Matches (Output : Text;
                             Step   : Step_Type'Class);
   procedure Output_Does_Not_Match (Output : Text;
                                    Step   : Step_Type'Class);

   procedure Files_Is (Step : Step_Type'Class);
   procedure Files_Is_Not (Step : Step_Type'Class);
   procedure File_Contains (Step : Step_Type'Class);
   procedure File_Does_Not_Contain (Step : Step_Type'Class);

end BBT.Tests.Actions;
