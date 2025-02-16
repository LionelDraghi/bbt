-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Text_Utilities; use Text_Utilities;

private package BBT.Tests.Actions is

   function Is_Success (I : Integer) return Boolean;

   procedure Run_Cmd (Step         :     Step_Type;
                      Cmd          :     String;
                      Output_Name  :     String;
                      Check_Result :     Boolean;
                      Spawn_OK     : out Boolean;
                      Return_Code  : out Integer);

   procedure Create_If_None (Step : Step_Type);
   procedure Create_New (Step : Step_Type);
   procedure Erase_And_Create (Step : Step_Type);

   procedure Setup_No_File (Step : Step_Type);
   procedure Setup_No_Dir (Step : Step_Type);
   -- Clean up, with interactive confirmation by user,
   -- unless Settings.Assume_Yes is set.

   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               : Step_Type);
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               : Step_Type);

   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type);
   procedure Check_Dir_Existence (Dir_Name : String;
                                  Step     : Step_Type);
   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type);
   procedure Check_No_Dir (Dir_Name : String;
                           Step     : Step_Type);

   procedure Check_No_Output (Output : Text;
                              Step   : Step_Type);
   procedure Output_Is (Output : Text;
                        Step   : Step_Type);
   procedure Output_Contains (Output : Text;
                              Step   : Step_Type);
   procedure Output_Does_Not_Contain (Output : Text;
                                      Step   : Step_Type);
   procedure Output_Matches (Output : Text;
                             Step   : Step_Type);
   procedure Output_Does_Not_Match (Output : Text;
                                    Step   : Step_Type);

   procedure Files_Is (Step : Step_Type);
   procedure Files_Is_Not (Step : Step_Type);
   procedure File_Contains (Step : Step_Type);
   procedure File_Does_Not_Contain (Step : Step_Type);

end BBT.Tests.Actions;
