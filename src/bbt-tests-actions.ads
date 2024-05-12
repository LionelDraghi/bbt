with Text_Utilities; use Text_Utilities;

private package BBT.Tests.Actions is

   procedure Run_Cmd (Cmd         :        String;
                      Output_Name :        String;
                      Spawn_OK    :    out Boolean;
                      Return_Code :    out Integer);

   procedure Create_File (Step     :        Step_Type;
                          Scenario : in out Scenario_Type);

   procedure Delete_File (Step     :        Step_Type;
                          Scenario : in out Scenario_Type);
   --  Clean up, with interactive confirmation by user,
   --  unless Settings.Assume_Yes is set.

   procedure Return_Error (Last_Returned_Code : Integer;
                           Step               :        Step_Type;
                           Scenario : in out Scenario_Type);
   procedure Return_No_Error (Last_Returned_Code : Integer;
                              Step               :        Step_Type;
                              Scenario : in out Scenario_Type);

   procedure Check_File_Existence (File_Name : String;
                                   Step      : Step_Type;
                                   Scenario  : in out Scenario_Type);
   procedure Check_No_File (File_Name : String;
                            Step      : Step_Type;
                            Scenario  : in out Scenario_Type);

   procedure Output_Equal_To (Output   : Text;
                              Step     :        Step_Type;
                              Scenario : in out Scenario_Type);
   procedure Output_Contains (Output   : Text;
                              Step     :        Step_Type;
                              Scenario : in out Scenario_Type);

   procedure Files_Equal_To (Step     :        Step_Type;
                             Scenario : in out Scenario_Type);
   procedure File_Contains (Step     :        Step_Type;
                            Scenario : in out Scenario_Type);

end BBT.Tests.Actions;
