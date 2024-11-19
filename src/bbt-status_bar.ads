with BBT.Documents;

private package BBT.Status_Bar is

   --  -------------------------------------------------------------------------
   procedure Enable;
   procedure Disable;

   --  -------------------------------------------------------------------------
   procedure Initialize_Progress_Bar (Max_Event : Positive);

   --  -------------------------------------------------------------------------
   procedure Progress_Bar_Next_Step (File_Name : String);

   --  -------------------------------------------------------------------------
   -- procedure Put_Results (Results : Documents.Test_Results_Count);

   --  -------------------------------------------------------------------------
   procedure Put_Activity (S : String);

end BBT.Status_Bar;
