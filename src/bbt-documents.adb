with BBT.IO;           use BBT.IO;
with BBT.Settings;
with BBT.Tests_Builder;

package body BBT.Documents is


   -- type Indent_Level is range 0 .. 3;
   Prefix : constant Texts.Vector := [1 => "",
                                      2 => "  "];
   Current_Indent_Level : Positive := 1;

   -- --------------------------------------------------------------------------
   function To_Bold (S    : String;
                     Bold : Boolean) return String is
     (if Bold then "**" & S & "**" else S);
   function To_Bold (S : Unbounded_String;
                     Bold : Boolean) return String is
     (To_Bold (To_String (S), Bold));

   -- --------------------------------------------------------------------------
   procedure Add_Fail (To : in out Scenario_Type) is
   begin
      To.Failed_Step_Count := @ + 1;
   end Add_Fail;
   procedure Add_Success (To : in out Scenario_Type) is
   begin
      To.Successful_Step_Count := @ + 1;
   end Add_Success;

   -- --------------------------------------------------------------------------
   procedure Put_Text (The_Text : Text) is
      Pref : constant String := Prefix (Current_Indent_Level);
   begin
      for L of The_Text loop
         Put_Line (Pref & L);
      end loop;
   end Put_Text;

   -- --------------------------------------------------------------------------
   --  procedure Put_File_Content (Text : Texts.Vector) is
   --     Pref : constant String := Prefix (Current_Indent_Level);
   --  begin
   --     if not Text.Is_Empty then
   --        Put_Line (Pref & "```");
   --        Put_Text (Text);
   --        Put_Line (Pref & "```");
   --     end if;
   --  end Put_File_Content;

    --  -- --------------------------------------------------------------------------
   procedure Put_Step (Step               : Step_Type;
                       With_Comments      : Boolean := False;
                       With_Bold_Keywords : Boolean := False) is
      pragma Unreferenced (With_Comments, With_Bold_Keywords);
   begin
      Current_Indent_Level := 1;
      declare
         Pref : constant String := Prefix (Current_Indent_Level);
      begin
         --** Put_Line (Pref & S.Kind'Image);
         Put_Line (Pref & "- " & To_String (Step.Step_String));
         if not Texts.Is_Empty (Step.File_Content) then
            Put_Line ("```");
            for L of Step.File_Content loop
               Put_Line (Pref & "  " & L);
            end loop;
            Put_Line ("```");
            New_Line;
         end if;
      end;
   end Put_Step;

   -- --------------------------------------------------------------------------
   procedure Put_Scenario (Scenario           : Scenario_Type;
                           With_Comments      : Boolean := False;
                           With_Bold_Keywords : Boolean := False) is
   begin
      Current_Indent_Level := 1;
      declare
         Pref : constant String := Prefix (Current_Indent_Level) & "### ";
      begin
         if With_Bold_Keywords then
            Put_Line (Pref & To_Bold ("Scenario", With_Bold_Keywords)
                      & ": " &  To_String (Scenario.Name));
         else
            Put_Line (Pref & "Scenario: " & To_String (Scenario.Name));
         end if;
         New_Line;

         for Step of Scenario.Step_List loop
            Put_Step (Step, With_Comments, With_Bold_Keywords);
         end loop;

         if With_Comments then
            Put_Text (Scenario.Comment);
         end if;
      end;
   end Put_Scenario;

   -- --------------------------------------------------------------------------
   procedure Put_Feature (Feature            : Feature_Type;
                          With_Comments      : Boolean := False;
                          With_Bold_Keywords : Boolean := False) is
      Pref : constant String := "## ";
   begin
      Current_Indent_Level := 1;
      if With_Bold_Keywords then
         Put_Line (Pref & To_Bold ("Feature",
                   With_Bold_Keywords) & ": " & To_String (Feature.Name));
      else
         Put_Line (Pref & "Feature" & ": " & To_String (Feature.Name));
      end if;
      New_Line;
      for Scenario of Feature.Scenario_List loop
         Put_Scenario (Scenario, With_Comments, With_Bold_Keywords);
      end loop;
      if With_Comments then
         Put_Text (Feature.Comment);
      end if;
   end Put_Feature;

   -- --------------------------------------------------------------------------
   procedure Put_Document (Doc                : Document_Type;
                           With_Comments      : Boolean := False;
                           With_Bold_Keywords : Boolean := False) is
   begin
      Current_Indent_Level := 1;
      Put_Line ("# " & To_Bold (Doc.Name, With_Bold_Keywords));
      New_Line;
      for Feature of Doc.Feature_List loop
         Put_Feature (Feature, With_Comments, With_Bold_Keywords);
      end loop;
      if With_Comments then
         Put_Text (Doc.Comment);
      end if;
   end Put_Document;

   -- --------------------------------------------------------------------------
   procedure Put_Document_List (Doc_List           : Documents_Lists.Vector;
                                With_Comments      : Boolean;
                                With_Bold_Keywords : Boolean) is
      pragma Unreferenced (With_Comments, With_Bold_Keywords);
   begin
      Put_Line ("**Document list:**");
      New_Line;
      Put_Line ("[[TOC]]");
      New_Line;
      for Doc of Doc_List loop
         Put_Document (Doc,
                       With_Comments      => Settings.With_Comments,
                       With_Bold_Keywords => Settings.With_Bold_Keywords);
      end loop;
   end Put_Document_List;

   -- --------------------------------------------------------------------------
   function Result (Scenario : Scenario_Type) return Test_Result is
   begin
      if Scenario.Failed_Step_Count > 0 then
         return Failed;
      elsif Scenario.Successful_Step_Count > 0 then
         return Successful;
      else
         return Empty;
      end if;
   end Result;

   -- --------------------------------------------------------------------------
   procedure Put_Run_Summary is
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
      Test_Result_Counts    : array (Test_Result) of Natural := [others => 0];
   begin
      for D of BBT.Tests_Builder.The_Document_List.all loop
         for F of D.Feature_List loop
            for Scen of F.Scenario_List loop
               Successful_Step_Count := @ + Scen.Successful_Step_Count;
               Failed_Step_Count     := @ + Scen.Failed_Step_Count;
               Test_Result_Counts (Result (Scen)) := @ + 1;
            end loop;
         end loop;
      end loop;
      New_Line;
      Put_Line ("Failed     tests = " & Test_Result_Counts (Failed)'Image);
      Put_Line ("Successful tests = " & Test_Result_Counts (Successful)'Image);
      Put_Line ("Empty      tests = " & Test_Result_Counts (Empty)'Image);
      if Settings.Is_Authorised (Verbose) then
         New_Line;
         Put_Line ("Failed     steps = " & Failed_Step_Count'Image);
         Put_Line ("Successful steps = " & Successful_Step_Count'Image);
      end if;
   end Put_Run_Summary;

end BBT.Documents;
