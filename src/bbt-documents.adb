with BBT.Tests.Builder;

with Ada.Directories; use Ada.Directories;

package body BBT.Documents is

   -- type Indent_Level is range 0 .. 3;
   Prefix : constant Texts.Vector := [1 => "",
                                      2 => "  "];
   Current_Indent_Level : Positive := 1;

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type)
   is
      --  procedure Put_If_Not_Null (Prefix : String; S : Unbounded_String) is
      --  begin
      --     if S /= Null_Unbounded_String then
      --        Output.Put (Prefix & " " & S'Image);
      --     end if;
      --  end Put_If_Not_Null;
   begin
      Output.Put (S.Cat'Image & ", ");
      Output.Put (S.Action'Image);
      Output.Put (", Step_String = "    & S.Step_String'Image);
      Output.Put (", Object_String = "  & S.Object_String'Image);
      Output.Put (", Subject_String = " & S.Subject_String'Image);
      Output.Put (", File_Type = "      & S.File_Type'Image);
      if not S.File_Content.Is_Empty then
         Output.Put (S.File_Content'Image);
      end if;
   end Put_Image;

   -- --------------------------------------------------------------------------
   procedure Add_Result (Success : Boolean; To : in out Scenario_Type) is
   begin
      if Success then
         To.Successful_Step_Count := @ + 1;
      else
         To.Failed_Step_Count := @ + 1;
      end if;
   end Add_Result;

   -- --------------------------------------------------------------------------
   procedure Put_Text (The_Text : Text) is
      Pref : constant String := Prefix (Current_Indent_Level);
   begin
      for L of The_Text loop
         Put_Line (Pref & L);
      end loop;
   end Put_Text;

   -- --------------------------------------------------------------------------
   procedure Put_Step (Step : Step_Type) is
      Pref : constant String := Prefix (1);
   begin
      Put_Line (Pref & Step'Image);
   end Put_Step;

   -- --------------------------------------------------------------------------
   procedure Put_Scenario (Scenario : Scenario_Type) is
   begin
      Current_Indent_Level := 1;
      declare
         Pref : constant String := Prefix (Current_Indent_Level) & "### ";
      begin
         New_Line;
         Put_Line (Pref & "Scenario " & To_String ((Scenario.Name)));
         New_Line;
         for Step of Scenario.Step_List loop
            Put_Step (Step);
         end loop;
      end;
   end Put_Scenario;

   function Parent_Doc (Scen : Scenario_Type) return access Document_Type is
     (if Scen.Parent_Feature /= null then Scen.Parent_Feature.Parent_Document
      else Scen.Parent_Document);

   function Is_In_Feature (Scen : Scenario_Type) return Boolean is
     (Scen.Parent_Feature /= null);

   -- --------------------------------------------------------------------------
   procedure Put_Feature (Feature : Feature_Type) is
      Pref : constant String := "## ";
   begin
      Current_Indent_Level := 1;
      Put_Line (Pref & "Feature" & ": " & To_String (Feature.Name));
      for Scenario of Feature.Scenario_List loop
         Put_Scenario (Scenario);
      end loop;
   end Put_Feature;

   -- --------------------------------------------------------------------------
   procedure Put_Document (Doc : Document_Type) is
   begin
      Current_Indent_Level := 1;
      Put_Line ("# " & To_String (Doc.Name));
      New_Line;
      for Feature of Doc.Feature_List loop
         Put_Feature (Feature);
      end loop;
   end Put_Document;

   -- --------------------------------------------------------------------------
   procedure Put_Document_List (Doc_List : Documents_Lists.Vector) is
   begin
      Put_Line ("**Document list:**");
      New_Line;
      Put_Line ("[[TOC]]");
      New_Line;
      for Doc of Doc_List loop
         Put_Document (Doc);
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
      Test_Result_Counts : array (Test_Result) of Natural := [others => 0];
      procedure Get_Results (S : Scenario_Type) is
      begin
         Test_Result_Counts (Result (S)) := @ + 1;
      end Get_Results;
   begin
      for D of BBT.Tests.Builder.The_Tests_List.all loop

         if D.Feature_List.Is_Empty and D.Scenario_List.Is_Empty then
            -- Empty Doc should be reported
            Test_Result_Counts (Empty) := @ + 1;
         end if;
         --
         --  if D.Background /= null then
         --     -- Background results are normally moved to the scenario after run,
         --     -- but in case of a scenario where there is only the background...
         --     -- Fixme: not sure this is usefull at all.
         --     Get_Results (D.Background.all);
         --  end if;
         --
         for Scen of D.Scenario_List loop
            Get_Results (Scen);
         end loop;

         for F of D.Feature_List loop

            if F.Scenario_List.Is_Empty then
               -- Empty Feature should be reported
               Test_Result_Counts (Empty) := @ + 1;
            end if;

            --  if F.Background /= null then
            --     -- Background results are normally moved to the scenario after
            --     -- run, but in case of a scenario where there is only the
            --     -- background...
            --     -- Fixme: not sure this is usefull at all.
            --     Get_Results (F.Background.all);
            --  end if;

            for Scen of F.Scenario_List loop
               Get_Results (Scen);
            end loop;

         end loop;
      end loop;

      New_Line;
      Put_Line ("------------------------------------------------");
      Put_Line ("- Failed     tests = " & Test_Result_Counts (Failed)'Image);
      Put_Line ("- Successful tests = " & Test_Result_Counts (Successful)'Image);
      Put_Line ("- Empty      tests = " & Test_Result_Counts (Empty)'Image);

   end Put_Run_Summary;

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type) is
   begin
      To_Scen.Failed_Step_Count     := @ + From_Scen.Failed_Step_Count;
      To_Scen.Successful_Step_Count := @ + From_Scen.Successful_Step_Count;
      From_Scen.Failed_Step_Count     := 0;
      From_Scen.Successful_Step_Count := 0;
   end Move_Results;

end BBT.Documents;
