-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Documents.Scenarios is

   -- --------------------------------------------------------------------------
   function Parent_Doc
     (Scen : Scenario_Type) return access Docs.Document_Type is
     (Scen.Parent_Document);

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
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type) is
   begin
      To_Scen.Failed_Step_Count       := @ + From_Scen.Failed_Step_Count;
      To_Scen.Successful_Step_Count   := @ + From_Scen.Successful_Step_Count;
      From_Scen.Failed_Step_Count     := 0;
      From_Scen.Successful_Step_Count := 0;
   end Move_Results;

   -- -------------------------------------------------------------------------
   procedure Set_Filter (Scen     : in out Scenario_Type;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Scen => " & To_String (Scen.Name)
                      & ", To => " & Filtered'Image);
      Scen.Filtered := Filtered;
      for Step of Scen.Step_List loop
         Set_Filter (Step.all, Filtered);
      end loop;
   end Set_Filter;

   -- -------------------------------------------------------------------------
   procedure Filter (Scen : in out Scenario_Type) is
   begin
      Scen.Filtered := True;
   end Filter;
   -- --------------------------------------------------------------------------
   procedure Unfilter (Scen : in out Scenario_Type) is
   begin
      Scen.Filtered := False;
   end Unfilter;
   -- -------------------------------------------------------------------------
   procedure Filter_Tree (Scen : in out Scenario_Type) is
   begin
      Set_Filter (Scen, True);
   end Filter_Tree;
   -- --------------------------------------------------------------------------
   procedure Unfilter_Tree (Scen : in out Scenario_Type) is
   begin
      Set_Filter (Scen, False);
   end Unfilter_Tree;
   -- --------------------------------------------------------------------------
   procedure Unfilter_Parents (S : in out Scenario_Type) is
   begin
      Put_Debug_Line ("Select_Parents of scen """ & (+S.Name) & """");
      if S.Parent_Feature /= null then
         Unfilter (S.Parent_Feature.all);
         Unfilter_Parents (S.Parent_Feature.all);
      end if;
      if S.Parent_Document /= null then
         Unfilter (S.Parent_Document.all);
      end if;
   end Unfilter_Parents;

   function Create_Scenario
     (Name       : String;
      Parent_Doc : not null access Docs.Document_Type;
      Loc        : Location_Type) return Scenario_Type is
   begin
      return (Name            => To_Unbounded_String (Name),
              Parent_Document => Parent_Doc,
              Location        => Loc,
              Other           => <>)
   end Create_Scenario;

   function Create_Scenario
     (Name           : String;
      Parent_Feature : not null access Features.Feature_Type;
      Loc            : Location_Type) return Scenario_Type is
   begin
      return (Name           => To_Unbounded_String (Name),
              Parent_Feature => Parent_Feature,
              Location       => Loc,
              Other          => <>)
   end Create_Scenario;

end BBT.Documents.Scenarios;
