-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Features,
     BBT.Tests.Filter_List;

use BBT.Model.Features,
    BBT.Tests.Filter_List;


package body BBT.Model.Scenarios is

   -- --------------------------------------------------------------------------
   function Has_Cmd_List
     (Scen : Scenario_Type) return Boolean
   is
      use type Step_Lists.Cursor;
   begin
      return Scen.Cmd_List_Step_Index /= Step_Lists.No_Element;
   end Has_Cmd_List;

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
   function Result (Scenario : Scenario_Type'Class)
                    return Test_Result is
   begin
      if Scenario.Failed_Step_Count > 0 then
         return Failed;
      elsif Scenario.Successful_Step_Count > 0 then
         return Successful;
      elsif not Scenario.Has_Run then
         return Not_Run;
      else
         return Empty;
      end if;
   end Result;

   -- --------------------------------------------------------------------------
   function Get_Results
      (Scen : Scenario_Type) return Test_Results_Count is
   begin
      if Scen.Failed_Step_Count > 0 then
         return [Failed => 1, others => 0];
      elsif Scen.Successful_Step_Count > 0 then
         return [Successful => 1, others => 0];
      elsif not Scen.Has_Run then
         return [Not_Run => 1, others => 0];
      else
         return [Empty => 1, others => 0];
      end if;
   end Get_Results;

   -- --------------------------------------------------------------------------
   function Is_In_Feature (Scen : Scenario_Type) return Boolean is
     (Scen.Parent.all in Feature_Type);

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type'Class) is
   begin
      To_Scen.Failed_Step_Count       := @ + From_Scen.Failed_Step_Count;
      To_Scen.Successful_Step_Count   := @ + From_Scen.Successful_Step_Count;
      From_Scen.Failed_Step_Count     := 0;
      From_Scen.Successful_Step_Count := 0;
   end Move_Results;

   -- -------------------------------------------------------------------------
   procedure Set_Filter (Scen     : in out Scenario_Type'Class;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Scen => " & (+Scen.Name)
                      & ", To => " & Filtered'Image);
      Scen.Filtered := Filtered;
      for Step of Scen.Step_List loop
         Set_Filter (Step, Filtered);
      end loop;
   end Set_Filter;

   -- --------------------------------------------------------------------------
   overriding procedure Apply_Filters_To (Scen : in out Scenario_Type) is
      Result : constant Filter_Result := Is_Filtered (+Scen.Name, Scenario);

   begin
      Put_Debug_Line ("Apply_Filters_To scen '" & (+Scen.Name) & "'");
      case Result is
         when Filtered =>
            Put_Debug_Line ("Scenario filtered : '" & (+Scen.Name) & "'");
            Filter_Tree (Scen);

         when Selected =>
            Put_Debug_Line ("Scenario selected : '" & (+Scen.Name) & "'");
            Unfilter_Parents (Scen);
            Unfilter_Tree (Scen);
            -- If scenario is selected, we must mark the parent feature
            -- or document as selected, and possibly Background, etc.

         when No_Match => null;
            Put_Debug_Line ("Scenario ignored : '" & (+Scen.Name) & "'");

      end case;

      for S of Scen.Step_List loop
         Apply_Filters_To (S);
      end loop;
   end Apply_Filters_To;

   -- -------------------------------------------------------------------------
   procedure Add_Step (Scen : in out Scenario_Type;
                       Step :        Step_Type'Class) is
   begin
      Scen.Step_List.Append (Step);
   end Add_Step;

   -- -------------------------------------------------------------------------
   function Last_Step
     (Scen : in out Scenario_Type) return Step_Maybe
   is
     (Last_Step (Scen.Step_List));

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

   -- -------------------------------------------------------------------------
   function Create_Scenario
     (Name          : String;
      Parent        : Node_Access;
      Location      : Location_Type;
      Is_Background : Boolean := False) return Scenario_Type
   is
     (Is_Background         => Is_Background,
      Filtered              => <>,
      Location              => Location,
      Comment               => <>,
      Name                  => To_Unbounded_String (Name),
      Parent                => Parent,
      Step_List             => <>,
      Has_Run               => <>,
      Failed_Step_Count     => <>,
      Successful_Step_Count => <>,
      Cmd_List              => <>,
      Cmd_List_Step_Index   => <>);

   -- --------------------------------------------------------------------------
   function Last_Scenario
     (Scen : in out Scenario_Lists.Vector) return Scenario_Maybe
   is
     (Scenario_Maybe (Scen.Reference (Scen.Last).Element));

end BBT.Model.Scenarios;
