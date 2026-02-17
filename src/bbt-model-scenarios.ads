-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Steps,
     Ada.Containers.Indefinite_Vectors;

use  BBT.Model.Steps;

package BBT.Model.Scenarios is

   type Scenario_Type;
   type Scenario_Access is not null access all Scenario_Type;
   type Scenario_Maybe  is          access all Scenario_Type;

   -- --------------------------------------------------------------------------
   type Scenario_Type (Is_Background : Boolean) is new Non_Root_Node with record
      Step_List             : Steps.List        := Step_Lists.Empty_Vector;
      Has_Run               : Boolean           := False;
      Failed_Step_Count     : Natural           := 0;
      Successful_Step_Count : Natural           := 0;
      Cmd_List              : Cmd_Lists.Vector  := Cmd_Lists.Empty_Vector;
      -- Cmd_List is filled in the parser, when an `or` keyword is detected.
      -- Then, in the the builder, the scenario is expanded in one scenario
      -- by Cmd, and the Cmd_List is emptied.
      -- Each scenario will be identical, except for the run command, that will
      -- be in Object_String, one by Cmd_List item.
      Cmd_List_Step_Index   : Step_Lists.Cursor := Step_Lists.No_Element;
      -- Store the index in Step_List where the cmd_list was found
   end record;

   -- --------------------------------------------------------------------------
   function Create_Scenario
     (Name          : String;
      Parent        : Node_Access;
      Location      : Location_Type;
      Is_Background : Boolean := False) return Scenario_Type;

   -- --------------------------------------------------------------------------
   function Is_In_Feature
     (Scen : Scenario_Type) return Boolean;
   function Has_Cmd_List
     (Scen : Scenario_Type) return Boolean;
   procedure Add_Result
     (Success : Boolean; To : in out Scenario_Type);
   function Result (Scenario : Scenario_Type'Class)
                    return Test_Result;
   function Get_Results
      (Scen : Scenario_Type) return Test_Results_Count;

   procedure Unfilter_Tree
     (Scen : in out Scenario_Type);
   procedure Filter_Tree
     (Scen : in out Scenario_Type);
   -- Mark the scenario and all contained steps as filtered
   -- -------------------------------------------------------------------------
   procedure Set_Filter (Scen     : in out Scenario_Type'Class;
                         Filtered :        Boolean);
   overriding procedure Apply_Filters_To
     (Scen : in out Scenario_Type);
   procedure Add_Step
     (Scen : in out Scenario_Type;
      Step :        Step_Type'Class);
   function Last_Step
     (Scen : in out Scenario_Type) return Step_Maybe;

   -- --------------------------------------------------------------------------
   procedure Move_Results
     (From_Scen, To_Scen : in out Scenario_Type'Class);

   -- --------------------------------------------------------------------------
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type'Class);
   subtype List is Scenario_Lists.Vector;
   Empty_List : List renames Scenario_Lists.Empty_Vector;
   function Last_Scenario
     (Scen : in out Scenario_Lists.Vector) return Scenario_Maybe;

end BBT.Model.Scenarios;
