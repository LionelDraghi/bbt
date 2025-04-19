-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Text_Utilities,
     Ada.Containers.Indefinite_Vectors,
     Ada.Strings.Unbounded;

use Text_Utilities,
    Ada.Strings.Unbounded;

with BBT.Documents.Steps;

limited with BBT.Documents.Docs,
     BBT.Documents.Features;

package BBT.Documents.Scenarios is

   -- --------------------------------------------------------------------------
   type Scenario_Type is new Node with private;
   function Parent_Doc (Scen : Scenario_Type) return access Docs.Document_Type;
--   function Is_In_Feature (Scen : Scenario_Type) return Boolean;
   -- function Has_Cmd_List  (Scen : Scenario_Type) return Boolean;
   procedure Add_Result   (Success : Boolean;
                           To      : in out Scenario_Type);
   procedure Unfilter (Scen : in out Scenario_Type);
   procedure Filter   (Scen : in out Scenario_Type);
   procedure Unfilter_Tree (Scen : in out Scenario_Type);
   procedure Filter_Tree   (Scen : in out Scenario_Type);
   -- Mark the scenario and all contained steps as filtered
   procedure Unfilter_Parents (S : in out Scenario_Type);
   function Name (S : Scenario_Type) return String;
   function Has_Run (S : Scenario_Type) return Boolean;
   function Successful_Step_Count (S : Scenario_Type) return Natural;
   function Failed_Step_Count (S : Scenario_Type) return Natural;

   use type BBT.Documents.Steps.List;
   function Step_List (S : Scenario_Type) return not null access Steps.List;
   procedure Add_Step (To_Scen : Scenario_Type;
                       S       : Steps.Step_Type);
   function Last_Step (S : Scenario_Type) return Steps.Step_Type;

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type);

   type Scenario_Access is access all Scenario_Type;
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Access);
   subtype List is Scenario_Lists.Vector;
   subtype Reference_Type is Scenario_Lists.Reference_Type;
   function Last_Scen (L : in out List) return Scenario_Access;
   -- return the last added scenario

   function Create_Scenario
     (Name       : String;
      Parent_Doc : not null access Docs.Document_Type;
      Location        : Location_Type) return Scenario_Type;
   function Create_Scenario
     (Name           : String;
      Parent_Feature : not null access Features.Feature_Type;
      Location            : Location_Type) return Scenario_Type;

private
   type Scenario_Type is new Node with record
      Name                  : Unbounded_String;
      Step_List             : access Steps.List;
      Parent_Feature        : access Features.Feature_Type;
      Parent_Document       : access Docs.Document_Type;
      Has_Run               : Boolean := False;
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
      Cmd_List              : Steps.Cmd_Lists.Vector;
      -- Cmd_List is filled in the parser, when an `or` keyword is detected.
      -- Then, in the the builder, the scenario is expanded in one scenario
      -- by Cmd, and the Cmd_List is emptied.
      -- Each scenario will be identical, except for the run command, that will
      -- be in Object_String, one by Cmd_List item.
      Cmd_List_Step_Index   : Steps.Step_Lists.Cursor;
      -- store the index in Step_List where the cmd_list was found
   end record
     with Invariant => (Parent_Feature /= null) xor (Parent_Document /= null);
   -- The scenario declared in a Feature or directly in a Document,
   -- but can't be in both or none.


   function Last (L : in out List) return Scenario_Access is
     (L.Reference (L.Last).Element.all);

   use all type Steps.Step_Lists.Cursor;
   --  function Has_Cmd_List (Scen : Scenario_Type) return Boolean is
   --    (Scen.Cmd_List_Step_Index /= Steps.Step_Lists.No_Element);

   function Name (S : Scenario_Type) return String is
      (To_String (S.Name));
   function Step_List (S : Scenario_Type) return not null access Steps.List is
      (S.Step_List);
   function Has_Run (S : Scenario_Type) return Boolean is
     (S.Has_Run);
   function Successful_Step_Count (S : Scenario_Type) return Natural is
     (S.Successful_Step_Count);
   function Failed_Step_Count (S : Scenario_Type) return Natural is
     (S.Failed_Step_Count);




end BBT.Documents.Scenarios;
