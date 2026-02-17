-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Scenarios,
     Ada.Containers.Indefinite_Vectors;

use  BBT.Model.Scenarios;

limited with BBT.Model.Documents;

package BBT.Model.Features is

   type Feature_Type;
   type Feature_Access  is not null access all Feature_Type;
   type Feature_Maybe   is          access all Feature_Type;

   function Last_Feature return Feature_Access;

   -- --------------------------------------------------------------------------
   type Feature_Type is new Non_Root_Node with record
      Scenario_List : aliased Scenarios.List;
      Background    : access  Scenario_Type;
   end record;

   -- --------------------------------------------------------------------------
   function Create_Feature
     (Name     : Unbounded_String;
      Parent   : Documents.Document_Access;
      Location : Location_Type) return Feature_Type;
   overriding function Has_Background
     (F : Feature_Type) return Boolean;
   procedure Filter_Tree
     (F : in out Feature_Type);
   procedure Unfilter_Tree
     (F : in out Feature_Type);
   -- Mark the feature and all contained scenarios and steps as filtered
   overriding procedure Apply_Filters_To
     (F : in out Feature_Type);
   function Background
     (F : in out Feature_Type) return Scenario_Maybe;
   function Get_Results
      (F : Feature_Type) return Test_Results_Count;

   -- -------------------------------------------------------------------------
   procedure Set_Filter (F        : in out Feature_Type'Class;
                         Filtered :        Boolean);

   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type'Class);
   subtype List is Feature_Lists.Vector;
   Empty_List : List renames Feature_Lists.Empty_Vector;

   function Last_Feature (F : in out Feature_Lists.Vector) return Feature_Maybe;
   -- return the last created Feature

end BBT.Model.Features;
