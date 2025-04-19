-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors,
     Text_Utilities,
     Ada.Strings.Unbounded;

use Text_Utilities,
    Ada.Strings.Unbounded;

with BBT.Documents.Scenarios;

limited with BBT.Documents.Docs;

package BBT.Documents.Features is

   -- --------------------------------------------------------------------------
   type Feature_Type is new Node with private;
   procedure Filter   (F : in out Feature_Type);
   procedure Unfilter (F : in out Feature_Type);
   procedure Filter_Tree   (F : in out Feature_Type);
   procedure Unfilter_Tree (F : in out Feature_Type);
   -- Mark the feature and all contained scenarios and steps as filtered
   procedure Unfilter_Parents (F : in out Feature_Type);
   function Name (F : Feature_Type) return String;

   type Feature_Access is access all Feature_Type;
   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Access);
   subtype List is Feature_Lists.Vector;
   subtype Reference_Type is Feature_Lists.Reference_Type;
   function Last (L : in out List) return Feature_Access;
   -- return the last created Feature
   function Last_Scenario (F : Feature_Type) return Scenarios.Scenario_Access;
   -- return the last created Scenario

   function Parent_Doc (F : Feature_Type) return not null access
     Documents.Docs.Document_Type;
   function Scenario_List (F : Feature_Type) return Scenarios.List;
   function Has_Background (F : Feature_Type) return Boolean;
   function Background (F : Feature_Type) return access Scenarios.Scenario_Type
     with Precondition => Has_Background (F);

   function Create_Feature
     (Name            : Unbounded_String;
      Parent_Document : not null access Docs.Document_Type;
      Location        : Location_Type) return Feature_Type;

   procedure Add_Background (Scen       : Scenarios.Scenario_Type;
                             To_Feature : Feature_Type);


private
   type Feature_Type is new Node with record
      Name            : Unbounded_String;
      Scenario_List   : Scenarios.List;
      Background      : access Scenarios.Scenario_Type;
      Parent_Document : not null access Docs.Document_Type;
   end record;

   function Last (L : in out List) return Feature_Access is
     (L.Reference (L.Last).Element.all);

   function Name -- Fixme à Factoriser
     (F : Feature_Type) return String is (To_String (F.Name));
   function Scenario_List
     (F : Feature_Type) return Scenarios.List is (F.Scenario_List);
   function Background
     (F : Feature_Type) return access Scenarios.Scenario_Type is (F.Background);

end BBT.Documents.Features;
