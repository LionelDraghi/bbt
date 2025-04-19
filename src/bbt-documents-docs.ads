-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Documents.Features,
     BBT.Documents.Scenarios,
     BBT.Documents.Steps;

with Text_Utilities,
     Ada.Containers.Indefinite_Vectors,
     Ada.Strings.Unbounded;

use Text_Utilities,
    Ada.Strings.Unbounded;

package BBT.Documents.Docs is

   -- --------------------------------------------------------------------------
   type Document_Type is new Node with private;
   function Has_Background (D : Document_Type) return Boolean;
   function Output_File_Name (D : Document_Type) return String;
   --  procedure Filter   (D : in out Document_Type);
   --  procedure Unfilter (D : in out Document_Type);
   procedure Filter_Tree   (D : in out Document_Type);
   procedure Unfilter_Tree (D : in out Document_Type);
   procedure Unfilter_Parents (D : in out Document_Type);
   -- Mark the document contained features, scenarios, etc.
   -- as filtered/unfiltered
   function Name (D : Document_Type) return String;
   function Scenario_List (D : Document_Type) return Scenarios.List;
   function Feature_List (D : Document_Type) return Features.List;
   function Background (D : Document_Type) return Scenarios.Scenario_Access;
   function Comment (D : in out Document_Type) return not null access Text;

   function Enclosing_Doc
     (S : in out Steps.Step_Type) return not null access Document_Type;

   type Document_Access is access all Document_Type;
   function Create_Doc
     (Name     : String;
      Location : Location_Type) return Document_Access;

   package Document_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Access);
   subtype List is Document_Lists.Vector;
   subtype Reference_Type is Document_Lists.Reference_Type;
   function Last_Doc (L : in out List) return Document_Access;
   -- return the last created Doc
   function Last_Feature (L : in out List) return Features.Feature_Access;
   -- return the last created Feature in the last created Doc
   function Last_Scenario (L : in out List) return Scenarios.Scenario_Access;
   -- return the last created Scenario directly attached to the Doc

   procedure Add
     (F : Features.Feature_Type; To_Doc : Document_Type);
   procedure Add_To_Doc (Scen   : Scenarios.Scenario_Type;
                         To_Doc : Document_Type);
   procedure Add_To_Feature (Scen   : Scenarios.Scenario_Type;
                             To_Doc : Document_Type);
   procedure Add_Background (Scen   : Scenarios.Scenario_Type;
                             To_Doc : Document_Type);

private
   type Document_Type is new Node with record
      Name          : Unbounded_String;
      Scenario_List : aliased Scenarios.List;
      Feature_List  : aliased Features.List;
      Background    : aliased Scenarios.Scenario_Access;
   end record;
   function Name
     (D : Document_Type) return String is (To_String (D.Name));
   function Scenario_List
     (D : Document_Type) return Scenarios.List is (D.Scenario_List);
   function Feature_List
     (D : Document_Type) return Features.List is (D.Feature_List);
   function Background
     (D : Document_Type) return Scenarios.Scenario_Access is (D.Background);

   function Last_Doc (L : in out List) return Document_Access is
     (L.Reference (L.Last).Element.all);
   function Last_Feature (L : in out List) return Features.Feature_Access is
     (Features.Last (Last_Doc (L).Feature_List));
   function Last_Scenario (L : in out List) return Scenarios.Scenario_Access is
     (Scenarios.Last_Scen (Last_Doc (L).Scenario_List));

end BBT.Documents.Docs;
