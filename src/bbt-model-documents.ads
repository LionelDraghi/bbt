-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors,
     BBT.Model.Features,
     BBT.Model.Scenarios;

use BBT.Model.Features,
    BBT.Model.Scenarios;

package BBT.Model.Documents is

   -- --------------------------------------------------------------------------
   type Document_Type;
   type Document_Access is not null access all Document_Type;

   function Current_Doc return Document_Access;

   -- --------------------------------------------------------------------------
   type Document_Type is new Root_Node with record
      Scenario_List : Scenarios.List       := Scenario_Lists.Empty_Vector;
      Feature_List  : Features.List        := Feature_Lists.Empty_Vector;
      Background    : access Scenario_Type := null;
   end record;

   -- --------------------------------------------------------------------------
   function Create_Document
     (Name     : Unbounded_String;
      Location : Location_Type; -- only the file name, obviously
      Comment  : Text := Empty_Text) return Document_Type;

   function Parent_Doc
     (P : Non_Root_Node'Class) return Document_Access;
   overriding function Has_Background
     (D : Document_Type) return Boolean;
   function Output_File_Name
     (D : Document_Type) return String;
   procedure Filter_Tree
     (D : in out Document_Type);
   procedure Unfilter_Tree
     (D : in out Document_Type);
   -- Mark the document all contained features, scenarios, etc.
   -- as filtered/unfiltered
   overriding procedure Apply_Filters_To
     (D : in out Document_Type); -- Fixme: should be private
   function Last_Scenario_In_Doc
     (D : in out Document_Type) return Scenario_Maybe;
   function Last_Scenario_In_Feature
     (D : in out Document_Type) return Scenario_Maybe;
   function Background
     (D : in out Document_Type) return Scenario_Maybe;
   function Last_Feature
     (D : in out Document_Type) return Feature_Maybe;
   function Get_Results (D : Document_Type) return Test_Results_Count;

   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type'Class);
   subtype List is Documents_Lists.Vector;

   -- --------------------------------------------------------------------------
   function Last_Doc
     (D : in out Documents_Lists.Vector) -- Fixme: should be type List
      return Document_Access;

   function Doc_List return access Documents_Lists.Vector;

   -- --------------------------------------------------------------------------
   function Count (Test : Test_Result) return Natural;
   -- Walk through the Document list to sum all scenarios result's.
   function Success return Boolean;
   function No_Fail return Boolean;
   -- Success ensure that there is successfully run tests, and
   -- no empty/error/not run tests
   -- No_Fail **just** check that there is no fail test.
   procedure Sum_Results (Docs : List);
   function Get_Results (DL : List) return Test_Results_Count;
   -- -----------------------------------------------------------------------
   subtype Count_String is String (1 .. 7);
   Blank_Image : constant Count_String := [others => ' '];
   function Count_String_Image (Test : Test_Result) return Count_String;
   -- Warning, image is cut if it does not fit

   procedure Apply_Filters; -- Apply recursively on the whole tree

   procedure Generate_Badge;

end BBT.Model.Documents;
