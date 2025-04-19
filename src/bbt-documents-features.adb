-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Documents.Features is

   -- --------------------------------------------------------------------------
   function Parent_Doc (F : Feature_Type) return not null access
     Documents.Docs.Document_Type is
       (F.Parent_Document);

   -- --------------------------------------------------------------------------
   function Is_In_Feature (Scen : Scenario_Type) return Boolean is
     (Scen.Parent_Feature /= null);

   -- -------------------------------------------------------------------------
   procedure Set_Filter (F        : in out Feature_Type;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Feature => " & (+F.Name)
                      & ", To => " & Filtered'Image);
      F.Filtered := Filtered;
      for Scen of F.Scenario_List loop
         Set_Filter (Scen, Filtered);
      end loop;
      if F.Background /= null then
         Set_Filter (F.Background.all, Filtered);
      end if;
   end Set_Filter;

   -- -------------------------------------------------------------------------
   procedure Filter (F : in out Feature_Type) is
   begin
      Put_Debug_Line ("Set_Filter (Feature => " & (+F.Name)
                      & ", To => True");
      F.Filtered := True;
   end Filter;
   -- --------------------------------------------------------------------------
   procedure Unfilter (F : in out Feature_Type) is
   begin
      Put_Debug_Line ("Set_Filter (Feature => " & (+F.Name)
                      & ", To => False");
      F.Filtered := False;
   end Unfilter;
   -- -------------------------------------------------------------------------
   procedure Filter_Tree (F : in out Feature_Type) is
   begin
      Set_Filter (F, True);
   end Filter_Tree;
   -- --------------------------------------------------------------------------
   procedure Unfilter_Tree (F : in out Feature_Type) is
   begin
      Set_Filter (F, False);
   end Unfilter_Tree;
   -- --------------------------------------------------------------------------
   procedure Unfilter_Parents (F : in out Feature_Type) is
   begin
      Put_Debug_Line ("Select_Parents of feature """ & (+F.Name)  & """");
      if F.Background /= null then
         Unfilter (F.Background.all);
      end if;
      if Parent_Doc (F) /= null then
         Unfilter (F.Parent_Document.all);
      end if;
   end Unfilter_Parents;

   use all type Steps.Step_Lists;
   function Has_Background (F : Feature_Type) return Boolean is
     (F.Background /= null and then not F.Background.Step_List.Is_Empty);

   function Last_Scenario (L : in out List) return Scenarios.Scenario_Access is
      F : Feature_Access := Last (L);
   begin
      return (Scenarios.Last (F);
              end Last_Scenario;

              function Create_Feature
                (Name            : Unbounded_String;
                 Parent_Document : not null access Docs.Document_Type;
                 Location        : Location_Type) return Feature_Type is
              begin
              end;

              end BBT.Documents.Features;
