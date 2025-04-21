-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Steps,
     BBT.Tests.Filter_List;

use BBT.Model.Steps,
    BBT.Tests.Filter_List;

private with BBT.Model.Documents;

package body BBT.Model.Features is

   -- --------------------------------------------------------------------------
   function Last_Feature return Feature_Access is
     (Feature_Access (Documents.Current_Doc.Feature_List.Reference
      (Documents.Current_Doc.Feature_List.Last).Element));

   -- --------------------------------------------------------------------------
   function Background
     (F : in out Feature_Type) return Scenario_Maybe is
     (F.Background);

   -- -------------------------------------------------------------------------
   function Create_Feature
     (Name     : Unbounded_String;
      Parent   : Documents.Document_Access;
      Location : Location_Type) return Feature_Type
   is
     (Filtered      => <>,
      Location      => Location,
      Comment       => <>,
      Name          => Name,
      Parent        => Node_Access (Parent),
      Scenario_List => <>,
      Background    => <>);

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

   -- -------------------------------------------------------------------------
   procedure Set_Filter (F        : in out Feature_Type'Class;
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
   overriding function Has_Background (F : Feature_Type) return Boolean is
     (F.Background /= null and then not F.Background.Step_List.Is_Empty);

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
   overriding procedure Apply_Filters_To (F : in out Feature_Type) is
      Result : constant Filter_Result := Is_Filtered (+F.Name, Feature);

   begin
      Put_Debug_Line ("Apply_Filters_To feature '" & (+F.Name) & "'");
      case Result is
         when Filtered =>
            Put_Debug_Line ("Feature filtered : '" & (+F.Name) & "'");
            Filter_Tree (F);

         when Selected =>
            Put_Debug_Line ("Feature selected : '" & (+F.Name) & "'");
            Unfilter_Parents (F);
            Unfilter_Tree (F);

         when No_Match => null;
            Put_Debug_Line ("Feature ignored : '" & (+F.Name) & "'");
      end case;

      if F.Background /= null then
         Apply_Filters_To (F.Background.all);
      end if;

      for Scen of F.Scenario_List loop
         Apply_Filters_To (Scen);
      end loop;
   end Apply_Filters_To;

   -- -------------------------------------------------------------------------
   function Last_Feature
     (F : in out Feature_Lists.Vector) return Feature_Maybe
   is
     (Feature_Maybe (F.Reference (F.Last).Element));

end BBT.Model.Features;
