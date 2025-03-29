-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Tests.Filter_List;

use BBT.Tests.Filter_List;

procedure BBT.Documents.Apply_Filters
  (To_Docs : access Documents_Lists.Vector) is

-- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Documents)
                             renames BBT.IO.Put_Line;


   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (S : in out Step_Type) is
      Result : constant Filter_Result := Is_Filtered (+S.Step_String, Step);
   begin
      Put_Debug_Line ("Apply_Filters_To step '" & (+S.Step_String) & "'");
      case Result is
         when Selected =>
            Put_Debug_Line ("Step selected : '" & (+S.Step_String) & "'");
            Unfilter_Parents (S);
            -- If a Step matches, we also need the enclosing scenario
            Unfilter (S);
            -- if One step is selected, we must mark the parent scenario as
            -- selected, and possibly Background, etc.

         when  Filtered =>
            Put_Debug_Line ("Step filtered : '" & (+S.Step_String) & "'");
            Filter (S);

         when No_Match => null;
            Put_Debug_Line ("Step ignored : '" & (+S.Step_String) & "'");

      end case;
   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (Scen : in out Scenario_Type) is
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
            -- if scenario is selected, we must mark the parent feature or
            -- document as selected, and possibly Background, etc.

         when No_Match => null;
            Put_Debug_Line ("Scenario ignored : '" & (+Scen.Name) & "'");

      end case;

      for S of Scen.Step_List loop
         Apply_Filters_To (S);
      end loop;
   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (F : in out Feature_Type) is
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

begin
   -- --------------------------------------------------------------------------
   for D of To_Docs.all loop
      declare
         Result : constant Filter_Result := Is_Filtered (+D.Name, Doc_Name);

      begin
         Put_Debug_Line ("Apply_Filters_To doc : '" & (+D.Name) & "'");

         case Result is
            when Filtered =>
               Put_Debug_Line ("Doc filtered : '" & (+D.Name) & "'");
               Filter_Tree (D);

            when Selected =>
               Put_Debug_Line ("Doc selected : '" & (+D.Name) & "'");
               Unfilter_Tree (D);

            when No_Match => null;
               Put_Debug_Line ("Doc ignored : '" & (+D.Name) & "'");

         end case;

         if D.Background /= null then
            Apply_Filters_To (D.Background.all);
         end if;

         for S of D.Scenario_List loop
            Apply_Filters_To (S);
         end loop;

         for F of D.Feature_List loop
            Apply_Filters_To (F);
         end loop;
      end;
   end loop;

end BBT.Documents.Apply_Filters;
