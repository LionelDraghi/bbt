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
            Unfilter_Parents (S);
            -- If a Step matches, we also need the enclosing scenario
            Unfilter (S);
            -- if One step is selected, we must mark the parent scenario as
            -- selected, and possibly Background, etc.

         when  Filtered =>
            Filter (S);

         when No_Match => null;
      end case;
   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (Scen : in out Scenario_Type) is
      Result : constant Filter_Result := Is_Filtered (+Scen.Name, Step);

   begin
      Put_Debug_Line ("Apply_Filters_To scen '" & (+Scen.Name) & "'");
      case Result is
         when Filtered =>
            Filter_Tree (Scen);

         when Selected =>
            Unfilter_Parents (Scen);
            Unfilter_Tree (Scen);
            -- if scenario is selected, we must mark the parent feature or
            -- document as selected, and possibly Background, etc.

         when No_Match => null;
      end case;

      for S of Scen.Step_List loop
         Apply_Filters_To (S);
      end loop;
   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (F : in out Feature_Type) is
      Result : constant Filter_Result := Is_Filtered (+F.Name, Step);

   begin
      Put_Debug_Line ("Apply_Filters_To feature '" & (+F.Name) & "'");
      case Result is
         when Filtered =>
            Filter_Tree (F);

         when Selected =>
            Unfilter_Parents (F);
            Unfilter_Tree (F);

         when No_Match => null;
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
         Result : constant Filter_Result := Is_Filtered (+D.Name, Step);

      begin
         Put_Debug_Line ("Apply_Filters_To doc : '" & (+D.Name) & "'");

         case Result is
            when Filtered =>
               Filter_Tree (D);

            when Selected =>
               Unfilter_Tree (D);

            when No_Match => null;
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
