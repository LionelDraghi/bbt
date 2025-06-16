-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Tests.Filter_List,
     File_Utilities,
     Ada.Directories;

use BBT.Tests.Filter_List;

package body BBT.Model.Documents is

   use Documents_Lists;
   The_Doc_List : aliased Vector := Empty_Vector;

   -- --------------------------------------------------------------------------
   function Doc_List return access Documents_Lists.Vector is
     (The_Doc_List'Access);

   -- --------------------------------------------------------------------------
   function Current_Doc return Document_Access is
     (Document_Access (The_Doc_List.Reference (The_Doc_List.Last).Element));

   -- --------------------------------------------------------------------------
   function Has_Background (D : Document_Type) return Boolean is
     (D.Background /= null and then not D.Background.Step_List.Is_Empty);

   -- --------------------------------------------------------------------------
   function Parent_Doc
     (P : Non_Root_Node'Class) return Document_Access
   is
   begin
      if P.Parent.all in Document_Type then
         -- This is the root Document
         return Document_Access (P.Parent);
      else
         -- Let's go one more level up
         return Parent_Doc (Non_Root_Node'Class (P.Parent.all));
      end if;
   end Parent_Doc;

   -- --------------------------------------------------------------------------
   function Create_Document
     (Name          : Unbounded_String;
      Location      : Location_Type;
      Comment       : Text    := Empty_Text) return Document_Type
   is
     (Filtered        => <>,
      Location        => Location,
      Comment         => Comment,
      Name            => Name,
      Scenario_List   => <>,
      Feature_List    => <>,
      Background      => <>);

   -- -------------------------------------------------------------------------
   procedure Set_Filter (D        : in out Document_Type;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Doc => " & (+D.Name)
                      & ", To => " & Filtered'Image);
      D.Filtered := Filtered;
      for Scen of D.Scenario_List loop
         Scenarios.Set_Filter (Scen, Filtered);
      end loop;
      for F of D.Feature_List loop
         Features.Set_Filter (F, Filtered);
      end loop;
      if D.Background /= null then
         Scenarios.Set_Filter (D.Background.all, Filtered);
      end if;
   end Set_Filter;

   -- -------------------------------------------------------------------------
   procedure Unfilter_Tree (D : in out Document_Type) is
   begin
      Set_Filter (D, False);
   end Unfilter_Tree;
   procedure Filter_Tree (D : in out Document_Type) is
   begin
      Set_Filter (D, True);
   end Filter_Tree;

   -- --------------------------------------------------------------------------
   overriding procedure Apply_Filters_To (D : in out Document_Type) is
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

   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   function Last_Scenario_In_Doc
     (D : in out Document_Type) return Scenario_Maybe is
     (if D.Scenario_List.Is_Empty then null
      else
         Scenario_Maybe
        (D.Scenario_List.Reference (D.Scenario_List.Last).Element));

   -- --------------------------------------------------------------------------
   function Last_Scenario_In_Feature
     (D : in out Document_Type) return Scenario_Maybe is
     (Last_Scenario (Last_Feature (D.Feature_List).Scenario_List));

   -- --------------------------------------------------------------------------
   function Last_Feature
     (D : in out Document_Type) return Feature_Maybe
   is
     (Last_Feature (D.Feature_List));

   -- --------------------------------------------------------------------------
   function Background
     (D : in out Document_Type) return Scenario_Maybe is
     (D.Background);

   -- --------------------------------------------------------------------------
   function Last_Doc
     (D : in out Documents_Lists.Vector) return Document_Access
   is
     (Document_Access (D.Reference (D.Last).Element));

   -- --------------------------------------------------------------------------
   procedure Apply_Filters is
   begin
      for Doc of The_Doc_List loop
         Apply_Filters_To (Doc);
      end loop;
   end Apply_Filters;

   --  -- --------------------------------------------------------------------------
   function Output_File_Name (D : Document_Type) return String is
      use BBT.Settings,
          File_Utilities;
   begin
      if Output_File_Dir (Output_File_Dir'Last) = Separator then
         return Output_File_Dir &
           Ada.Directories.Simple_Name (To_String (D.Name)) & ".out";
      else
         return Output_File_Dir & Separator &
           Ada.Directories.Simple_Name (To_String (D.Name)) & ".out";
      end if;
   end Output_File_Name;

end BBT.Model.Documents;
