-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

package body BBT.Documents.Docs is

   -- --------------------------------------------------------------------------
   function Create_Doc
     (Name     : String;
      Location : Location_Type) return Document_Access is
     (new Document_Type'(Name     => To_Unbounded_String (Name),
                         Location => Location,
                         others   => <>));

   use type Scenarios.Scenario_Access;
   -- -------------------------------------------------------------------------
   function Has_Background (D : Document_Type) return Boolean is
     (D.Background /= null and then not D.Background.all.Step_List.Is_Empty);

   procedure Set_Filter (D        : in out Document_Type;
                         Filtered :        Boolean);

   -- -------------------------------------------------------------------------
   --  procedure Filter   (D : in out Document_Type) is
   --  begin
   --     D.Filtered := True;
   --  end Filter;
   --  procedure Unfilter (D : in out Document_Type) is
   --  begin
   --     D.Filtered := False;
   --  end Unfilter;
   procedure Unfilter_Tree (D : in out Document_Type) is
   begin
      Set_Filter (D, False);
   end Unfilter_Tree;
   procedure Filter_Tree (D : in out Document_Type) is
   begin
      Set_Filter (D, True);
   end Filter_Tree;
   procedure Unfilter_Parents (D : in out Document_Type) is
   begin
      BBT.IO.Put_Error ("Document have no parents to unfilter");
   end Unfilter_Parents;

   -- --------------------------------------------------------------------------
   function Enclosing_Doc
     (S : in out Steps.Step_Type) return not null access Document_Type is
   begin
      return (if S.Parent_Scenario.Parent_Feature /= null
              then S.Parent_Scenario.Parent_Feature.Parent_Document
              else S.Parent_Scenario.Parent_Document);
   end Enclosing_Doc;

   -- -------------------------------------------------------------------------
   procedure Set_Filter (D        : in out Document_Type;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Doc => " & (+D.Name)
                      & ", To => " & Filtered'Image);
      D.Filtered := Filtered;
      for Scen of D.Scenario_List loop
         Set_Filter (Scen, Filtered);
      end loop;
      for F of D.Feature_List loop
         Set_Filter (F, Filtered);
      end loop;
      if D.Background /= null then
         Set_Filter (D.Background.all, Filtered);
      end if;
   end Set_Filter;

   -- -------------------------------------------------------------------------
   function Parent_Doc (F : Feature_Type) return access Document_Type is
     (F.Parent_Document);

   -- --------------------------------------------------------------------------
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

   -- --------------------------------------------------------------------------
   function Parent_Doc (Scen : Scenario_Type) return access Document_Type is
     (if Scen.Parent_Feature /= null then Scen.Parent_Feature.Parent_Document
      else Scen.Parent_Document);

   function Last (L : in out List) return Scenarios.Scenario_Access is
     (Scenarios.Last (Last (L).Scenario_List));

   procedure Add (F : Features.Feature_Type; To_Doc : Document_Type) is
   begin
      To_Doc.Feature_List.Append (F);
   end Add;

end BBT.Documents.Docs;
