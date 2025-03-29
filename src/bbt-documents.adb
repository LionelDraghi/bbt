-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with File_Utilities;

with Ada.Directories; use Ada.Directories;

package body BBT.Documents is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Documents)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type) is
   begin
      Output.Put ("   Step type        = " & S.Cat'Image & ", ");
      Output.New_Line;
      Output.Put ("   Action           = " & S.Action'Image);
      Output.New_Line;
      Output.Put ("   Step string      = " & S.Step_String'Image);
      Output.New_Line;
      Output.Put ("   Location         = " & Image (S.Location));
      Output.New_Line;
      Output.Put ("   Subject string   = " & S.Subject_String'Image);
      Output.New_Line;
      Output.Put ("   Object_String    = " & S.Object_String'Image);
      Output.New_Line;
      Output.Put ("   Object_File_Name = " & S.Object_File_Name'Image);
      Output.New_Line;
      Output.Put ("   File_Type        = " & S.File_Type'Image);
      Output.New_Line;
      Output.Put ("   Ignore order     = " & S.Ignore_Order'Image);
      Output.New_Line;
      Output.Put ("   File_Content     = " & Text_Image (S.File_Content));
      Output.New_Line;
   end Put_Image;

   -- --------------------------------------------------------------------------
   function Inline_Image (Step : Step_Type) return String is
   begin
      return
        ("'" & (+Step.Step_String) & "', Action = " & Step.Action'Image
         & (if Step.Subject_String /= Null_Unbounded_String
           then (", Subject = """ & (+Step.Subject_String) & """")
           else "")
         & (if Step.Filtered
           then (", FILTERED" & (+Step.Subject_String))
           else "")
         & (if Step.Object_String /= Null_Unbounded_String
           then (", Object = """ & (+Step.Object_String) & """")
           else "")
         & (if Step.Object_File_Name /= Null_Unbounded_String
           then (if Step.File_Type = Directory
             then ", dir = """ & (+Step.Object_File_Name) & """"
             else ", File = """ & (+Step.Object_File_Name) & """")
           else "")
         & (if Step.Executable_File
           then ", attrib = exec"
           else "")
         & (if Step.Ignore_Order
           then ", Ignore_Order = True"
           else "")
         & (if Is_Empty (Step.File_Content)
           then ""
           else (", File content = """ & (Step.File_Content'Image) & """")));
   end Inline_Image;

   -- --------------------------------------------------------------------------
   procedure Add_Result (Success : Boolean; To : in out Scenario_Type) is
   begin
      if Success then
         To.Successful_Step_Count := @ + 1;
      else
         To.Failed_Step_Count := @ + 1;
      end if;
   end Add_Result;

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

   -- --------------------------------------------------------------------------
   function Is_In_Feature (Scen : Scenario_Type) return Boolean is
     (Scen.Parent_Feature /= null);

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type) is
   begin
      To_Scen.Failed_Step_Count       := @ + From_Scen.Failed_Step_Count;
      To_Scen.Successful_Step_Count   := @ + From_Scen.Successful_Step_Count;
      From_Scen.Failed_Step_Count     := 0;
      From_Scen.Successful_Step_Count := 0;
   end Move_Results;

   -- --------------------------------------------------------------------------
   procedure Set_Filter (S        : in out Step_Type;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Step => " & (+S.Step_String)
                      & ", To => " & Filtered'Image);
      S.Filtered := Filtered;
   end Set_Filter;

   -- --------------------------------------------------------------------------
   procedure Filter (S : in out Step_Type) is
   begin
      Set_Filter (S, True);
   end Filter;
   -- --------------------------------------------------------------------------
   procedure Unfilter (S : in out Step_Type) is
   begin
      Set_Filter (S, False);
   end Unfilter;
   -- --------------------------------------------------------------------------
   procedure Unfilter_Parents (S : in out Step_Type) is
   begin
      Put_Debug_Line ("Select_Parents of step """ & (+S.Step_String) & """");
      if S.Parent_Scenario /= null then
         Unfilter (S.Parent_Scenario.all);
         Unfilter_Parents (S.Parent_Scenario.all);
      end if;
   end Unfilter_Parents;

   -- --------------------------------------------------------------------------
   function Enclosing_Doc (S : in out Step_Type) return not null access Document_Type is
   begin
      return (if S.Parent_Scenario.Parent_Feature /= null
              then S.Parent_Scenario.Parent_Feature.Parent_Document
              else S.Parent_Scenario.Parent_Document);
   end Enclosing_Doc;

   -- -------------------------------------------------------------------------
   procedure Set_Filter (Scen     : in out Scenario_Type;
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
   procedure Filter (Scen : in out Scenario_Type) is
   begin
      Scen.Filtered := True;
   end Filter;
   -- --------------------------------------------------------------------------
   procedure Unfilter (Scen : in out Scenario_Type) is
   begin
      Scen.Filtered := False;
   end Unfilter;
   -- -------------------------------------------------------------------------
   procedure Filter_Tree (Scen : in out Scenario_Type) is
   begin
      Set_Filter (Scen, True);
   end Filter_Tree;
   -- --------------------------------------------------------------------------
   procedure Unfilter_Tree (Scen : in out Scenario_Type) is
   begin
      Set_Filter (Scen, False);
   end Unfilter_Tree;
   -- --------------------------------------------------------------------------
   procedure Unfilter_Parents (S : in out Scenario_Type) is
   begin
      Put_Debug_Line ("Select_Parents of scen """ & (+S.Name) & """");
      if S.Parent_Feature /= null then
         Unfilter (S.Parent_Feature.all);
         Unfilter_Parents (S.Parent_Feature.all);
      end if;
      if S.Parent_Document /= null then
         Unfilter (S.Parent_Document.all);
      end if;
   end Unfilter_Parents;


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
      if F.Parent_Document /= null then
         Unfilter (F.Parent_Document.all);
      end if;
   end Unfilter_Parents;


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
   procedure Filter   (D : in out Document_Type) is
   begin
      D.Filtered := True;
   end Filter;
   procedure Unfilter (D : in out Document_Type) is
   begin
      D.Filtered := False;
   end Unfilter;
   procedure Unfilter_Tree (D : in out Document_Type) is
   begin
      Set_Filter (D, False);
   end Unfilter_Tree;
   procedure Filter_Tree (D : in out Document_Type) is
   begin
      Set_Filter (D, True);
   end Filter_Tree;

end BBT.Documents;
