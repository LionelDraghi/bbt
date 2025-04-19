-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Tests.Filter_List,
     File_Utilities,
     Ada.Directories;

use BBT.Tests.Filter_List,
    Ada.Directories;

package body BBT.Documents is

   use Documents_Lists;

   The_Doc_List : aliased Vector := Empty_Vector;

   -- --------------------------------------------------------------------------
   function The_Tests_List return access Documents_Lists.Vector is
     (The_Doc_List'Access);

   --  function Current_Doc return access Document_Type is
   --    (The_Doc_List.Reference (The_Doc_List.Last).Element);

   -- --------------------------------------------------------------------------
   function Current_Doc return Document_Maybe is
     (Document_Maybe (The_Doc_List.Reference (The_Doc_List.Last).Element));

   -- --------------------------------------------------------------------------
   function Last_Feature return Feature_Maybe is
     (Feature_Maybe (Current_Doc.Feature_List.Reference
      (Current_Doc.Feature_List.Last).Element));

   -- --------------------------------------------------------------------------
   procedure Filter (N : in out Node'Class) is
   begin
      N.Filtered := True;
   end Filter;
   -- --------------------------------------------------------------------------
   procedure Unfilter (N : in out Node'Class) is
   begin
      N.Filtered := False;
   end Unfilter;

   -- --------------------------------------------------------------------------
   function Parent_Doc
     (P : Leaf_Node'Class) return not null access Document_Type
   is
     (P.Parent_Document);

   -- --------------------------------------------------------------------------
   function Create_Document
     (Name          : Unbounded_String;
      Location      : Location_Type;
      Comment       : Text    := Empty_Text) return Document_Type
   is
     (Name            => Name,
      Location        => Location,
      Comment         => Comment,
      others          => <>);

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
           then (", Filtered " & (+Step.Subject_String))
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
   function Create_Step (Cat              : Extended_Step_Categories;
                         Action           : Actions;
                         Step_String      : Unbounded_String;
                         Location         : Location_Type;
                         Comment          : Text;
                         Subject_String   : Unbounded_String;
                         Object_String    : Unbounded_String;
                         Object_File_Name : Unbounded_String;
                         File_Type        : Ada.Directories.File_Kind;
                         Executable_File  : Boolean;
                         Ignore_Order     : Boolean;
                         File_Content     : Text)
                         return Step_Type is
     (Filtered         => Filtered_By_Default, -- <>
      Parent_Document  => Current_Doc,         -- <>
      Cat              => Cat,
      Action           => Action,
      Step_String      => Step_String,
      Location         => Location,
      Comment          => Comment,
      Subject_String   => Subject_String,
      Object_String    => Object_String,
      Object_File_Name => Object_File_Name,
      File_Type        => File_Type,
      Executable_File  => Executable_File,
      Ignore_Order     => Ignore_Order,
      File_Content     => File_Content,
      Parent_Scenario  => <>);

   -- --------------------------------------------------------------------------
   procedure Set_Parent_Scenario (Step     : in out Step_Type'Class;
                                  Scenario : Scenario_Maybe) is
   begin
      if Scenario = null then
         Put_Error ("***** null Scenario in call to Set_Parent_Scenario *****");
      end if;
      Step.Parent_Scenario := Scenario;
   end Set_Parent_Scenario;

   -- --------------------------------------------------------------------------
   function Is_In_Feature (Scen : Scenario_Type) return Boolean is
     (Scen.Parent_Feature /= null);

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type'Class) is
   begin
      To_Scen.Failed_Step_Count       := @ + From_Scen.Failed_Step_Count;
      To_Scen.Successful_Step_Count   := @ + From_Scen.Successful_Step_Count;
      From_Scen.Failed_Step_Count     := 0;
      From_Scen.Successful_Step_Count := 0;
   end Move_Results;

   -- --------------------------------------------------------------------------
   procedure Set_Filter (S        : in out Step_Type'Class;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Step => " & (+S.Step_String)
                      & ", To => " & Filtered'Image);
      S.Filtered := Filtered;
   end Set_Filter;

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
   overriding procedure Apply_Filters_To (S : in out Step_Type) is
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

   -- --------------------------------------------------------------------------
   overriding procedure Apply_Filters_To (Scen : in out Scenario_Type) is
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

   -- -------------------------------------------------------------------------
   procedure Add_Step (Scen : in out Scenario_Type;
                       Step :        Step_Type'Class) is
   begin
      Scen.Step_List.Append (Step);
   end Add_Step;

   -- -------------------------------------------------------------------------
   function Last_Step
     (Scen : in out Scenario_Type) return Step_Maybe
   is
     (Last_Step (Scen.Step_List));

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
   procedure Unfilter_Parents (Scen : in out Scenario_Type) is
   begin
      Put_Debug_Line ("Select_Parents of scen """ & (+Scen.Name) & """");
      if Scen.Parent_Feature /= null then
         Unfilter (Scen.Parent_Feature.all);
         Unfilter_Parents (Scen.Parent_Feature.all);
      end if;
      if Scen.Parent_Document /= null then
         Unfilter (Scen.Parent_Document.all);
      end if;
   end Unfilter_Parents;

   -- -------------------------------------------------------------------------
   function Create_Scenario
     (Name           : String;
      Parent_Feature : Feature_Maybe  := null;
      Location       : Location_Type) return Scenario_Type
   is
     (Name            => To_Unbounded_String (Name),
      Parent_Document => Current_Doc,
      Parent_Feature  => Parent_Feature,
      Location        => Location,
      others          => <>);

   -- -------------------------------------------------------------------------
   function Create_Feature
     (Name     : Unbounded_String;
      Location : Location_Type) return Feature_Type
   is
     (Name            => Name,
      Parent_Document => Current_Doc,
      Location        => Location,
      others          => <>);

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
     (Scenario_Maybe (D.Scenario_List.Reference (D.Scenario_List.Last).Element));

   function Last_Scenario_In_Feature
     (D : in out Document_Type) return Scenario_Maybe is

   begin
      return Last_Scenario (Last_Feature (D.Feature_List).Scenario_List);
   end Last_Scenario_In_Feature;

   function Last_Feature
     (D : in out Document_Type) return Feature_Maybe
   is
     (Last_Feature (D.Feature_List));

   function Background
     (D : in out Document_Type) return Scenario_Maybe is

   begin
      return D.Background;
   end Background;

   function Background
     (F : in out Feature_Type) return Scenario_Maybe is
     (F.Background);

   function Last_Doc
     (D : in out Documents_Lists.Vector) return Document_Access
   is
     (Document_Access (D.Reference (D.Last).Element));

   function Last_Scenario
     (Scen : in out Scenario_Lists.Vector) return Scenario_Maybe
   is
     (Scenario_Maybe (Scen.Reference (Scen.Last).Element));

   function Last_Step
     (S : in out Step_Lists.Vector) return Step_Maybe
   is
     (Step_Maybe (S.Reference (S.Last).Element));

   -- --------------------------------------------------------------------------
   procedure Apply_Filters is
   begin
      for Doc of The_Doc_List loop
         Apply_Filters_To (Doc);
      end loop;
   end Apply_Filters;

end BBT.Documents;
