-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
with BBT.Tests.Builder;
with File_Utilities;

with Ada.Directories; use Ada.Directories;

package body BBT.Documents is

   --  -- --------------------------------------------------------------------------
   --  procedure Put_Image
   --    (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
   --     S      :        Object_Text_Type) is
   --  begin
   --     Output.Put ("Text source = " & S.Source'Image & " : ");
   --     case S.Source is
   --        when Code_Span =>
   --           Output.Put (S.Object_String'Image);
   --        when Code_Block =>
   --           Output.New_Line;
   --           Output.Increase_Indent;
   --           Output.Put (S.File_Content'Image);
   --           Output.Decrease_Indent;
   --        when File =>
   --           Output.Put (S.File_Type'Image & S.File_Name'Image);
   --     end case;
   --     Output.New_Line;
   --  end Put_Image;

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

   function Short_Line_Image (Step : Step_Type) return String is
   begin
      return
        ("Action = " & Step.Action'Image
         & (if Step.Subject_String /= Null_Unbounded_String
           then (", Subject = """ & (+Step.Subject_String) & """")
           else "")
         & (if Step.Object_String /= Null_Unbounded_String
           then (", Object = """ & (+Step.Object_String) & """")
           else "")
         & (if Step.Object_File_Name /= Null_Unbounded_String
           then (if Step.File_Type = Directory
             then ", dir = """ & (+Step.Object_File_Name) & """"
             else ", File = """ & (+Step.Object_File_Name) & """")
           else "")
         & (if Step.Ignore_Order
           then ", Ignore_Order = True"
           else "")
         & (if Is_Empty (Step.File_Content)
           then ""
           else (", File content = """ & (Step.File_Content'Image) & """")));
   end Short_Line_Image;

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
   procedure Put_Text (The_Text : Text) is
   begin
      for L of The_Text loop
         Put_Line (L);
      end loop;
   end Put_Text;

   -- --------------------------------------------------------------------------
   procedure Put_Step (Step : Step_Type) is
   begin
      Put_Line (Line (Step.Location)'Image & ": Step """ &
                (+Step.Step_String) & """");
      Put_Line (Step'Image);
   end Put_Step;

   -- --------------------------------------------------------------------------
   procedure Put_Scenario (Scenario : Scenario_Type) is
   begin
      Put_Line (Line (Scenario.Location)'Image & ": Scenario """ &
                  To_String ((Scenario.Name)) & """");
      for Step of Scenario.Step_List loop
         Put_Step (Step);
      end loop;
      -- New_Line;
   end Put_Scenario;

   function Parent_Doc (Scen : Scenario_Type) return access Document_Type is
     (if Scen.Parent_Feature /= null then Scen.Parent_Feature.Parent_Document
      else Scen.Parent_Document);

   function Is_In_Feature (Scen : Scenario_Type) return Boolean is
     (Scen.Parent_Feature /= null);

   -- --------------------------------------------------------------------------
   procedure Put_Feature (Feature : Feature_Type) is
      -- Pref : constant String := "## ";
   begin
      -- Current_Indent_Level := 1;
      Put_Line (Line (Feature.Location)'Image & ": Feature """ &
                  To_String (Feature.Name) & """");

      if Feature.Background /= null then
         Put_Scenario (Feature.Background.all);
      end if;

      for Scenario of Feature.Scenario_List loop
         Put_Scenario (Scenario);
      end loop;
      New_Line;
   end Put_Feature;

   -- --------------------------------------------------------------------------
   procedure Put_Document (Doc : Document_Type) is
   begin
      Put_Line ("Document Name : " & To_String (Doc.Name));
      New_Line;
      if Doc.Background /= null then
       Put_Scenario (Doc.Background.all);
      end if;

      for S of Doc.Scenario_List loop
         Put_Scenario (S);
      end loop;

      for Feature of Doc.Feature_List loop
         Put_Feature (Feature);
      end loop;
   end Put_Document;

   -- --------------------------------------------------------------------------
   procedure Put_Document_List (Doc_List : Documents_Lists.Vector) is
   begin
      for Doc of Doc_List loop
         Put_Document (Doc);
      end loop;
   end Put_Document_List;

   -- --------------------------------------------------------------------------
   function Result (Scenario : Scenario_Type) return Test_Result is
   begin
      if Scenario.Failed_Step_Count > 0 then
         return Failed;
      elsif Scenario.Successful_Step_Count > 0 then
         return Successful;
      elsif not Scenario.Has_Run then
         return Not_Run;
      else
         return Empty;
      end if;
   end Result;

   Results : Test_Results_Count;

   -- --------------------------------------------------------------------------
   procedure Compute_Overall_Tests_Results is
      procedure Get_Results (S : Scenario_Type) is
      begin
         Results (Result (S)) := @ + 1;
      end Get_Results;
   begin
      for D of BBT.Tests.Builder.The_Tests_List.all loop

         if D.Feature_List.Is_Empty and D.Scenario_List.Is_Empty then
            -- Empty Doc should be reported
            Results (Empty) := @ + 1;
         end if;

         for Scen of D.Scenario_List loop
            Get_Results (Scen);
         end loop;

         for F of D.Feature_List loop
            if F.Scenario_List.Is_Empty then
               -- Empty Feature should be reported
               Results (Empty) := @ + 1;
            end if;

            for Scen of F.Scenario_List loop
               Get_Results (Scen);
            end loop;

         end loop;
      end loop;
   end Compute_Overall_Tests_Results;

   -- --------------------------------------------------------------------------
   function Overall_Results return Test_Results_Count is (Results);

   -- --------------------------------------------------------------------------
   procedure Put_Overall_Results is
   begin
      New_Line;
      Put_Line ("------------------");
      Put_Line ("- Failed     = " & Results (Failed)'Image);
      Put_Line ("- Successful = " & Results (Successful)'Image);
      Put_Line ("- Empty      = " & Results (Empty)'Image);
      if Results (Failed) /= 0 then
         Put_Line ("- Not run    = " & Results (Not_Run)'Image);
      end if;
   end Put_Overall_Results;

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type) is
   begin
      To_Scen.Failed_Step_Count     := @ + From_Scen.Failed_Step_Count;
      To_Scen.Successful_Step_Count := @ + From_Scen.Successful_Step_Count;
      From_Scen.Failed_Step_Count     := 0;
      From_Scen.Successful_Step_Count := 0;
   end Move_Results;

end BBT.Documents;
