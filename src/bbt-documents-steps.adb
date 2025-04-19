-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Directories; use Ada.Directories;

package body BBT.Documents.Steps is

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
           then (", Filtered" & To_String (Step.Subject_String))
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
   function Create_Step
     (Cat              : Extended_Step_Categories;
      Action           : Actions;
      Step_String      : Step_Strings;
      Location         : IO.Location_Type;
      Comment          : Text;
      Subject_String   : Unbounded_String;
      Object_String    : Unbounded_String;
      Object_File_Name : Unbounded_String;
      File_Type        : Ada.Directories.File_Kind;
      Executable_File  : Boolean;
      Ignore_Order     : Boolean) return Step_Type is
   begin
      return (Cat => Cat,
              Action           => Action,
              Step_String      => Step_String,
              Location         => Location,
              Comment          => Comment,
              Subject_String   => Subject_String,
              Object_String    => Object_String,
              Object_File_Name => Object_File_Name,
              File_Type        => File_Type,
              Executable_File  => Executable_File,
              Ignore_Order     => Ignore_Order);
   end Create_Step;

   function Parent_Scenario
     (S : Step_Type) return access Scenarios.Scenario_Type is
     (S.Parent_Scenario);

end BBT.Documents.Steps;
