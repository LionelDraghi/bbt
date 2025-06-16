-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Model.Scenarios,
     BBT.Tests.Filter_List,
     Ada.Directories;

use BBT.Model.Scenarios,
    BBT.Tests.Filter_List,
    Ada.Directories;

package body BBT.Model.Steps is

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type) is
   begin
      --  Output.New_Line;
      --  Output.Put ("   Location         = " & Image (S.Location));
      Output.New_Line;
      Put_Image (Output, S.Data);
   end Put_Image;

   -- --------------------------------------------------------------------------
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Data) is
   begin
      Output.Put ("   Step type        = " & S.Cat'Image & ", ");
      Output.New_Line;
      Output.Put ("   Action           = " & S.Action'Image);
      Output.New_Line;
      Output.Put ("   Src_Code         = " & S.Src_Code'Image);
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
      Output.Put ("   File_Content     = " & Code_Fenced_Image (S.File_Content));
      Output.New_Line;
   end Put_Image;

   -- --------------------------------------------------------------------------
   function Inline_Image (Step : Step_Type) return String is
   begin
      return
        ("'" & (+Step.Data.Src_Code) & "', Action = " & Step.Data.Action'Image
         & (if Step.Data.Subject_String /= Null_Unbounded_String
           then (", Subject parameter = """ & (+Step.Data.Subject_String) & """")
           else "")
         & (if Step.Filtered
           then (", Filtered " & (+Step.Data.Subject_String))
           else "")
         & (if Step.Data.Object_String /= Null_Unbounded_String
           then (", Object parameter = """ & (+Step.Data.Object_String) & """")
           else "")
         & (if Step.Data.Object_File_Name /= Null_Unbounded_String
           then (if Step.Data.File_Type = Directory
             then ", dir = """ & (+Step.Data.Object_File_Name) & """"
             else ", File = """ & (+Step.Data.Object_File_Name) & """")
           else "")
         & (if Step.Data.Executable_File
           then ", attrib = exec"
           else "")
         & (if Step.Data.Ignore_Order
           then ", Ignore_Order = True"
           else "")
         & (if Is_Empty (Step.Data.File_Content)
           then ""
           else (", File content = """ & (Step.Data.File_Content'Image) & """")));
   end Inline_Image;

   -- --------------------------------------------------------------------------
   function Create_Step (Info            : Step_Data;
                         Loc             : Location_Type;
                         Parent_Scenario : Scenarios.Scenario_Access)
                         return Step_Type
   is (Filtered => <>,
       Location => Loc,
       Comment  => <>,
       Name     => To_Unbounded_String ("step"),
       Data     => Info,
       Parent   => Node_Access (Parent_Scenario));

   -- --------------------------------------------------------------------------
   procedure Set_Filter (S        : in out Step_Type'Class;
                         Filtered :        Boolean) is
   begin
      Put_Debug_Line ("Set_Filter (Step => " & (+S.Data.Src_Code)
                      & ", To => " & Filtered'Image);
      S.Filtered := Filtered;
   end Set_Filter;

   -- --------------------------------------------------------------------------
   procedure Set_Has_Syntax_Error (S         : in out Step_Type'Class;
                                   Has_Error :        Boolean) is
   begin
      Put_Debug_Line ("Set_Has_Syntax_Error (Step => " & (+S.Data.Src_Code)
                      & ", To => " & Has_Error'Image);
      S.Data.Syntax_Error := Has_Error;
   end Set_Has_Syntax_Error;

   -- --------------------------------------------------------------------------
   function Has_Syntax_Error (S : in out Step_Type'Class) return Boolean is
      (S.Data.Syntax_Error);

   -- --------------------------------------------------------------------------
   overriding procedure Apply_Filters_To (S : in out Step_Type) is
      Result : constant Filter_Result := Is_Filtered (+S.Data.Src_Code, Step);
   begin
      Put_Debug_Line ("Apply_Filters_To step '" & (+S.Data.Src_Code) & "'");
      case Result is
         when Selected =>
            Put_Debug_Line ("Step selected : '" & (+S.Data.Src_Code) & "'");
            Unfilter_Parents (S);
            Unfilter (S);
            -- if One step is selected, we must mark the parent scenario
            -- as selected, and possibly Background, etc.

         when  Filtered =>
            Put_Debug_Line ("Step filtered : '" & (+S.Data.Src_Code) & "'");
            Filter (S);

         when No_Match => null;
            Put_Debug_Line ("Step ignored : '" & (+S.Data.Src_Code) & "'");

      end case;
   end Apply_Filters_To;

   -- -------------------------------------------------------------------------
   function Parent (S : Step_Type) return Scenarios.Scenario_Access is
     (Scenario_Access (S.Parent));

   -- --------------------------------------------------------------------------
   function Last_Step
     (S : in out Step_Lists.Vector) return Step_Maybe
   is
     (Step_Maybe (S.Reference (S.Last).Element));

end BBT.Model.Steps;
