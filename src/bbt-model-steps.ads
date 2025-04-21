-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors,
     Ada.Directories,
     Ada.Strings.Text_Buffers;

limited with BBT.Model.Scenarios;

package BBT.Model.Steps is

-- --------------------------------------------------------------------------
   type Extended_Step_Categories is
     (Unknown,
      Given_Step, -- Precond, -- Given
      When_Step,  -- Action,  -- When
      Then_Step); -- Checks); -- Then
   subtype Step_Categories is Extended_Step_Categories range
     Extended_Step_Categories'Succ (Unknown) ..
     Extended_Step_Categories'Last;

   type Actions is
     (None,
      Check_File_Existence,
      Check_Dir_Existence,
      Check_No_File,
      Check_No_Dir,
      Create_If_None,
      Erase_And_Create,
      Setup_No_File,
      Setup_No_Dir,
      Error_Return_Code,
      File_Contains,
      File_Does_Not_Contain,
      File_Is,
      File_Is_Not,
      No_Error_Return_Code,
      Output_Contains,
      Output_Matches,
      Output_Does_Not_Contain,
      Output_Does_Not_Match,
      Output_Is,
      No_Output,
      Run_Cmd,
      Run_Without_Error);

   type Step_Type;
   type Step_Maybe      is          access all Step_Type;

   package Cmd_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   subtype Cmd_List is Cmd_Lists.Vector;
   Empty_Cmd_List : Cmd_Lists.Vector := Cmd_Lists.Empty_Vector;

   function Filtered_By_Default return Boolean is (Settings.Selection_Mode);
   -- If in Selection mode, then items are filtered by default, and selected
   -- items should be provided with --select.
   -- If in normal mode, nothing is filtered by default, and filtered items
   -- should be provided with --exclude.

   -- --------------------------------------------------------------------------
   type Step_Data is record
      Cat              : Extended_Step_Categories  := Unknown;
      Action           : Actions                   := None;
      Src_Code         : Unbounded_String          := Null_Unbounded_String;
      Subject_String   : Unbounded_String          := Null_Unbounded_String;
      Object_String    : Unbounded_String          := Null_Unbounded_String;
      Object_File_Name : Unbounded_String          := Null_Unbounded_String;
      File_Type        : Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
      Executable_File  : Boolean                   := False;
      Ignore_Order     : Boolean                   := True;
      File_Content     : Text                      := Empty_Text;
   end record with Put_Image => Put_Image;

   -- --------------------------------------------------------------------------
   type Step_Type is new Non_Root_Node with record
      Data : Step_Data;
   end record with Put_Image => Put_Image;

   --  -- --------------------------------------------------------------------------
   --  function Create_Step (Cat              : Extended_Step_Categories;
   --                        Action           : Actions;
   --                        Src_Code         : Unbounded_String;
   --                        Location         : Location_Type;
   --                        Comment          : Text;
   --                        Subject_String   : Unbounded_String;
   --                        Object_String    : Unbounded_String;
   --                        Object_File_Name : Unbounded_String;
   --                        File_Type        : Ada.Directories.File_Kind;
   --                        Executable_File  : Boolean;
   --                        Ignore_Order     : Boolean;
   --                        File_Content     : Text)
   --                        return Step_Type;

   -- --------------------------------------------------------------------------
   function Create_Step (Info            : Step_Data;
                         Loc             : Location_Type;
                         Parent_Scenario : Scenarios.Scenario_Access)
                         return Step_Type;
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type);
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Data);
   -- Put_Image returns the full image

   function Inline_Image
     (Step : Step_Type) return String;
   -- Short_Line_Image returns a single line image with main non null fields.

   overriding procedure Apply_Filters_To
     (S : in out Step_Type);

   function Parent
     (S : Step_Type) return Scenarios.Scenario_Access;
   -- A Step cannot be created outside of an existing scenario,
   -- this access should never be null.

   -- --------------------------------------------------------------------------
   procedure Set_Filter (S        : in out Step_Type'Class;
                         Filtered :        Boolean);

   -- --------------------------------------------------------------------------
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type'Class);
   subtype List is Step_Lists.Vector;
   Empty_Step_List : List renames Step_Lists.Empty_Vector;

   function Last_Step
     (S : in out Step_Lists.Vector) return Step_Maybe;

end BBT.Model.Steps;
