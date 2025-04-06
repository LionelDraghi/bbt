-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO,
     BBT.Settings,
     Text_Utilities;

use BBT.IO,
    Text_Utilities;

with Ada.Containers.Indefinite_Vectors,
     Ada.Directories,
     Ada.Strings.Text_Buffers,
     Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

private package BBT.Documents is
-- Defines the main bbt internal data structure, wich is essentialy a tree of
-- Documents containing Features containing Scenario containing steps.
-- This structure is a a simplified subset of Gerkhin AST :
-- https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast
--
-- The various type of step are also defined here, and this is bbt's own
-- vocabulary.

   -- --------------------------------------------------------------------------
   type Extended_Step_Categories is (Unknown,
                                     Given_Step, -- Precond, -- Given
                                     When_Step,  -- Action,  -- When
                                     Then_Step); -- Checks); -- Then
   subtype Step_Categories is Extended_Step_Categories range
     Extended_Step_Categories'Succ (Unknown) .. Extended_Step_Categories'Last;

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

   type Scenario_Type;
   type Feature_Type;
   type Document_Type;

   package Cmd_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   function Filtered_By_Default return Boolean is (Settings.Selection_Mode);
   -- If in Selection mode, then items are filtered by default, and selected
   -- items should be provided with --select.
   -- If in normal mode, nothing is filtered by default, and filtered items
   -- should be provided with --exclude.

   -- --------------------------------------------------------------------------
   type Step_Type is record
      Cat              : Extended_Step_Categories  := Unknown;
      Action           : Actions                   := None;
      Step_String      : Unbounded_String          := Null_Unbounded_String;
      Location         : Location_Type;
      Comment          : Text                      := Empty_Text;
      Subject_String   : Unbounded_String          := Null_Unbounded_String;
      Object_String    : Unbounded_String          := Null_Unbounded_String;
      Object_File_Name : Unbounded_String          := Null_Unbounded_String;
      File_Type        : Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
      Executable_File  : Boolean                   := False;
      Ignore_Order     : Boolean                   := True;
      File_Content     : Text                      := Empty_Text;
      Filtered         : Boolean                   := Filtered_By_Default;
      Parent_Scenario  : access Scenario_Type;
   end record with Put_Image => Put_Image;
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type);
   -- Put_Image returns the full image
   function Inline_Image (Step : Step_Type) return String;
   -- Short_Line_Image returns a single line image with main non null fields.
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type);
   function "+" (Name : Unbounded_String) return String is (To_String (Name));
   function "+" (Name : String) return Unbounded_String is
     (To_Unbounded_String (Name));
   procedure Filter   (S : in out Step_Type);
   procedure Unfilter (S : in out Step_Type);
   -- Mark the Step as filtered
   procedure Unfilter_Parents (S : in out Step_Type);
   function Enclosing_Doc (S : in out Step_Type) return not null access Document_Type;

   -- --------------------------------------------------------------------------
   type Scenario_Type is record
      Name                  : Unbounded_String;
      Location              : Location_Type;
      Comment               : Text    := Empty_Text;
      Step_List             : aliased Step_Lists.Vector;
      Parent_Feature        : access Feature_Type;
      Parent_Document       : access Document_Type;
      Has_Run               : Boolean := False;
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
      Cmd_List              : Cmd_Lists.Vector;
      -- Cmd_List is filled in the parser, when an `or` keyword is detected.
      -- Then, in the the builder, the scenario is expanded in one scenario
      -- by Cmd, and the Cmd_List is emptied.
      -- Each scenario will be identical, except for the run command, that will
      -- be in Object_String, one by Cmd_List item.
      Cmd_List_Step_Index   : Step_Lists.Cursor;
      -- store the index in Step_List where the cmd_list was found
      Filtered              : Boolean := Filtered_By_Default;
      -- If Filtered = True, then it should not being run
   end record;
   -- with Type_Invariant => Parent_Feature /= null xor Parent_Document /= null;
   function Parent_Doc (Scen : Scenario_Type) return access Document_Type;
   function Is_In_Feature (Scen : Scenario_Type) return Boolean;
   use type Step_Lists.Cursor;
   function Has_Cmd_List (Scen : Scenario_Type) return Boolean is
     (Scen.Cmd_List_Step_Index /= Step_Lists.No_Element);
   procedure Add_Result  (Success : Boolean; To : in out Scenario_Type);
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type);
   procedure Unfilter (Scen : in out Scenario_Type);
   procedure Filter   (Scen : in out Scenario_Type);
   procedure Unfilter_Tree (Scen : in out Scenario_Type);
   procedure Filter_Tree   (Scen : in out Scenario_Type);
   -- Mark the scenario and all contained steps as filtered
   procedure Unfilter_Parents (S : in out Scenario_Type);

   -- --------------------------------------------------------------------------
   type Feature_Type is record
      Name            : Unbounded_String;
      Location        : Location_Type;
      Comment         : Text    := Empty_Text;
      Scenario_List   : Scenario_Lists.Vector;
      Background      : access Scenario_Type;
      Parent_Document : access Document_Type;
      Filtered        : Boolean := Filtered_By_Default;
      -- If Filtered = True, then it should not being run
   end record;
   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type);
   function Has_Background (F : Feature_Type) return Boolean is
     (F.Background /= null and then not F.Background.Step_List.Is_Empty);
   procedure Filter   (F : in out Feature_Type);
   procedure Unfilter (F : in out Feature_Type);
   procedure Filter_Tree   (F : in out Feature_Type);
   procedure Unfilter_Tree (F : in out Feature_Type);
   -- Mark the feature and all contained scenarios and steps as filtered
   procedure Unfilter_Parents (F : in out Feature_Type);

   -- --------------------------------------------------------------------------
   type Document_Type is record
      Name          : Unbounded_String;
      Location      : Location_Type; -- only the file name, obviously
      Comment       : Text    := Empty_Text;
      Scenario_List : Scenario_Lists.Vector;
      Feature_List  : Feature_Lists.Vector;
      Background    : access Scenario_Type;
      Filtered      : Boolean := Filtered_By_Default;
      -- If Filtered = True, then it should not being run
   end record;
   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type);
   function Has_Background (D : Document_Type) return Boolean is
     (D.Background /= null and then not D.Background.Step_List.Is_Empty);
   function Output_File_Name (D : Document_Type) return String;
   procedure Filter   (D : in out Document_Type);
   procedure Unfilter (D : in out Document_Type);
   procedure Filter_Tree   (D : in out Document_Type);
   procedure Unfilter_Tree (D : in out Document_Type);
   -- Mark the document all contained features, scenarios, etc.
   -- as filtered/unfiltered

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type);

end BBT.Documents;
