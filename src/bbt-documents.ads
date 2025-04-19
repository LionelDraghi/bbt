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

   type Step_Type;
   type Scenario_Type;
   type Feature_Type;
   type Document_Type;

   type Scenario_Access is not null access all Scenario_Type;
   type Feature_Access  is not null access all Feature_Type;
   type Document_Access is not null access all Document_Type;

   type Step_Maybe      is          access all Step_Type;
   type Scenario_Maybe  is          access all Scenario_Type;
   type Feature_Maybe   is          access all Feature_Type;
   type Document_Maybe  is          access all Document_Type;

   function Current_Doc return Document_Maybe; -- Fixme:
   function Last_Feature return Feature_Maybe;

   package Cmd_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   function Filtered_By_Default return Boolean is (Settings.Selection_Mode);
   -- If in Selection mode, then items are filtered by default, and selected
   -- items should be provided with --select.
   -- If in normal mode, nothing is filtered by default, and filtered items
   -- should be provided with --exclude.

   -- --------------------------------------------------------------------------
   type Node is abstract tagged record  -- Fixme: private
      Filtered        : Boolean        := Filtered_By_Default;
      Location        : Location_Type;
      Comment         : Text           := Empty_Text;
   end record;

   procedure Filter   (N : in out Node'Class);
   procedure Unfilter (N : in out Node'Class);
   -- Mark the item as filtered

   procedure Apply_Filters_To (N : in out Node) is abstract; 

   -- --------------------------------------------------------------------------
   type Leaf_Node is abstract new Node with record
      Parent_Document : Document_Maybe := Current_Doc; -- should not be null
      -- Name         : Unbounded_String;
   end record;

   function Parent_Doc
     (P : Leaf_Node'Class) return not null access Document_Type;

   -- --------------------------------------------------------------------------
   type Step_Type is new Leaf_Node with record
      Cat              : Extended_Step_Categories  := Unknown;
      Action           : Actions                   := None;
      Step_String      : Unbounded_String          := Null_Unbounded_String;
      Subject_String   : Unbounded_String          := Null_Unbounded_String;
      Object_String    : Unbounded_String          := Null_Unbounded_String;
      Object_File_Name : Unbounded_String          := Null_Unbounded_String;
      File_Type        : Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
      Executable_File  : Boolean                   := False;
      Ignore_Order     : Boolean                   := True;
      File_Content     : Text                      := Empty_Text;
      Parent_Scenario  : Scenario_Maybe; -- access Scenario_Type;
   end record with Put_Image => Put_Image;

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
                         return Step_Type;
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type);
   -- Put_Image returns the full image
   function Inline_Image (Step : Step_Type) return String;
   -- Short_Line_Image returns a single line image with main non null fields.
   function "+" (Name : Unbounded_String) return String is (To_String (Name));
   function "+" (Name : String) return Unbounded_String is
     (To_Unbounded_String (Name));
   procedure Unfilter_Parents (S : in out Step_Type);
   -- function Enclosing_Doc (S : in out Step_Type'class) return not null access Document_Type;
   procedure Set_Parent_Scenario (Step : in out Step_Type'Class;
                                  Scenario : Scenario_Maybe);
   overriding procedure Apply_Filters_To (S : in out Step_Type);

   -- --------------------------------------------------------------------------
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type'Class);
   function Last_Step
     (S : in out Step_Lists.Vector) return Step_Maybe;

   -- --------------------------------------------------------------------------
   type Scenario_Type is new Leaf_Node with record
      Name                  : Unbounded_String;
      Step_List             : aliased Step_Lists.Vector;
      Parent_Feature        : access Feature_Type;
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
   end record;

   -- --------------------------------------------------------------------------
   function Create_Scenario
     (Name           : String;
      Parent_Feature : Feature_Maybe   := null;
      Location       : Location_Type) return Scenario_Type;
     -- with pre => Parent_Feature /= null xor Parent_Doc /= null;

   function Is_In_Feature (Scen : Scenario_Type) return Boolean;
   use type Step_Lists.Cursor;
   function Has_Cmd_List (Scen : Scenario_Type) return Boolean is
     (Scen.Cmd_List_Step_Index /= Step_Lists.No_Element);
   procedure Add_Result  (Success : Boolean; To : in out Scenario_Type);
   procedure Unfilter_Tree (Scen : in out Scenario_Type);
   procedure Filter_Tree   (Scen : in out Scenario_Type);
   -- Mark the scenario and all contained steps as filtered
   procedure Unfilter_Parents (Scen : in out Scenario_Type);
   overriding procedure Apply_Filters_To (Scen : in out Scenario_Type);
   procedure Add_Step (Scen : in out Scenario_Type;
                       Step :        Step_Type'Class);
   function Last_Step
     (Scen : in out Scenario_Type) return Step_Maybe;

   -- --------------------------------------------------------------------------
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type'Class);

   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type'Class);
   function Last_Scenario
     (Scen : in out Scenario_Lists.Vector) return Scenario_Maybe;

   -- --------------------------------------------------------------------------
   type Feature_Type is new Leaf_Node with record
      Name          : Unbounded_String;
      Scenario_List : aliased Scenario_Lists.Vector;
      Background    : access Scenario_Type;
   end record;

   -- --------------------------------------------------------------------------
   function Create_Feature
     (Name     : Unbounded_String;
      Location : Location_Type) return Feature_Type;

   function Has_Background (F : Feature_Type) return Boolean is
     (F.Background /= null and then not F.Background.Step_List.Is_Empty);
   procedure Filter_Tree   (F : in out Feature_Type);
   procedure Unfilter_Tree (F : in out Feature_Type);
   -- Mark the feature and all contained scenarios and steps as filtered
   procedure Unfilter_Parents (F : in out Feature_Type);
   overriding procedure Apply_Filters_To (F : in out Feature_Type);
  -- function Last_Scenario (F : Feature_Type) return Scenario_Maybe;
   function Background
     (F : in out Feature_Type) return Scenario_Maybe;

   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type'Class);
   -- subtype List is Feature_Lists.Vector;
   function Last_Feature (F : in out Feature_Lists.Vector) return Feature_Maybe;
   -- return the last created Feature

   -- --------------------------------------------------------------------------
   type Document_Type is new Node with record
      Name          : Unbounded_String;
      Scenario_List : Scenario_Lists.Vector;
      Feature_List  : Feature_Lists.Vector;
      Background    : access Scenario_Type;
   end record;

   -- --------------------------------------------------------------------------
   function Create_Document
     (Name     : Unbounded_String;
      Location : Location_Type; -- only the file name, obviously
      Comment  : Text    := Empty_Text) return Document_Type;

   function Has_Background (D : Document_Type) return Boolean;
   function Output_File_Name (D : Document_Type) return String;
   procedure Filter_Tree   (D : in out Document_Type);
   procedure Unfilter_Tree (D : in out Document_Type);
   -- Mark the document all contained features, scenarios, etc.
   -- as filtered/unfiltered
   overriding procedure Apply_Filters_To (D : in out Document_Type); -- Fixme: should be private
   function Last_Scenario_In_Doc
     (D : in out Document_Type) return Scenario_Maybe;
   function Last_Scenario_In_Feature
     (D : in out Document_Type) return Scenario_Maybe;
   function Background
     (D : in out Document_Type) return Scenario_Maybe;
   -- function Last_Feature  (F : in out Document_Type) return Feature_Maybe;
   function Last_Feature
     (D : in out Document_Type) return Feature_Maybe;

   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type'Class);
   function Last_Doc (D : in out Documents_Lists.Vector) return Document_Access;

   -- --------------------------------------------------------------------------
   function The_Tests_List return access Documents_Lists.Vector;
   procedure Apply_Filters; -- Apply recursively on the whole tree

private
   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Model)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   function Has_Background (D : Document_Type) return Boolean is
     (D.Background /= null and then not D.Background.Step_List.Is_Empty);

end BBT.Documents;
