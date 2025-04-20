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
-- Defines the main bbt internal data structure, wich is essentialy a tree
-- of Documents containing Features containing Scenario containing steps.
-- This structure is a a simplified subset of Gerkhin AST :
-- https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast
--
-- The various type of step are also defined here, and this is bbt's own
-- vocabulary.

   function "+" (Name : Unbounded_String) return String is
     (To_String (Name));
   function "+" (Name : String) return Unbounded_String is
     (To_Unbounded_String (Name));

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
   type Scenario_Type;
   type Feature_Type;
   type Document_Type;

   type Scenario_Access is not null access all Scenario_Type;
   type Feature_Access  is not null access all Feature_Type;
   type Document_Access is not null access all Document_Type;

   type Step_Maybe      is          access all Step_Type;
   type Scenario_Maybe  is          access all Scenario_Type;
   type Feature_Maybe   is          access all Feature_Type;

   function Current_Doc  return Document_Access;
   function Last_Feature return Feature_Access;

   package Cmd_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   function Filtered_By_Default return Boolean is (Settings.Selection_Mode);
   -- If in Selection mode, then items are filtered by default, and selected
   -- items should be provided with --select.
   -- If in normal mode, nothing is filtered by default, and filtered items
   -- should be provided with --exclude.

   -- --------------------------------------------------------------------------
   type Root_Node is abstract tagged record
      Filtered : Boolean       := Filtered_By_Default;
      Location : Location_Type := No_Location;
      Comment  : Text          := Empty_Text;
      Name     : Unbounded_String := Null_Unbounded_String;
   end record;

   type Node_Access is not null access all Root_Node'Class;

   procedure Filter
     (N : in out Root_Node);
   procedure Unfilter
     (N : in out Root_Node);
   -- Mark the item as filtered
   procedure Apply_Filters_To
     (N : in out Root_Node) is abstract;
   function Has_Background
     (D : Root_Node) return Boolean is (False);

   -- --------------------------------------------------------------------------
   type Non_Root_Node is abstract new Root_Node with record
      Parent : aliased Node_Access; -- There is always a parent node
   end record;

   procedure Unfilter_Parents
     (N : in out Non_Root_Node'Class);
   function Parent_Doc
     (P : Non_Root_Node'Class) return Document_Access;

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
                         Parent_Scenario : Scenario_Access)
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
     (S : Step_Type) return Scenario_Access;
   -- A Step cannot be created outside of an existing scenario,
   -- this access should never be null.

   -- --------------------------------------------------------------------------
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type'Class);
   function Last_Step
     (S : in out Step_Lists.Vector) return Step_Maybe;

   -- --------------------------------------------------------------------------
   type Scenario_Type is new Non_Root_Node with record
      Step_List             : Step_Lists.Vector := Step_Lists.Empty_Vector;
      Has_Run               : Boolean           := False;
      Failed_Step_Count     : Natural           := 0;
      Successful_Step_Count : Natural           := 0;
      Cmd_List              : Cmd_Lists.Vector  := Cmd_Lists.Empty_Vector;
      -- Cmd_List is filled in the parser, when an `or` keyword is detected.
      -- Then, in the the builder, the scenario is expanded in one scenario
      -- by Cmd, and the Cmd_List is emptied.
      -- Each scenario will be identical, except for the run command, that will
      -- be in Object_String, one by Cmd_List item.
      Cmd_List_Step_Index   : Step_Lists.Cursor := Step_Lists.No_Element;
      -- Store the index in Step_List where the cmd_list was found
   end record;

   -- --------------------------------------------------------------------------
   function Create_Scenario
     (Name     : String;
      Parent   : Node_Access;
      Location : Location_Type) return Scenario_Type;

   -- --------------------------------------------------------------------------
   function Is_In_Feature
     (Scen : Scenario_Type) return Boolean;
   function Has_Cmd_List
     (Scen : Scenario_Type) return Boolean;
   procedure Add_Result
     (Success : Boolean; To : in out Scenario_Type);
   procedure Unfilter_Tree
     (Scen : in out Scenario_Type);
   procedure Filter_Tree
     (Scen : in out Scenario_Type);
   -- Mark the scenario and all contained steps as filtered
   overriding procedure Apply_Filters_To
     (Scen : in out Scenario_Type);
   procedure Add_Step
     (Scen : in out Scenario_Type;
      Step :        Step_Type'Class);
   function Last_Step
     (Scen : in out Scenario_Type) return Step_Maybe;

   -- --------------------------------------------------------------------------
   procedure Move_Results
     (From_Scen, To_Scen : in out Scenario_Type'Class);

   -- --------------------------------------------------------------------------
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type'Class);
   function Last_Scenario
     (Scen : in out Scenario_Lists.Vector) return Scenario_Maybe;

   -- --------------------------------------------------------------------------
   type Feature_Type is new Non_Root_Node with record
      Scenario_List : aliased Scenario_Lists.Vector;
      Background    : access Scenario_Type;
   end record;

   -- --------------------------------------------------------------------------
   function Create_Feature
     (Name     : Unbounded_String;
      Parent   : Document_Access;
      Location : Location_Type) return Feature_Type;
   overriding function Has_Background
     (F : Feature_Type) return Boolean;
   procedure Filter_Tree
     (F : in out Feature_Type);
   procedure Unfilter_Tree
     (F : in out Feature_Type);
   -- Mark the feature and all contained scenarios and steps as filtered
   overriding procedure Apply_Filters_To
     (F : in out Feature_Type);
   function Background
     (F : in out Feature_Type) return Scenario_Maybe;

   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type'Class);
   -- subtype List is Feature_Lists.Vector;
   function Last_Feature (F : in out Feature_Lists.Vector) return Feature_Maybe;
   -- return the last created Feature

   -- --------------------------------------------------------------------------
   type Document_Type is new Root_Node with record
      Scenario_List : Scenario_Lists.Vector := Scenario_Lists.Empty_Vector;
      Feature_List  : Feature_Lists.Vector  := Feature_Lists.Empty_Vector;
      Background    : access Scenario_Type  := null;
   end record;

   -- --------------------------------------------------------------------------
   function Create_Document
     (Name     : Unbounded_String;
      Location : Location_Type; -- only the file name, obviously
      Comment  : Text := Empty_Text) return Document_Type;

   overriding function Has_Background
     (D : Document_Type) return Boolean;
   function Output_File_Name
     (D : Document_Type) return String;
   procedure Filter_Tree
     (D : in out Document_Type);
   procedure Unfilter_Tree
     (D : in out Document_Type);
   -- Mark the document all contained features, scenarios, etc.
   -- as filtered/unfiltered
   overriding procedure Apply_Filters_To
     (D : in out Document_Type); -- Fixme: should be private
   function Last_Scenario_In_Doc
     (D : in out Document_Type) return Scenario_Maybe;
   function Last_Scenario_In_Feature
     (D : in out Document_Type) return Scenario_Maybe;
   function Background
     (D : in out Document_Type) return Scenario_Maybe;
   function Last_Feature
     (D : in out Document_Type) return Feature_Maybe;

   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type'Class);
   function Last_Doc
     (D : in out Documents_Lists.Vector)
      return Document_Access;

   -- --------------------------------------------------------------------------
   function The_Tests_List return access Documents_Lists.Vector;

   procedure Apply_Filters; -- Apply recursively on the whole tree

private
   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line
     (Item      : String;
      Location  : Location_Type    := No_Location;
      Verbosity : Verbosity_Levels := Debug;
      Topic     : Extended_Topics  := IO.Model)
      renames BBT.IO.Put_Line;

end BBT.Documents;
