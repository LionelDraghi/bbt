-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author : Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.IO;         use BBT.IO;
with Text_Utilities; use Text_Utilities;

with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Strings.Text_Buffers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private package BBT.Documents is
-- Defines the main bbt internal data structure, wich is essentialy a tree of
-- Documents containing Features containing Scenario containing steps.
-- This structure is a a simplified subset of Gerkhin AST :
-- https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast
--
-- The various type of step are also defined here, and this is bbt's own
-- vocabulary.

   -- --------------------------------------------------------------------------
   type Test_Result is (Failed, Empty, Successful);

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
      Create_File,
      Create_Directory,
      Erase_And_Create,
      Setup_No_File,
      Setup_No_Dir,
      Error_Return_Code,
      File_Contains,
      File_Does_Not_Contain,
      File_Is,
      No_Error_Return_Code,
      Output_Contains,
      Output_Does_Not_Contain,
      Output_Is,
      No_Output,
      Run_Cmd,
      Run_Without_Error);

   type Scenario_Type;
   type Feature_Type;
   type Document_Type;

   package Cmd_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   -- --------------------------------------------------------------------------
   type Step_Type is record
      Cat             : Extended_Step_Categories  := Unknown;
      Action          : Actions                   := None;
      Step_String     : Unbounded_String          := Null_Unbounded_String;
      Location        : Location_Type;
      Subject_String  : Unbounded_String          := Null_Unbounded_String;
      Object_String   : Unbounded_String          := Null_Unbounded_String;
      File_Type       : Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
      Ignore_Order    : Boolean                   := True;
      File_Content    : Text                      := Empty_Text;
      Parent_Scenario : access Scenario_Type;
   end record with Put_Image => Put_Image;
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type);
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type);
   function "+" (Name : Unbounded_String) return String is (To_String (Name));
   function "+" (Name : String) return Unbounded_String is
     (To_Unbounded_String (Name));

   -- --------------------------------------------------------------------------
   type Scenario_Type is record
      Name                  : Unbounded_String;
      Location              : Location_Type; -- record only the keyword line
      Comment               : Text;
      Step_List             : aliased Step_Lists.Vector;
      Parent_Feature        : access Feature_Type;
      Parent_Document       : access Document_Type;
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
   -- with Type_Invariant => Parent_Feature /= null xor Parent_Document /= null;
   function Parent_Doc (Scen : Scenario_Type) return access Document_Type;
   function Is_In_Feature (Scen : Scenario_Type) return Boolean;
   use type Step_Lists.Cursor;
   function Has_Cmd_List (Scen : Scenario_Type) return Boolean is
     (Scen.Cmd_List_Step_Index /= Step_Lists.No_Element);
   procedure Add_Result  (Success : Boolean; To : in out Scenario_Type);
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type);

   -- --------------------------------------------------------------------------
   type Feature_Type is record
      Name            : Unbounded_String;
      Location        : Location_Type; -- only record the keyword line
      Comment         : Text;
      Scenario_List   : Scenario_Lists.Vector;
      Background      : access Scenario_Type;
      Parent_Document : access Document_Type;
   end record;
   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type);
   function Has_Background (F : Feature_Type) return Boolean is
     (F.Background /= null and then not F.Background.Step_List.Is_Empty);

   -- --------------------------------------------------------------------------
   type Document_Type is record
      Name          : Unbounded_String;
      Location      : Location_Type; -- record only location of the keyword line
      Comment       : Text;
      Scenario_List : Scenario_Lists.Vector;
      Feature_List  : Feature_Lists.Vector;
      Background    : access Scenario_Type;
   end record;
   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type);
   function Has_Background (D : Document_Type) return Boolean is
     (D.Background /= null and then not D.Background.Step_List.Is_Empty);
   function Output_File_Name (D : Document_Type) return String;

   --  function Created_Files_List_File_Name (D : Document_Type) return String
   --    is (To_String (D.Name) & ".created_files");

   -- --------------------------------------------------------------------------
   procedure Put_Text          (The_Text : Text);
   procedure Put_Step          (Step     : Step_Type);
   procedure Put_Scenario      (Scenario : Scenario_Type);
   procedure Put_Feature       (Feature  : Feature_Type);
   procedure Put_Document      (Doc      : Document_Type);
   procedure Put_Document_List (Doc_List : Documents_Lists.Vector);

   -- --------------------------------------------------------------------------
   function Result (Scenario : Scenario_Type) return Test_Result;
   procedure Move_Results (From_Scen, To_Scen : in out Scenario_Type);

   type Test_Results_Count is array (Test_Result) of Natural
     with Default_Component_Value => 0;

   procedure Compute_Overall_Tests_Results;
   function Overall_Results return Test_Results_Count;
   procedure Put_Overall_Results;

end BBT.Documents;
