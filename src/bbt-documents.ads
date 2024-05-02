with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Text_Utilities; use Text_Utilities;

package BBT.Documents is

   --  A simplified subset of Gerkhin AST :
   --  https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast

   -- Keywords (https://cucumber.io/docs/gherkin/reference/) :
   --  Feature
   --  Example (or Scenario)
   --  Given, When, Then, And, But for steps
   --     (that is line starting starting with ('-' '*' or '+')
   --  Background

   -- --------------------------------------------------------------------------
   type Test_Result is (Failed, Empty, Successful);

   type Extended_Step_Categories is (Unknown,
                                     Given_Step, -- Precond, -- Given
                                     When_Step,  -- Action,  -- When
                                     Then_Step); -- Checks); -- Then
   subtype Step_Categories is Extended_Step_Categories range
     Extended_Step_Categories'Succ (Unknown) .. Extended_Step_Categories'Last;

   type Step_Kind is (Unknown,
                      Run_Cmd,
                      Successfully_Run_Cmd,
                      Error_Return_Code,
                      No_Error_Return_Code,
                      Get_Output,
                      Output_Is_String,
                      File_Is_String,
                      Output_Contains_String,
                      File_Contains_String,
                      Output_Is_File,
                      File_Is_File,
                      Output_Contains_File,
                      File_Contains_File,
                      Existing_File,
                      File_Creation);

   -- --------------------------------------------------------------------------
   type Step_Details is record
      Kind            : Step_Kind := Unknown;
      Text            : Unbounded_String := Null_Unbounded_String;
      Cat             : Extended_Step_Categories;
      Cmd             : Unbounded_String := Null_Unbounded_String;
      Expected_Output : Unbounded_String := Null_Unbounded_String;
      File_Name       : Unbounded_String := Null_Unbounded_String;
   end record;

   -- --------------------------------------------------------------------------
   type Step_Type is record
      Text         : Unbounded_String;
      File_Content : Texts.Vector;
      Details      : Step_Details;
      Category     : Extended_Step_Categories := Unknown;
   end record;
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type);

   -- --------------------------------------------------------------------------
   type Scenario_Type is record
      Name      : Unbounded_String;
      Comment   : Texts.Vector;
      Step_List : Step_Lists.Vector;
      -----------------------
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
   end record;
   procedure Add_Fail   (To : in out Scenario_Type);
   procedure Add_Success (To : in out Scenario_Type);
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type);

   -- --------------------------------------------------------------------------
   type Feature_Type is record
      Name          : Unbounded_String;
      Comment       : Texts.Vector;
      Scenario_List : Scenario_Lists.Vector;
   end record;
   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type);

   -- --------------------------------------------------------------------------
   type Document_Type is record
      Name         : Unbounded_String;
      Comment      : Texts.Vector;
      Feature_List : Feature_Lists.Vector;
   end record;
   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type);

   -- --------------------------------------------------------------------------
   procedure Put_Text (Text : Texts.Vector);

   procedure Put_Step (Step               : Step_Type;
                       With_Comments      : Boolean := False;
                       With_Bold_Keywords : Boolean := False);
   procedure Put_Scenario (Scenario           : Scenario_Type;
                           With_Comments      : Boolean := False;
                           With_Bold_Keywords : Boolean := False);
   procedure Put_Feature (Feature            : Feature_Type;
                          With_Comments      : Boolean := False;
                          With_Bold_Keywords : Boolean := False);
   procedure Put_Document (Doc                : Document_Type;
                           With_Comments      : Boolean := False;
                           With_Bold_Keywords : Boolean := False);
   procedure Put_Document_List (Doc_List           : Documents_Lists.Vector;
                                With_Comments      : Boolean;
                                With_Bold_Keywords : Boolean);

   -- --------------------------------------------------------------------------
   function Result (Scenario : Scenario_Type) return Test_Result;
   procedure Put_Run_Summary;


end BBT.Documents;
