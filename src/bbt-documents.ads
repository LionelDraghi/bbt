with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Text_Utilities; use Text_Utilities;

-- Defines the main bbt internal data structure, wich is essentialy a tree of
-- Documents containing Features containing scenarii containing steps.
-- This structure is a a simplified subset of Gerkhin AST :
-- https://github.com/cucumber/gherkin?tab=readme-ov-file#abstract-syntax-tree-ast
--
-- The various type of step are also defined here, and this is bbt's own
-- vocabulary.

package BBT.Documents is

   -- Keywords (https://cucumber.io/docs/gherkin/reference/) :
   --  Feature
   --  Example (or Scenario)
   --  Given, When, Then, And, But for steps
   --     (that is line starting starting with ('-' '*' or '+')
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

   type Step_Kind is
     (Unknown,
      Run_Cmd,                -- when I run `cmd`
      Successfully_Run_Cmd,   -- when i successfully run `cmd`
      --                      --------------------------------------------------
      Error_Return_Code,      -- then I get error
      No_Error_Return_Code,   -- then I get no error
      Output_Is,              -- then output is `msg`
      --                      or then output is
      --                           followed by code fenced content
      --                      or then I get     `msg`
      --                      or then I get
      --                           followed by code fenced content
      Output_Contains,        -- then output contains `msg`
      --                      or then output contains
      --                           followed by code fenced content
      File_Is,                -- Then `config.ini` is
      --                           followed by code fenced content
      --                      or then `config.ini` is `mode=silent`
      File_Contains,          -- Then `config.ini` contains
      --                           followed by code fenced content
      --                      or Then `config.ini` contains `--version`
      --                      --------------------------------------------------
      No_File,                -- Given there is no `config.ini` file
      Existing_File,          -- Given then existing `config.ini` file
      File_Creation);         -- Given the file `config.ini`
                              -- followed by code fenced content

   -- --------------------------------------------------------------------------
   type Step_Details is record
      Kind            : Step_Kind := Unknown;
      Text            : Unbounded_String := Null_Unbounded_String;
      Cat             : Extended_Step_Categories;
      Cmd             : Unbounded_String := Null_Unbounded_String;
      Expected_Output : Unbounded_String := Null_Unbounded_String;
      File_Name       : Unbounded_String := Null_Unbounded_String;
   end record;
   Empty_Step_Details : constant Step_Details;

   -- --------------------------------------------------------------------------
   type Step_Type is record
      Step_String  : Unbounded_String;
      File_Content : Text;
      Details      : Step_Details;
      Category     : Extended_Step_Categories := Unknown;
   end record;
   Empty_Step : constant Step_Type;
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type);

   -- --------------------------------------------------------------------------
   type Scenario_Type is record
      Name      : Unbounded_String;
      Comment   : Text;
      Step_List : Step_Lists.Vector;
      -----------------------
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
   end record;
   Empty_Scenario : constant Scenario_Type;
   procedure Add_Fail   (To : in out Scenario_Type);
   procedure Add_Success (To : in out Scenario_Type);
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type);

   -- --------------------------------------------------------------------------
   type Feature_Type is record
      Name          : Unbounded_String;
      Comment       : Text;
      Scenario_List : Scenario_Lists.Vector;
      Background    : Scenario_Type;
   end record;
   Empty_Feature : constant Feature_Type;
   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type);

   -- --------------------------------------------------------------------------
   type Document_Type is record
      Name         : Unbounded_String;
      Comment      : Text;
      Feature_List : Feature_Lists.Vector;
      Background   : Scenario_Type;
   end record;
   Empty_Document : constant Document_Type;
   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type);

   -- --------------------------------------------------------------------------
   procedure Put_Text (The_Text : Text);

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

private
   Empty_Step_Details : constant Step_Details
     := (Kind            => Unknown,
         Text            => Null_Unbounded_String,
         Cat             => Unknown,
         Cmd             => Null_Unbounded_String,
         Expected_Output => Null_Unbounded_String,
         File_Name       => Null_Unbounded_String);
   Empty_Step : constant Step_Type
     := (Step_String  => Null_Unbounded_String,
         File_Content => Empty_Text,
         Details      => Empty_Step_Details,
         Category     => Unknown);
   Empty_Scenario : constant Scenario_Type
     := (Name                  => Null_Unbounded_String,
         Step_List             => Step_Lists.Empty,
         Comment               => Empty_Text,
         Failed_Step_Count     |
         Successful_Step_Count => 0);
   Empty_Feature : constant Feature_Type
     :=  (Name          => Null_Unbounded_String,
          Scenario_List => Scenario_Lists.Empty,
          Comment       => Empty_Text,
          Background    => Empty_Scenario);
   Empty_Document : constant Document_Type
     := (Name         => Null_Unbounded_String,
         Comment      => Empty_Text,
         Feature_List => Feature_Lists.Empty,
         Background   => Empty_Scenario);

end BBT.Documents;
