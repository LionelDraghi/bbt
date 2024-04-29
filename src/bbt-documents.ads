with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
   package Texts is new Ada.Containers.Indefinite_Vectors (Positive,
                                                           String);
   subtype String_Vector is Texts.Vector;
   Empty_Text : constant String_Vector := Texts.Empty_Vector;

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
                      Error_Return_Code,
                      No_Error_Return_Code,
                      Std_Output);

   -- --------------------------------------------------------------------------
   type Step_Details (Kind : Step_Kind := Unknown) is record
      Text : Unbounded_String;
      Cat : Extended_Step_Categories;
      case Kind is
         when Run_Cmd =>
            Cmd : Unbounded_String;
         when Error_Return_Code | No_Error_Return_Code =>
            null;
         when Std_Output =>
            Expected_Output : Unbounded_String;
         when Unknown => null;
      end case;
   end record;

   -- --------------------------------------------------------------------------
   type Step_Type is record
      Text         : Unbounded_String;
      File_Content : String_Vector;
      Details      : Step_Details;
      Category     : Extended_Step_Categories := Unknown;
   end record;
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Type);

   -- --------------------------------------------------------------------------
   type Scenario_Type is record
      Name      : Unbounded_String;
      Comment   : String_Vector;
      Step_List : Step_Lists.Vector;
      -----------------------
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
   end record;
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type);
--   function Run_Result (Scenario : Scenario_Type) return Test_Result;

   -- --------------------------------------------------------------------------
   type Feature_Type is record
      Name          : Unbounded_String;
      Comment       : String_Vector;
      Scenario_List : Scenario_Lists.Vector;
   end record;
   package Feature_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Feature_Type);
   -- function Run_Result (Scenario : Feature_Type) return Test_Result;

   -- --------------------------------------------------------------------------
   type Document_Type is record
      Name         : Unbounded_String;
      Comment      : String_Vector;
      Feature_List : Feature_Lists.Vector;
   end record;
   package Documents_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Document_Type);
   -- function Run_Result (Scenario : Document_Type) return Test_Result;

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
   procedure Put_Run_Summary;


end BBT.Documents;
