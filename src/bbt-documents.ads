with BBT.IO;             use BBT.IO;
with Text_Utilities;        use Text_Utilities;

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
      Run_Cmd,
      Run_Without_Error,
      --                      --------------------------------------------------
      Error_Return_Code,
      No_Error_Return_Code,
      Output_Is,
      Output_Contains,
      File_Is,
      File_Contains,
      --                      --------------------------------------------------
      Check_No_File,
      Check_File_Existence,
      Create_If_None,
      Create_New
     );
   -- NB : file is intended here in the broader sens, that is ordinary
   --      file or directory.

   -- --------------------------------------------------------------------------
   type Step_Type is record
      Cat             : Extended_Step_Categories  := Unknown;
      Action          : Actions                   := None;
      Step_String     : Unbounded_String          := Null_Unbounded_String;
      Location        : Location_Type;
      Subject_String  : Unbounded_String          := Null_Unbounded_String;
      Object_String   : Unbounded_String          := Null_Unbounded_String;
      File_Type       : Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
      File_Content    : Text                      := Empty_Text;
   end record with Put_Image => Put_Image;
   Empty_Step : constant Step_Type;
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
      Name      : Unbounded_String;
      Location  : Location_Type; -- record only location of the keyword line
      Comment   : Text;
      Step_List : Step_Lists.Vector;
      -----------------------
      Failed_Step_Count     : Natural := 0;
      Successful_Step_Count : Natural := 0;
   end record;
   Empty_Scenario : constant Scenario_Type;
   --  procedure Add_Fail   (To : in out Scenario_Type);
   --  procedure Add_Success (To : in out Scenario_Type);
   procedure Add_Result  (Success : Boolean; To : in out Scenario_Type);
   package Scenario_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Scenario_Type);

   -- --------------------------------------------------------------------------
   type Feature_Type is record
      Name          : Unbounded_String;
      Location      : Location_Type; -- record only location of the keyword line
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
      Location     : Location_Type; -- record only location of the keyword line
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
                       With_Comments      : Boolean;
                       With_Bold_Keywords : Boolean);
   procedure Put_Scenario (Scenario           : Scenario_Type;
                           With_Comments      : Boolean;
                           With_Bold_Keywords : Boolean);
   procedure Put_Feature (Feature            : Feature_Type;
                          With_Comments      : Boolean;
                          With_Bold_Keywords : Boolean);
   procedure Put_Document (Doc                : Document_Type;
                           With_Comments      : Boolean;
                           With_Bold_Keywords : Boolean);
   procedure Put_Document_List (Doc_List           : Documents_Lists.Vector;
                                With_Comments      : Boolean;
                                With_Bold_Keywords : Boolean);

   -- --------------------------------------------------------------------------
   function Result (Scenario : Scenario_Type) return Test_Result;
   procedure Put_Run_Summary;

private
   Empty_Step : constant Step_Type
     := (Step_String    => Null_Unbounded_String,
         File_Content   => Empty_Text,
         Location       => <>,
         Action         => None,
         Cat            => Unknown,
         Subject_String => Null_Unbounded_String,
         Object_String  => Null_Unbounded_String,
         File_Type      => Ada.Directories.Ordinary_File);

   Empty_Scenario : constant Scenario_Type
     := (Name                  => Null_Unbounded_String,
         Location              => <>,
         Step_List             => Step_Lists.Empty,
         Comment               => Empty_Text,
         Failed_Step_Count     |
         Successful_Step_Count => 0);

   Empty_Feature  : constant Feature_Type
     :=  (Name          => Null_Unbounded_String,
          Location      => <>,
          Scenario_List => Scenario_Lists.Empty,
          Comment       => Empty_Text,
          Background    => Empty_Scenario);

   Empty_Document : constant Document_Type
     := (Name         => Null_Unbounded_String,
         Location     => <>,
         Comment      => Empty_Text,
         Feature_List => Feature_Lists.Empty,
         Background   => Empty_Scenario);

end BBT.Documents;
