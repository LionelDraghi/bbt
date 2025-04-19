-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- Author: Lionel Draghi
-- SPDX-License-Identifier: APSL-2.0
-- SPDX-FileCopyrightText: 2024, Lionel Draghi
-- -----------------------------------------------------------------------------

with BBT.Settings;
limited with BBT.Documents.Scenarios;

with Text_Utilities,
     Ada.Containers.Indefinite_Vectors,
     Ada.Directories,
     Ada.Strings.Text_Buffers,
     Ada.Strings.Unbounded;

use Text_Utilities,
    Ada.Strings.Unbounded;

package BBT.Documents.Steps is

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

   package Cmd_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   function Filtered_By_Default return Boolean is (Settings.Selection_Mode);
   -- If in Selection mode, then items are filtered by default, and selected
   -- items should be provided with --select.
   -- If in normal mode, nothing is filtered by default, and filtered items
   -- should be provided with --exclude.

   -- --------------------------------------------------------------------------
   type Step_Type is new Node with private;
   function Inline_Image (Step : Step_Type) return String;
   -- Short_Line_Image returns a single line image with main non null fields.
   --  package Step_Lists is new Ada.Containers.Indefinite_Vectors
   --    (Positive, Step_Type);

   type Step_Strings is new Unbounded_String;
   Null_Step_Strings : constant Step_Strings :=
                         Step_Strings (Null_Unbounded_String);
   function "+" (Name : Step_Strings) return String is (To_String (Name));
   function "+" (Name : String) return Step_Strings is
     (To_Unbounded_String (Name));
   function Step_String
     (S : Step_Type) return access Step_Strings;
   function Location
     (S : Step_Type) return access Location_Type;
   function Parent_Scenario
     (S : Step_Type) return access Scenarios.Scenario_Type;
   function Subject_String
     (S : Step_Type) return access Unbounded_String;
   function Object_String
     (S : Step_Type) return access Unbounded_String;
   function Object_File_Name
     (S : Step_Type) return access Unbounded_String;
   function Cat
     (S : Step_Type) return access Extended_Step_Categories;
   function Action
     (S : Step_Type) return access Actions;
   function Comment
     (S : Step_Type) return access Text;
   function File_Type
     (S : Step_Type) return access Ada.Directories.File_Kind;
   function Executable_File
     (S : Step_Type) return access Boolean;
   function Ignore_Order
     (S : Step_Type) return access Boolean;
   function File_Content
     (S : Step_Type) return access Text;

   -- Mark the Step as filtered
   procedure Unfilter_Parents (S : in out Step_Type);

   type Step_Access is access all Step_Type;
   package Step_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, Step_Access);
   subtype List is Step_Lists.Vector;
   subtype Reference_Type is Step_Lists.Reference_Type;
   function Last (L : in out List) return Step_Access;
   -- return the last added step

   function Create_Step
     (Cat              : Extended_Step_Categories;
      Action           : Actions;
      Step_String      : Step_Strings;
      Location         : IO.Location_Type;
      Comment          : Text;
      Subject_String   : Unbounded_String;
      Object_String    : Unbounded_String;
      Object_File_Name : Unbounded_String;
      File_Type        : Ada.Directories.File_Kind;
      Executable_File  : Boolean;
      Ignore_Order     : Boolean)
     -- Parent_Scenario  : access Scenarios.Scenario_Type)
      return Step_Type;

private
   type Step_Type is new Node with record
      Cat              : Extended_Step_Categories  := Unknown;
      Action           : Actions                   := None;
      Step_String      : Step_Strings              := Null_Step_Strings;
      Subject_String   : Unbounded_String          := Null_Unbounded_String;
      Object_String    : Unbounded_String          := Null_Unbounded_String;
      Object_File_Name : Unbounded_String          := Null_Unbounded_String;
      File_Type        : Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
      Executable_File  : Boolean                   := False;
      Ignore_Order     : Boolean                   := True;
      File_Content     : Text                      := Empty_Text;
      Parent_Scenario  : access Scenarios.Scenario_Type;
   end record with Put_Image => Put_Image;
   procedure Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      S      :        Step_Type);
   -- Put_Image returns the full image

   function Last (L : in out List) return Step_Access is
     (L.Reference (L.Last).Element.all);

   -- The is no "Tree" to filter, Steps are the leaf in the tree
   -- of items that can be filtered.
   procedure Filter_Tree   (S : in out Step_Type) renames Filter;
   procedure Unfilter_Tree (S : in out Step_Type) renames Unfilter;
   function Step_String
     (S : Step_Type) return Step_Strings is (S.Step_String);
   function Subject_String
     (S : Step_Type) return Unbounded_String is (S.Subject_String);
   function Object_String
     (S : Step_Type) return Unbounded_String is (S.Object_String);
   function Object_File_Name
     (S : Step_Type) return Unbounded_String is (S.Object_File_Name);
   function Cat
     (S : Step_Type) return Extended_Step_Categories is (S.Cat);
   function Action
     (S : Step_Type) return Actions is (S.Action);

end BBT.Documents.Steps;
